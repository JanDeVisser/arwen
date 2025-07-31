/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <optional>
#include <sys/fcntl.h>
#include <sys/poll.h>
#include <thread>

#include <Util/Pipe.h>

namespace Util {

constexpr static int PipeEndRead = 0;
constexpr static int PipeEndWrite = 1;
constexpr static int DRAIN_SIZE = (64 * 1024);

ReadPipes::ReadPipes(std::optional<OnRead> on_stdout, std::optional<OnRead> on_stderr)
{
    pipes[StdOut].on_read = std::move(on_stdout);
    pipes[StdErr].on_read = std::move(on_stderr);
}

ReadPipes::~ReadPipes()
{
    close();
}

CError ReadPipes::Pipe::initialize(std::optional<OnRead> on_read_fnc)
{
    on_read = std::move(on_read_fnc);
    if (::pipe(pipe) == -1) {
        return make_error();
    }
    return {};
}

CError ReadPipes::initialize(std::optional<OnRead> on_stdout, std::optional<OnRead> on_stderr)
{
    if (auto res = pipes[StdOut].initialize(on_stdout); !res.has_value()) {
        return std::unexpected(res.error());
    }
    if (auto res = pipes[StdErr].initialize(on_stderr); !res.has_value()) {
        return std::unexpected(res.error());
    }
    return {};
}

CError ReadPipes::Pipe::connect(int fd)
{
    this->fd = fd;
    if (fcntl(fd, F_SETFL, O_NONBLOCK) < 0) {
        return make_error();
    }
    return {};
}

void ReadPipes::Pipe::connect_parent()
{
    MUST(connect(pipe[PipeEndRead]));
    ::close(pipe[PipeEndWrite]);
}

void ReadPipes::connect_parent()
{
    pipes[StdOut].connect_parent();
    pipes[StdErr].connect_parent();
    std::thread thr = std::thread(&ReadPipes::read, this);
    thr.detach();
}

void ReadPipes::Pipe::connect_child(int fd)
{
    while ((dup2(pipe[PipeEndWrite], fd) == -1) && (errno == EINTR)) { }
    this->fd = fd;
    ::close(pipe[PipeEndRead]);
    ::close(pipe[PipeEndWrite]);
}

void ReadPipes::connect_child(int out, int err)
{
    pipes[StdOut].connect_child(out);
    pipes[StdErr].connect_child(err);
}

void ReadPipes::Pipe::close()
{
    if (fd >= 0) {
        ::close(fd);
    }
    fd = -1;
}

void ReadPipes::close()
{
    m_condition.notify_all();
    pipes[StdOut].close();
    pipes[StdErr].close();
}

bool ReadPipes::expect(Channel channel)
{
    std::unique_lock lk(m_mutex);
    auto            &p { pipes[channel] };
    m_condition.wait(lk, [&p]() {
        return !p.current.empty() || p.fd < 0;
    });
    if (p.fd < 0) {
        return false;
    }
    return true;
}

std::string ReadPipes::current(Channel channel)
{
    std::string ret;
    auto       &p { pipes[channel] };
    if (p.fd >= 0 && p.current.empty()) {
        std::unique_lock lk(m_mutex);
        if (p.fd >= 0 && p.current.empty()) {
            m_condition.wait(lk, [&p]() {
                return !p.current.empty() || p.fd < 0;
            });
        }
    }
    ret.swap(p.current);
    return ret;
}

void ReadPipes::read()
{
    struct pollfd poll_fd[2] = { 0 };
    for (auto ix = 0; ix < 2; ++ix) {
        poll_fd[ix].fd = pipes[ix].fd;
        poll_fd[ix].events = POLLIN;
    }

    bool done { false };
    do {
        if (poll(poll_fd, 2, -1) == -1) {
            if (errno == EINTR) {
                continue;
            }
            break;
        }
        for (auto ix = 0; ix < 2; ++ix) {
            if (poll_fd[ix].revents & POLLIN) {
                MUST(drain(ix));
            }
            if (poll_fd[ix].revents & POLLHUP) {
                done = true;
                break;
            }
        }
    } while (!done);
}

CError ReadPipes::drain(Channel channel)
{
    auto &p { pipes[channel] };
    if (p.fd < 0) {
        return {};
    }
    {
        std::unique_lock lk(m_mutex);
        char             buffer[DRAIN_SIZE];
        while (true) {
            ssize_t count = ::read(p.fd, buffer, sizeof(buffer) - 1);
            if (count >= 0) {
                buffer[count] = 0;
                if (count > 0) {
                    p.current.append(buffer, count);
                    if (count == sizeof(buffer) - 1) {
                        continue;
                    }
                }
                break;
            }
            if (errno == EINTR) {
                continue;
            }
            return make_error();
        }
        m_condition.notify_all();
    }
    if (!p.current.empty() && p.on_read) {
        (*p.on_read)(*this);
    }
    return {};
}

WritePipe::~WritePipe()
{
    close();
}

CError WritePipe::initialize()
{
    if (pipe(m_pipe) == -1) {
        return make_error();
    }
    return {};
}

void WritePipe::close()
{
    if (m_fd >= 0) {
        ::close(m_fd);
    }
    m_fd = -1;
}

void WritePipe::connect_parent()
{
    m_fd = m_pipe[PipeEndWrite];
    ::close(m_pipe[PipeEndRead]);
}

void WritePipe::connect_child(int fd) const
{
    while ((dup2(m_pipe[PipeEndRead], fd) == -1) && (errno == EINTR)) { }
    ::close(m_pipe[PipeEndRead]);
    ::close(m_pipe[PipeEndWrite]);
}

Result<size_t> WritePipe::write(std::string_view sv) const
{
    return write_chars(sv.data(), sv.length());
}

Result<size_t> WritePipe::write_chars(char const *buf, size_t num) const
{
    ssize_t total = { 0 };
    while (total < num) {
        ssize_t count = ::write(m_fd, buf + total, num - total);
        if (count < 0) {
            if (errno != EINTR) {
                return make_error();
            }
            continue;
        }
        total += count;
    }
    return { total };
}
}
