/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <thread>

#include <Util/Pipe.h>

namespace Util {

constexpr static int PipeEndRead = 0;
constexpr static int PipeEndWrite = 1;
constexpr static int DRAIN_SIZE = (64 * 1024);

ReadPipe::ReadPipe(OnRead on_read)
    : m_on_read(std::move(on_read))
{
}

ReadPipe::~ReadPipe()
{
    close();
}

CError ReadPipe::initialize()
{
    if (pipe(m_pipe) == -1) {
        return make_error();
    }
    return {};
}

CError ReadPipe::initialize(std::optional<OnRead> on_read)
{
    m_on_read = std::move(on_read);
    return initialize();
}

CError ReadPipe::connect(int fd)
{
    m_fd = fd;
    if (fcntl(m_fd, F_SETFL, O_NONBLOCK) < 0) {
        return make_error();
    }

    std::thread thr = std::thread(&ReadPipe::read, this);
    thr.detach();
    return {};
}

void ReadPipe::connect_parent()
{
    MUST(connect(m_pipe[PipeEndRead]));
    ::close(m_pipe[PipeEndWrite]);
}

void ReadPipe::connect_child(int fd) const
{
    while ((dup2(m_pipe[PipeEndWrite], fd) == -1) && (errno == EINTR)) { }
    ::close(m_pipe[PipeEndRead]);
    ::close(m_pipe[PipeEndWrite]);
}

void ReadPipe::close()
{
    m_condition.notify_all();
    if (m_fd >= 0) {
        ::close(m_fd);
    }
    m_fd = -1;
}

bool ReadPipe::expect()
{
    std::unique_lock lk(m_mutex);
    m_condition.wait(lk, [this]() {
        return !m_current.empty() || m_fd < 0;
    });
    if (m_fd < 0) {
        return false;
    }
    return true;
}

std::string ReadPipe::current()
{
    std::string ret;
    {
        std::unique_lock lk(m_mutex);
        Defer            sg { [&lk]() {
            lk.unlock();
        } };
        if (m_fd >= 0 && m_current.empty()) {
            m_condition.wait(lk, [this]() {
                return !m_current.empty() || m_fd < 0;
            });
        }
        if (m_fd < 0) {
            return {};
        }
        ret = std::move(m_current);
        m_current = "";
    }
    return ret;
}

void ReadPipe::read()
{
    struct pollfd poll_fd = { 0 };
    poll_fd.fd = m_fd;
    poll_fd.events = POLLIN;

    while (true) {
        if (poll(&poll_fd, 1, -1) == -1) {
            if (errno == EINTR) {
                continue;
            }
            break;
        }
        if (poll_fd.revents & POLLIN) {
            MUST(drain());
        }
        if (poll_fd.revents & POLLHUP) {
            break;
        }
    }
    close();
}

CError ReadPipe::drain()
{
    {
        std::unique_lock lk(m_mutex);
        Defer            guard { [&lk]() {
            lk.unlock();
        } };
        char buffer[DRAIN_SIZE];
        while (true) {
            ssize_t count = ::read(m_fd, buffer, sizeof(buffer) - 1);
            if (count >= 0) {
                buffer[count] = 0;
                if (count > 0) {
                    m_current.append(buffer, count);
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
    if (m_on_read) {
        (*m_on_read)(*this);
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
