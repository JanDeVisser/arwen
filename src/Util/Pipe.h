/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <condition_variable>
#include <functional>
#include <string>
#include <string_view>
#include <sys/fcntl.h>
#include <sys/poll.h>
#include <unistd.h>

#include <Util/Defer.h>
#include <Util/Logging.h>

namespace Util {

class ReadPipe {
public:
    using OnRead = std::function<void(ReadPipe &)>;
    ReadPipe() = default;
    ReadPipe(ReadPipe const &) = delete;
    ReadPipe(ReadPipe &&) = delete;
    ReadPipe(OnRead on_read);
    ~ReadPipe();

    CError      initialize();
    CError      initialize(std::optional<OnRead> on_read);
    CError      connect(int fd);
    void        connect_parent();
    void        connect_child(int fd) const;
    void        close();
    bool        expect();
    std::string current();

private:
    void   read();
    CError drain();

    int                     m_pipe[2] { 0, 0 };
    int                     m_fd { -1 };
    std::string             m_current {};
    std::mutex              m_mutex {};
    std::condition_variable m_condition {};
    std::optional<OnRead>   m_on_read {};
    bool                    m_debug { false };
};

class WritePipe {
public:
    WritePipe() = default;
    WritePipe(WritePipe const &) = delete;
    WritePipe(WritePipe &&from) = delete;
    ~WritePipe();

    CError         initialize();
    void           close();
    void           connect_parent();
    void           connect_child(int fd) const;
    Result<size_t> write(std::string_view sv) const;
    Result<size_t> write_chars(char const *buf, size_t num) const;

private:
    int m_pipe[2];
    int m_fd;
};

}
