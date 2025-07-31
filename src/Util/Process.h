/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <expected>
#include <print>
#include <sys/wait.h>

#include <Util/Error.h>
#include <Util/Pipe.h>
#include <Util/StringUtil.h>

extern "C" {
void sigchld(int);
}

namespace Util {

using ProcessResult = std::expected<int, LibCError>;

class Process {
public:
    using OnRead = typename ReadPipes::OnRead;

    Process(std::string_view const &cmd) noexcept;
    Process(std::string_view const &cmd, StringList args) noexcept;
    ~Process() = default;

    template<typename... Args>
    Process(std::string_view const &cmd, Args &&...args)
        : Process(cmd)
    {
        StringList cmd_args = {};
        set_arguments(cmd_args, std::forward<Args>(args)...);
    }

    pid_t         pid() const;
    Process      &on_stdout_read(OnRead const &on_read);
    Process      &on_stderr_read(OnRead const &on_read);
    std::string   stdout() { return std::move(m_out_pipes.current<ReadPipes::StdOut>()); }
    std::string   stderr() { return std::move(m_out_pipes.current<ReadPipes::StdErr>()); }
    CError        start();
    CError        background();
    ProcessResult wait();
    ProcessResult execute();
    CError        write_to(std::string_view const &msg);

    template<typename... Args>
    static ProcessResult execute(std::string_view cmd, Args &&...args)
    {
        Process p { cmd, std::forward<Args>(args)... };
        return p.execute();
    }

private:
    void set_arguments(StringList const &cmd_args);

    template<typename... Args>
    void set_arguments(StringList &cmd_args, std::string_view const &arg, Args &&...args)
    {
        cmd_args.emplace_back(arg);
        set_arguments(cmd_args, std::forward<Args>(args)...);
    }

    pid_t                    m_pid { -1 };
    std::string              m_command;
    std::vector<std::string> m_arguments;
    WritePipe                m_in {};
    ReadPipes                m_out_pipes {};
    std::optional<OnRead>    m_on_stdout_read {};
    std::optional<OnRead>    m_on_stderr_read {};
};
}
