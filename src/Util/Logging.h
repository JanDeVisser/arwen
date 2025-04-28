/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstring>
#include <format>
#include <iostream>
#include <mutex>
#include <string>
#include <sys/signal.h>

#include <config.h>

namespace Util {

template<class... Ts>
struct overloads : Ts... {
    using Ts::operator()...;
};

#define ENUMERATE_LOG_LEVELS(S) \
    S(None, 0)                  \
    S(Trace, 1)                 \
    S(Info, 2)                  \
    S(Warning, 3)               \
    S(Error, 4)                 \
    S(Fatal, 5)

enum class LogLevel {
#undef S
#define S(level, cardinal) level = cardinal,
    ENUMERATE_LOG_LEVELS(S)
#undef S
};

char const             *LogLevel_name(LogLevel);
std::optional<LogLevel> LogLevel_by_name(std::string_view const &);

template<typename T>
struct LogMessage {
    std::string_view file;
    size_t           line;
    std::string_view function;
    LogLevel         level;
    T const         *message;
};

static constexpr LogLevel m_level = LogLevel::Trace;
extern std::mutex g_logging_mutex;

template<typename T>
void print_message(std::basic_string<T> const &msg)
{
}

template<>
inline void print_message(std::string const &msg)
{
    std::cerr << msg << std::endl;
    std::cerr.flush();
}

template<>
inline void print_message(std::wstring const &msg)
{
    std::wcerr << msg << std::endl;
    std::wcerr.flush();
}

template<typename T, typename... Args>
void logmsg(LogMessage<T> const &msg, Args const &...args)
{
    std::lock_guard<std::mutex> const lock(g_logging_mutex);
    if (msg.level >= m_level) {
        std::string_view f(msg.file);
        if (f.front() == '/') {
            auto ix = f.find_last_of('/');
            if (ix != std::string_view::npos) {
                auto len = f.length() - ix - 1;
                if (len > 19) {
                    len = 19;
                }
                f = f.substr(ix + 1, len);
            }
        }
        auto file_line = std::format("{}:{}", f, msg.line);
        auto prefix = std::format("{:<24}:{:<20}:{:<5}:", file_line, msg.function, LogLevel_name(msg.level));
        auto message = std::vformat(msg.message, std::make_format_args(args...));
        print_message(std::format("{}{}", prefix, message));
    }
}

template<typename T, typename... Args>
void trace_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg<T>({ file, line, function, LogLevel::Trace, message }, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
void info_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg<T>({ file, line, function, LogLevel::Info, message }, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
void warning_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg(LogMessage<char> { file, line, function, LogLevel::Warning, message }, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
void error_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg<T>({ file, line, function, LogLevel::Error, message }, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
__attribute__((noreturn)) void fatal_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args const &...args)
{
    logmsg<T>(LogMessage<T> { file, line, function, LogLevel::Fatal, message }, std::forward<Args const &>(args)...);
    abort();
}

template<typename T, typename... Args>
void assert_msg(std::string_view const &file, size_t line, std::string_view const &function, bool condition, T const *message, Args const &...args)
{
    if (condition)
        return;
    logmsg<T>(LogMessage<T> { file, line, function, LogLevel::Fatal, message }, std::forward<Args const &>(args)...);
    abort();
}

#ifdef assert
#undef assert
#endif

#define trace(fmt, ...) trace_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define info(fmt, ...) info_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define warning(fmt, ...) warning_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define log_error(fmt, ...) error_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define fatal(fmt, ...) fatal_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define assert(condition) assert_msg(__FILE__, __LINE__, __func__, condition, "Assertion error: {}", #condition)
#define assert_with_msg(condition, fmt, ...) assert_msg(__FILE__, __LINE__, __func__, condition, "Assertion error: " #condition ": " fmt __VA_OPT__(, ) __VA_ARGS__)

#define UNREACHABLE() fatal("Unreachable")
#define NYI(msg) fatal("Not yet implemented: " msg)

}
