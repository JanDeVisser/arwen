/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cerrno>
#include <expected>
#include <format>
#include <string>

#include <config.h>

namespace Util {

struct LibCError {
#ifdef IS_LINUX
    static constexpr int ECUSTOM = EHWPOISON + 1;
#endif
#ifdef IS_APPLE
    static constexpr int ECUSTOM = EQFULL + 1;
#endif

    int              err_no = { 0 };
    std::string_view code = { "Unknown" };
    std::string      description = { "Unknown error" };

    explicit LibCError(int err) noexcept;

    LibCError() noexcept
        : LibCError(errno)
    {
    }

    explicit LibCError(std::string_view const description) noexcept
        : LibCError(ECUSTOM)
    {
        this->description = description;
    }

    template<typename... Args>
    explicit LibCError(std::string_view const fmt, Args const &...args) noexcept
        : LibCError(ECUSTOM)
    {
        description = std::vformat(fmt, std::make_format_args(args...));
    }

    [[nodiscard]] std::string to_string() const
    {
        return std::format("{} ({}): {}", code, err_no, description);
    }
};

using CError = std::expected<void, LibCError>;

inline std::unexpected<LibCError> make_error(int err = errno)
{
    return std::unexpected(LibCError { err });
}

inline std::unexpected<LibCError> make_error(std::string_view const msg)
{
    return std::unexpected(LibCError { msg });
}

template<typename... Args>
std::unexpected<LibCError> make_error(std::string_view const fmt, Args const &...args)
{
    return std::unexpected(LibCError { fmt, args... });
}

template<typename R>
using Result = std::expected<R, LibCError>;

#define TRY(...)                                     \
    do {                                             \
        auto _result = (__VA_ARGS__);                \
        if (!_result.has_value()) {                  \
            return std::unexpected(_result.error()); \
        }                                            \
    } while (0)

#define TRY_EVAL(...)                                \
    ({                                               \
        auto _result = (__VA_ARGS__);                \
        if (!_result.has_value()) {                  \
            return std::unexpected(_result.error()); \
        }                                            \
        _result.value();                             \
    })

#define MUST(...)                                                              \
    do {                                                                       \
        auto _result = (__VA_ARGS__);                                          \
        if (!_result.has_value()) {                                            \
            fatal("MUST {:}: {:}", #__VA_ARGS__, _result.error().to_string()); \
        }                                                                      \
    } while (0)

#define MUST_EVAL(...)                                                              \
    ({                                                                              \
        auto _result = (__VA_ARGS__);                                               \
        if (!_result.has_value()) {                                                 \
            fatal("MUST_EVAL {:}: {:}", #__VA_ARGS__, _result.error().to_string()); \
        }                                                                           \
        _result.value();                                                            \
    })

#define IGNORE(...)                   \
    do {                              \
        auto _result = (__VA_ARGS__); \
    } while (0)

}
