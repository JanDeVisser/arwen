/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctime>
#include <mutex>

#include <Util/Logging.h>
#include <Util/Options.h>

namespace Util {

std::mutex g_logging_mutex;

char const* LogLevel_name(LogLevel level)
{
    switch (level) {
#undef S
#define S(level, cardinal) \
    case LogLevel::level:                      \
        return #level;
        ENUMERATE_LOG_LEVELS(S)
#undef S
    default:
        fatal("Unreachable {}", "LogLevel_name");
    }
}

std::optional<LogLevel> LogLevel_by_name(std::string_view const& name)
{
#undef S
#define S(level, cardinal) \
    if (name == #level) {                      \
        return LogLevel::level;                \
    }
    ENUMERATE_LOG_LEVELS(S)
#undef S
    return {};
}

}
