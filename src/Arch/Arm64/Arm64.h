/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <expected>
#include <string>

#include <App/IR/IR.h>

namespace Arwen::Arm64 {

using namespace Util;
using namespace Arwen;

enum class ARM64ErrorCode {
    InternalError,
    IOError,
};

struct ARM64Error {
    ARM64ErrorCode code;
    std::string    message;

    std::string const &to_string() const
    {
        return message;
    }
};

std::expected<void, ARM64Error> generate_arm64(IR::pProgram program);

}
