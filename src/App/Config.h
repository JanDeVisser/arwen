/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <filesystem>

namespace Arwen {

namespace fs = std::filesystem;

fs::path arwen_dir();

}
