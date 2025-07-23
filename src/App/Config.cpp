/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <filesystem>

#include <config.h>

#include <Util/Logging.h>

#include <App/Config.h>

namespace Arwen {

using namespace Util;
namespace fs = std::filesystem;

fs::path arwen_dir()
{
    fs::path arw_dir { getenv("ARW_DIR") ? getenv("ARW_DIR") : ARWEN_DIR };
    if (arw_dir.empty()) {
        arw_dir = "/usr/share/arwen";
    }
    auto std_arw { arw_dir / "share" / "std.arw" };
    if (!fs::exists(std_arw)) {
        fatal("{} not found", std_arw.string());
    }
    return arw_dir;
}

}
