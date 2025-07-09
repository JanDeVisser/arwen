/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <dlfcn.h>
#include <filesystem>
#include <functional>
#include <mutex>

#include <Util/Logging.h>
#include <Util/Resolve.h>
#include <Util/StringUtil.h>

namespace Util {

namespace fs = std::filesystem;

std::mutex g_resolve_mutex;

/* ------------------------------------------------------------------------ */

DLError::DLError(char const *m)
    : message((m) ? m : "")
{
}

DLError::DLError(std::string m)
    : message(std::move(m))
{
}

Resolver::Library::Library(std::string img)
    : m_image(std::move(img))
{
    auto result = open();
    if (result.has_value()) {
        m_handle = result.value();
    } else {
        m_my_result = result.error();
    }
}

Resolver::Library::~Library()
{
    if (m_handle) {
        dlclose(m_handle);
    }
}

Result<LibHandle, DLError> Resolver::Library::result() const
{
    if (is_valid()) {
        return m_handle;
    }
    return m_my_result;
}

std::string Resolver::Library::to_string()
{
    return (!m_image.empty()) ? m_image : "Main Program Image";
}

fs::path Resolver::Library::platform_image(std::string const &image)
{
    if (image.empty()) {
        return "";
    }
    fs::path platform_image { image };
#ifdef __APPLE__
    platform_image.replace_extension("dylib");
#else
    platform_image.replace_extension("so");
#endif
    return platform_image;
}

Result<LibHandle, DLError> Resolver::Library::try_open(fs::path const &dir) const
{
    char const *p { nullptr };
    std::string path_string;
    if (!m_image.empty()) {
        fs::path const path { dir / platform_image(m_image) };
        trace("Attempting to open library '{}'", path.string());
        path_string = path.string();
        p = path_string.c_str();
    } else {
        trace("Attempting to open main program module");
    }
    dlerror();
    if (auto const lib_handle = dlopen(p, RTLD_NOW | RTLD_GLOBAL); lib_handle) {
        dlerror();
        trace("Successfully opened '{}'", (p) ? p : "main program module");
        return LibHandle { lib_handle };
    }
    return DLError {};
}

Result<LibHandle, DLError> Resolver::Library::open()
{
    auto image = platform_image(m_image);
    if (!image.empty()) {
        trace("resolve_open('{}') ~ '{}'", m_image, image.string());
    } else {
        trace("resolve_open('Main Program Image')");
    }
    Result<LibHandle, DLError> ret = DLError {};
    m_handle = nullptr;
    if (!m_image.empty()) {
        fs::path arw_dir { getenv("ARW_DIR") ? getenv("ARW_DIR") : ARWEN_DIR };
        if (arw_dir.empty()) {
            arw_dir = "/usr/share/arwen";
        }
        ret = try_open(arw_dir / "lib");
        if (ret.is_error()) {
            ret = try_open(arw_dir / "bin");
        }
        if (ret.is_error()) {
            ret = try_open(arw_dir);
        }
        if (ret.is_error()) {
            ret = try_open(arw_dir / "share/lib");
        }
        if (ret.is_error()) {
            ret = try_open(fs::path { "lib" });
        }
        if (ret.is_error()) {
            ret = try_open(fs::path { "bin" });
        }
        if (ret.is_error()) {
            ret = try_open(fs::path { "share/lib" });
        }
        if (ret.is_error()) {
            ret = try_open(fs::current_path());
        }
    } else {
        ret = try_open("");
    }
    if (ret.has_value()) {
        m_handle = ret.value();
        if (!image.empty()) {
            auto result = get_function(ARW_INIT);
            if (result.has_value()) {
                if (auto func_ptr = result.value(); func_ptr != nullptr) {
                    trace("resolve_open('{}') Executing initializer", to_string());
                    (func_ptr)();
                } else {
                    trace("resolve_open('{}') No initializer", to_string());
                }
            } else {
                log_error("resolve_open('{}') Error finding initializer: {}",
                    to_string(), result.error().message);
                m_my_result = result.error();
                return result.error();
            }
        }
        trace("Library '{}' opened successfully", to_string());
    } else {
        log_error("Resolver::Library::open('{}') FAILED", to_string());
        m_my_result = ret.error();
    }
    return ret;
}

Result<void_t, DLError> Resolver::Library::get_function(std::string const &function_name)
{
    if (!m_my_result.message.empty()) {
        return m_my_result;
    }
    if (m_functions.contains(function_name)) {
        return m_functions[function_name];
    }
    dlerror();
    trace("dlsym('{}', '{}')", to_string(), function_name);
    if (auto const function = dlsym(m_handle.handle, function_name.c_str()); function != nullptr) {
        auto const func_ptr = reinterpret_cast<void_t>(function);
        m_functions[function_name] = func_ptr;
        return func_ptr;
    }
// 'undefined symbol' is returned with an empty result pointer
#ifdef __APPLE__
    if (std::string_view err { dlerror() }; err.find("symbol not found") == std::string::npos) {
#else
    if (std::string_view err { dlerror() }; err.find("undefined symbol") == std::string::npos) {
#endif
        return DLError { std::string { err } };
    }
    m_functions[function_name] = nullptr;
    return nullptr;
}

/* ------------------------------------------------------------------------ */

Result<LibHandle, DLError> Resolver::open(std::string const &image)
{
    auto const platform_image = Library::platform_image(image);
    if (!m_images.contains(platform_image)) {
        if (auto const lib = std::make_shared<Library>(image); lib->is_valid()) {
            m_images[platform_image] = lib;
        } else {
            return lib->result();
        }
    }
    return m_images[platform_image]->result();
}

Result<void_t, DLError> Resolver::resolve(std::string const &func_name)
{
    std::lock_guard<std::mutex> const lock(g_resolve_mutex);
    auto                              s = func_name;
    if (auto const paren = s.find_first_of('('); paren != std::string::npos) {
        s.erase(paren);
        while (s[s.length() - 1] == ' ')
            s.erase(s.length() - 1);
    }
    // LOLWUT?
    // if (auto const space = s.find_first_of(' '); space != std::string::npos) {
    //     s.erase(0, space);
    //     while (s[0] == ' ')
    //         s.erase(0, 1);
    // }
    while (isblank(s[0])) {
        s.erase(0, 1);
    }

    std::string image;
    std::string function;
    auto        name = split(std::string_view { s }, ':');
    switch (name.size()) {
    case 2:
        image = name.front();
        /* fall through */
    case 1:
        function = name.back();
        break;
    default:
        return DLError { std::format("Invalid function reference '{}'", func_name).c_str() };
    }

    if (auto result = open(image); result.is_error()) {
        return result.error();
    }
    auto lib = m_images[Library::platform_image(image)];
    if (auto res = lib->get_function(function); res.has_value()) {
        return res.value();
    } else {
        return res.error();
    }
}

Resolver &Resolver::get_resolver() noexcept
{
    static Resolver                  *resolver = nullptr;
    std::lock_guard<std::mutex> const lock(g_resolve_mutex);
    if (!resolver) {
        resolver = new Resolver();
    }
    return *resolver;
}

}
