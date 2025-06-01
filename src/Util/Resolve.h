/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <filesystem>
#include <map>
#include <memory>
#include <string>

#include <Util/Result.h>

namespace Util {

namespace fs = std::filesystem;

constexpr static auto const *ARW_DIR = "ARW_DIR";
constexpr static auto const *ARW_INIT = "_arw_init";

typedef void (*void_t)();

typedef void *lib_handle_t;

struct LibHandle {
    lib_handle_t handle;

    LibHandle(void *h)
        : handle(h)
    {
    }

    // ReSharper disable once CppNonExplicitConversionOperator
    operator void *() const { return handle; }
};

struct DLError {
    std::string message {};
    DLError() = default;
    DLError(char const *m);
    DLError(std::string m);
};

class Resolver {
public:
    ~Resolver() = default;
    static Resolver           &get_resolver() noexcept;
    Result<LibHandle, DLError> open(std::string const &);
    Result<void_t, DLError>    resolve(std::string const &);

private:
    class Library {
    public:
        explicit Library(std::string img);
        ~Library();
        std::string                              to_string();
        static fs::path                          platform_image(std::string const &);
        [[nodiscard]] bool                       is_valid() const { return m_my_result.message.empty(); }
        [[nodiscard]] Result<LibHandle, DLError> result() const;
        Result<void_t, DLError>                  get_function(std::string const &);

    private:
        Result<LibHandle, DLError>               open();
        [[nodiscard]] Result<LibHandle, DLError> try_open(fs::path const &) const;

        LibHandle                     m_handle { nullptr };
        std::string                   m_image {};
        DLError                       m_my_result;
        std::map<std::string, void_t> m_functions {};
        friend Resolver;
    };

    Resolver() = default;
    std::map<std::string, std::shared_ptr<Library>> m_images;
};

}
