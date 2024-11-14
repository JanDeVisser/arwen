/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cerrno>
#include <fcntl.h>
#include <filesystem>
#include <memory>
#include <sys/stat.h>
#include <unistd.h>

#include <Result.h>
#include <ScopeGuard.h>

namespace Arwen {

namespace fs = std::filesystem;

struct SimpleBufferLocator {
public:
    SimpleBufferLocator() = default;
    [[nodiscard]] Result<fs::path> locate(std::string_view) const;
    [[nodiscard]] static Error<>   check_existence(fs::path const &file_name);
};

class FileBuffer {
public:
    FileBuffer(fs::path, char const *);

    FileBuffer(FileBuffer const&other) = default;

    FileBuffer(FileBuffer &&other) noexcept
        : m_path(std::move(other.m_path))
        , m_contents(other.m_contents)
    {
        other.m_contents = nullptr;
    }

    ~FileBuffer();

    template<typename Locator=SimpleBufferLocator>
    static Result<FileBuffer> from_file(std::string_view file_name, Locator const &locator = Locator {})
    {
        auto full_file_name = TRY_EVAL(locator.locate(file_name));
        auto fh = ::open(full_file_name.c_str(), O_RDONLY);
        auto file_closer = ScopeGuard([fh]() {
            if (fh > 0)
                close(fh);
        });
        if (fh < 0) {
            return LibCError {};
        }
        struct stat sb {};
        if (auto rc = fstat(fh, &sb); rc < 0)
            return LibCError {};
        if (S_ISDIR(sb.st_mode))
            return LibCError(EISDIR);

        auto size = sb.st_size;
        auto buffer = new char[size + 1];
        if (auto rc = ::read(fh, (void *) buffer, size); rc < size) {
            return LibCError {};
        }
        buffer[size] = '\0';
        return FileBuffer { full_file_name, buffer };
    }

    [[nodiscard]] std::string_view contents() const;
    [[nodiscard]] fs::path const &file_path() const { return m_path; }

private:
    fs::path    m_path;
    char const *m_contents;
};

}
