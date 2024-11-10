/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cerrno>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include <FileBuffer.h>
#include <Logging.h>

namespace Arwen {

Error<> SimpleBufferLocator::check_existence(fs::path const &file_name)
{
    auto fh = ::open(file_name.c_str(), O_RDONLY);
    auto sg = ScopeGuard([fh]() { if (fh > 0) ::close(fh); });
    if (fh < 0) {
        return LibCError {};
    }

    struct stat sb {};
    if (auto rc = fstat(fh, &sb); rc < 0) {
        return LibCError {};
    }
    if (S_ISDIR(sb.st_mode)) {
        return LibCError { EFTYPE };
    }
    return {};
}

Result<fs::path> SimpleBufferLocator::locate(std::string_view file_name) const
{
    TRY(check_existence(file_name));
    return file_name;
}

FileBuffer::FileBuffer(fs::path path, char const *text)
    : m_contents(text)
    , m_path(std::move(path))
{
}

FileBuffer::~FileBuffer()
{
    delete m_contents;
}

std::string_view FileBuffer::contents() const
{
    return { m_contents };
}

}
