/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <format>
#include <string>

#include <Util/StringUtil.h>

#include <Arch/AssemblyWriter.h>

namespace Arwen {

using namespace Util;

void AssemblyWriter::add_instruction(std::wstring_view const mnemonic, std::wstring_view const param)
{
    *active += std::format(L"\t{}\t{}\n", mnemonic, param);
}

void AssemblyWriter::add_instruction(std::wstring_view const mnemonic)
{
    *active += std::format(L"\t{}\n", mnemonic);
}

void AssemblyWriter::add_text(std::wstring_view const &text)
{
    if (text.empty()) {
        return;
    }
    auto t = strip(text);
    for (auto line : split(t, '\n')) {
        if (line.empty()) {
            *active += '\n';
            continue;
        }
        line = strip(line);
        if (line[0] == ';') {
            *active += std::format(L"\t//{}\n", line.substr(1));
            continue;
        }
        if ((line[0] == '.') || line.ends_with(L":")) {
            *active += std::format(L"{}\n", line);
            continue;
        }
        for (auto const &p : split(strip(line), ' ')) {
            if (p.empty()) {
                continue;
            }
            *active += std::format(L"\t{}", p);
        }
        *active += '\n';
    }
}

void AssemblyWriter::add_label(std::wstring_view const label)
{
    *active += std::format(L"\n{}:\n", label);
}

void AssemblyWriter::add_directive(std::wstring_view const directive, std::wstring_view const args)
{
    *active += std::format(L"{}\t{}\n", directive, args);
}

void AssemblyWriter::add_comment(std::wstring_view const comment)
{
    if (comment.empty()) {
        return;
    }
    *active += '\n';
    for (auto const line : split(strip(comment), '\n')) {
        *active += std::format(L"\t// {}\n", strip(line));
    }
}

bool AssemblyWriter::empty() const
{
    return code.empty();
}

bool AssemblyWriter::has_text() const
{
    return !empty();
}

void AssemblyWriter::activate_prolog()
{
    active = &prolog;
}

void AssemblyWriter::activate_code()
{
    active = &code;
}

void AssemblyWriter::activate_epilog()
{
    active = &epilog;
}

}
