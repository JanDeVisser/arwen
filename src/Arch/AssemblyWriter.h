/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <format>
#include <string>

namespace Arwen {

struct AssemblyWriter {
    std::wstring  prolog;
    std::wstring  code;
    std::wstring  epilog;
    std::wstring *active { &code };

    template<typename... Args>
    void add_instruction(std::wstring_view const mnemonic, std::wstring_view param_fmt, Args &&...args)
    {
        auto const fmt { std::format(L"\t{{}}\t{}\n", param_fmt) };
        *active += std::vformat(fmt, std::make_wformat_args(mnemonic, args...));
    }

    void               add_instruction(std::wstring_view const mnemonic, std::wstring_view const param);
    void               add_instruction(std::wstring_view const mnemonic);
    void               add_text(std::wstring_view const &text);
    void               add_label(std::wstring_view const label);
    void               add_directive(std::wstring_view const directive, std::wstring_view const args);
    void               add_comment(std::wstring_view const comment);
    [[nodiscard]] bool empty() const;
    [[nodiscard]] bool has_text() const;
    void               activate_prolog();
    void               activate_code();
    void               activate_epilog();
};
}
