/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cctype>
#include <cstring>
#include <iostream>

#include <Util/Options.h>

namespace Util {

using Option = std::pair<std::string_view, std::string_view>;
using Options = std::vector<Option>;

static Options s_options = {};

void set_option(std::string_view option, std::string_view value)
{
    //    std::cout << " set_option(" << option << ", " << value << ")";
    s_options.emplace_back(option, value);
}

int parse_options(int argc, char const **argv)
{
    auto ix = 1;
    //    std::cout << "parse_options(" << argc << ")\n";
    for (; ix < argc && strlen(argv[ix]) > 1 && *argv[ix] == '-'; ++ix) {
        //        std::cout << "ix: " << ix << " argv: " << argv[ix];
        std::string_view option = { argv[ix] + 1, 1 };
        std::string_view value = "true";
        if (strlen(argv[ix]) > 2 && *(argv[ix] + 1) == '-') {
            option = argv[ix] + 2;
            char const *equals = strchr(argv[ix] + 2, '=');
            if (equals) {
                option = std::string_view(argv[ix] + 2, equals - argv[ix] - 2);
                value = equals + 1;
            }
        } else if (isalpha(*(argv[ix] + 1))) {
            if (strlen(argv[ix]) > 2) {
                value = argv[ix] + 2;
            }
        } else {
            //            std::cout << "\n";
            continue;
        }
        set_option(option, value);
        //        std::cout << "\n";
    }
    //   std::cout << "arg_ix: " << ix << "\n";
    return ix;
}

std::optional<std::string_view> get_option(std::string_view option)
{
    for (auto opt : s_options) {
        if (opt.first == option) {
            return { opt.second };
        }
    }
    return {};
}

std::vector<std::string_view> get_option_values(std::string_view option)
{
    std::vector<std::string_view> ret = {};
    for (auto opt : s_options) {
        if (opt.first == option) {
            ret.emplace_back(opt.second);
        }
    }
    return ret;
}

bool has_option(std::string_view option)
{
    return get_option(option).has_value();
}

}
