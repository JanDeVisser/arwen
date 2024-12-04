/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstddef>
#include <map>
#include <optional>
#include <string_view>
#include <vector>

#include <Binder/Binder.h>
#include <IR/IR.h>
#include <Type/Value.h>

namespace Arwen::IR {

class Scope;

struct Machine {
    Program &program;
    bool     log { false };

    std::optional<Value> run(std::vector<Value> const &args = {});
};

class Scope {
public:
    Machine                          &machine;
    Scope                            *parent = nullptr;
    Function const                   &function;
    std::map<std::string_view, Value> variables;
    size_t                            ip;
    std::vector<Value>                stack;
    bool                              log { false };

    Scope(Machine &machine, Function &function, std::vector<Value> const &args = {});
    Scope(Scope &parent, Function const &function, std::vector<Value> const &args = {});

    std::optional<Value>          execute();
    void         jump(size_t target);
    void         push(Value value);
    Value        pop();
    void         set(std::string_view name, Value value);
    Value       &get(std::string_view name);
    Value const &get(std::string_view name) const;

private:
    void dump_stack();
    void initialize(std::vector<Value> args);
};

}
