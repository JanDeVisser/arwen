/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <map>
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

    void run(std::vector<Value> const &args = {});
};

class Scope {
public:
    Machine                          &machine;
    Scope                            *parent = nullptr;
    Function const                   &function;
    std::map<std::string_view, Value> variables;
    std::vector<Value>                stack;
    bool                              log { false };

    Scope(Machine &machine, Function &function, std::vector<Value> const &args = {});
    Scope(Scope &parent, Function const &function, std::vector<Value> const &args = {});

    void  execute();
    void  push(Value value);
    Value pop();

private:
    void dump_stack();
    void initialize(std::vector<Value> args);
};

}
