/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <type_traits>
#include <variant>

#include <Binder/Binder.h>
#include <IR/IR.h>
#include <Result.h>

namespace Arwen::IR {

using namespace Arwen;

Error<bool> Program::generate(Binder &binder)
{
    if (!binder.entrypoint) {
        return false;
    }
    auto const &program_impl = I(BoundProgram, binder.entrypoint);
    for (auto mod_ref : program_impl.modules) {
        auto const &mod = I(BoundModule, mod_ref);
        modules.emplace_back(modules.size(), mod_ref, mod.name);
        modules.back().generate(binder);
    }
    return true;
}

void Module::generate(Binder &binder)
{
    auto const &mod = I(BoundModule, bound_ref);
    for (auto name_ref : mod.names) {
        auto const &name = I(BoundFunction, name_ref);
        auto const &decl = I(BoundFunctionDecl, name.declaration);
        functions.emplace_back(functions.size(), name_ref, mod.name);
        function_refs.emplace(functions.back().name, functions.back().ref);
        functions.back().generate(binder);
    }
}

void Function::generate(Binder &binder)
{
    auto const &name = I(BoundFunction, bound_ref);
    auto const &decl = I(BoundFunctionDecl, name.declaration);

    std::visit(
        [&binder, this](auto const &impl) {
            generate<std::decay_t<decltype(impl)>>(impl, binder, *this);
        },
        binder[name.implementation].impl);
}

}
