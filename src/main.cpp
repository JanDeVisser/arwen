//
// Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
//
// SPDX-License-Identifier: MIT
//

#include <iostream>

#include <Grammar/Parser.h>
#include <GrammarParser/GrammarParser.h>
#include <AST/AST.h>

namespace Arwen {

#include <arwen.grammar>

auto expr_test = R"(func main() {
  println("Hello, World!");
  println(42);
  println(3.14);
  println(true);
  println(34 + 35);
  println(-34 + 35);
}
)";

auto if_test = R"(func main() {
  if (foo(x)) {
    println("ok");
  }
}
)";

auto if_else_test = R"(func main() {
  if (foo(32)) {
    println("ok");
  } else {
    println("not ok");
  }
}
)";

auto loop_test = R"(func main() {
  #blk loop {
    println("ok");
  }
}
)";

auto func_test = R"(func foo(x: int) int {
  println(x);
  return x + 1;
}
)";

auto assign_test = R"(func foo(x: int) {
  bar = 34 + 39;
}
)";

auto assign_assign_test = R"(func foo(x: int) {
  bar = quux = 34 + 39;
}
)";

auto var_decl_test = R"(func foo(x: int) {
  var foo;
  var bar = 34 + 39;
  const quux = 34 + 39;
}
)";

std::array<std::string_view, 8> tests = {
    expr_test,
    if_test,
    if_else_test,
    loop_test,

    func_test,
    assign_test,
    assign_assign_test,
    var_decl_test,
};

#undef TEST_GRAMMARPARSER
#define ARWEN

void arwen_main(int argc, char **argv) {
#ifdef ARWEN
    GrammarParser gp { arwen_grammar };
    Grammar grammar {};
    gp.parse(grammar).must();
    grammar.dump();
    Parser<ArwenParser> p { grammar };
    p.log = true;

    for (auto t : tests) {
        p.parse(t).must();
    }
#endif
#ifdef TEST_GRAMMARPARSER
    tests_GrammarParser();
#endif
}

}

int main(int argc, char **argv) {
    Arwen::arwen_main(argc, argv);
    return 0;
}
