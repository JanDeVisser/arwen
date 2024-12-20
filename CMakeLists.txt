cmake_minimum_required(VERSION 3.28)
project(arwen)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_EXPORT_COMPILE_COMMANDS on)

enable_language(ASM-ATT)

include_directories(src)

add_subdirectory(rt)

add_executable(
        arwen

        src/Error.cpp
        src/FileBuffer.cpp
        src/Foreign.cpp
        src/Lib.h
        src/Logging.cpp
        src/Options.cpp
        src/Resolve.cpp
        src/ScopeGuard.h
        src/SimpleFormat.h
        src/Set.h
        src/TaggedUnion.h
        src/Unescape.cpp
        src/trampoline.s

        src/Lexer/Config.cpp
        src/Lexer/Lexer.cpp
        src/Lexer/Token.cpp
        src/Lexer/TokenKind.cpp

        src/Grammar/Grammar.cpp
        src/Grammar/Parser.h
        src/Grammar/Rule.cpp
        src/Grammar/Sequence.cpp
        src/Grammar/Symbol.cpp

        src/GrammarParser/GrammarParser.cpp

        src/Type/Type.cpp
        src/Type/Value.cpp

        src/AST/Arwen.cpp
        src/AST/Operator.h

        src/Binder/Binder.cpp

        src/IR/Intrinsics.cpp
        src/IR/IR.cpp
        src/IR/Machine.cpp
        src/IR/Scope.cpp

        src/main.cpp
)

target_link_libraries(
        arwen
        arwenrt
)
