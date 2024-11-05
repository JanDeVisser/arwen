cmake_minimum_required(VERSION 3.29)
project(arwen)

set(CMAKE_CXX_STANDARD 23)

include_directories(src)

add_executable(
        arwen

        src/Lib.h
        src/Logging.cpp
        src/Options.cpp
        src/Resolve.cpp
        src/ScopeGuard.h
        src/Set.h
        src/Unescape.cpp
        src/Value.h

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

        src/AST/Arwen.cpp
        src/AST/Operator.h

        src/main.cpp
)
