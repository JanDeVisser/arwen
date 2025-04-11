cmake_minimum_required(VERSION 3.27)
project(arwen CXX)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

message("System: ${CMAKE_SYSTEM_NAME}. Processor: ${CMAKE_SYSTEM_PROCESSOR}. Version: ${CMAKE_SYSTEM_VERSION}. Compiler: ${CMAKE_CXX_COMPILER_ID}.")

# if (NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
# message(FATAL_ERROR "Arwen requires Clang because it used C++ language extensions not supported in GCC")
# endif()
set(CMAKE_C_STANDARD 23)
set(CMAKE_CXX_STANDARD 23)

if (APPLE)
    set(IS_APPLE TRUE)
    set(OS macos)
    set(ICONV -liconv)
elseif (WIN32)
    set(IS_WINDOWS TRUE)
    set(OS windows)
else ()
    set(IS_LINUX TRUE)
    set(OS linux)
    set(ICONV)
endif ()

# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=libc++ -lc++abi")
set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "..." FORCE)

set(arwen_NAME arwen)
set(arwen_VERSION_MAJOR 1)
set(arwen_VERSION_MINOR 0)
set(arwen_DIR ${CMAKE_INSTALL_PREFIX})
set(arwen_DATADIR ${CMAKE_INSTALL_PREFIX}/share)

include(CheckCSourceCompiles)
include(CheckCXXSourceCompiles)

configure_file(
        "config.h.in"
        "config.h"
)

include_directories("${PROJECT_BINARY_DIR}" "${PROJECT_SOURCE_DIR}")

add_library(
        Util
        STATIC
        src/Util/Checked.h
        src/Util/Error.cpp
        #       src/Util/Integer.h
        src/Util/IO.cpp
        src/Util/JSON.cpp
        src/Util/Lexer.h
        src/Util/Logging.cpp
        src/Util/Options.cpp
        src/Util/Pipe.h
        src/Util/Process.h
        src/Util/Result.h
        src/Util/Defer.h
        src/Util/StringScanner.h
        src/Util/StringUtil.cpp
        src/Util/Token.cpp
        src/Util/Utf8.cpp
)

target_link_libraries(
        Util
        ${ICONV}
        dl
)

include_directories(. src)

add_executable(
        arwen
        src/App/arwen.cpp
        src/App/BoundNode.cpp
        src/App/Parser.cpp
        src/App/Operator.cpp
        src/App/SyntaxNode.cpp
        src/App/Syntax/BinaryExpression.cpp
        src/App/Syntax/Embed.cpp
        src/App/Syntax/Enum.cpp
        src/App/Syntax/Function.cpp
        src/App/Syntax/Include.cpp
        src/App/Syntax/Loop.cpp
        src/App/Syntax/Number.cpp
        src/App/Syntax/Return.cpp
        src/App/Syntax/Struct.cpp
        src/App/Syntax/Type.cpp
        src/App/Type.cpp
)

target_link_libraries(
        arwen
        PRIVATE
        Util
        m
)

#add_compile_options("-fno-inline-functions")

install(TARGETS Util arwen
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        BUNDLE DESTINATION bundle)

install(DIRECTORY share DESTINATION .)
