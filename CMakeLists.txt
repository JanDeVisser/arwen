cmake_minimum_required(VERSION 3.27)
project(arwen CXX C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)

message("System: ${CMAKE_SYSTEM_NAME}. Processor: ${CMAKE_SYSTEM_PROCESSOR}. Version: ${CMAKE_SYSTEM_VERSION}. Compiler: ${CMAKE_CXX_COMPILER_ID}.")

# if (NOT "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
# message(FATAL_ERROR "Arwen requires Clang because it used C++ language extensions not supported in GCC")
# endif()
set(CMAKE_C_STANDARD 23)
set(CMAKE_CXX_STANDARD 26)

# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-all-candidates")

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

if( "${CMAKE_SYSTEM_PROCESSOR}"  STREQUAL "arm64")
  set(IS_ARM64 TRUE)
endif()

if( "${CMAKE_SYSTEM_PROCESSOR}"  STREQUAL "x86_64")
  set(IS_X86_64 TRUE)
endif()

# set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stdlib=libc++ -lc++abi")
set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "..." FORCE)

set(arwen_NAME arwen)
set(arwen_VERSION_MAJOR 1)
set(arwen_VERSION_MINOR 0)
set(arwen_DIR ${CMAKE_INSTALL_PREFIX})
set(arwen_DATADIR ${CMAKE_INSTALL_PREFIX}/share)

#add_compile_options("-fsanitize=address")
#add_link_options("-fsanitize=address")

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
        src/Util/Align.h
        src/Util/Arena.cpp
        src/Util/Checked.h
        src/Util/Defer.h
        src/Util/Error.cpp
        #       src/Util/Integer.h
        src/Util/IO.cpp
        src/Util/JSON.cpp
        src/Util/Lexer.h
        src/Util/Logging.cpp
        src/Util/Options.cpp
        src/Util/Pipe.cpp
        src/Util/Process.cpp
        src/Util/Resolve.cpp
        src/Util/StringScanner.h
        src/Util/StringUtil.cpp
        src/Util/Token.cpp
        src/Util/TokenLocation.h
        src/Util/Utf8.cpp
)

target_link_libraries(
        Util
        ${ICONV}
        dl
)

add_library(
    ArwenLang
    STATIC
    src/App/Config.cpp
    src/App/Parser.cpp
    src/App/Operator.cpp
    src/App/SyntaxNode.cpp
    src/App/Syntax/Block.cpp
    src/App/Syntax/Branch.cpp
    src/App/Syntax/Constant.cpp
    src/App/Syntax/Embed.cpp
    src/App/Syntax/Enum.cpp
    src/App/Syntax/Expression.cpp
    src/App/Syntax/Function.cpp
    src/App/Syntax/Import.cpp
    src/App/Syntax/Include.cpp
    src/App/Syntax/Insert.cpp
    src/App/Syntax/Loop.cpp
    src/App/Syntax/Module.cpp
    src/App/Syntax/Namespace.cpp
    src/App/Syntax/Program.cpp
    src/App/Syntax/Public.cpp
    src/App/Syntax/Return.cpp
    src/App/Syntax/Struct.cpp
    src/App/Syntax/Type.cpp
    src/App/Syntax/Variable.cpp
    src/App/Type.cpp
    src/App/Value.cpp
    src/App/IR/Generate.cpp
    src/App/IR/IR.cpp
    src/Interp/Execute.cpp
    src/Interp/Interpreter.cpp
    src/Interp/Native.cpp
    src/Interp/Scope.cpp
    src/Interp/Stack.cpp
)

target_link_libraries(
        ArwenLang
        PRIVATE
        arwenrt
        Util
        trampoline
        m
)

if (IS_APPLE)

add_library(
    ArwenCodeGen
    STATIC
    src/Arch/Arm64/Arm64.cpp
    src/Arch/Arm64/GenOp.cpp
)

endif()

if (IS_LINUX)
add_library(
    ArwenCodeGen
    STATIC
    src/Arch/Linux/x86_64/X86_64.cpp
    src/Arch/Linux/x86_64/GenOp.cpp
  )
endif()  

add_subdirectory(rt)

include_directories(. src)

target_link_libraries(
        ArwenCodeGen
        PRIVATE
        ArwenLang
      )

add_executable(
        arwen
        src/App/Main.cpp
)

target_link_libraries(
        arwen
        PRIVATE
        ArwenCodeGen
)

#add_compile_options("-fno-inline-functions")

install(TARGETS Util ArwenLang ArwenCodeGen arwen
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        BUNDLE DESTINATION bundle)

install(DIRECTORY share DESTINATION .)
