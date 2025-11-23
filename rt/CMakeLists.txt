
include_directories(.)

enable_language(ASM-ATT)

if (IS_ARM64)

add_library(
        arwenstart
        STATIC
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/start.s
      )

endif()

add_library(
        trampoline
        STATIC
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/trampoline.s
)

add_library(
        arwenrt
        SHARED
        dynarr.c
        puts.c
        utf8.c
        # arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/strlen.s
        # arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/puti.s
        # arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/to_string.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/alloc.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/endln.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/open.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/puthex.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/puts.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/putln.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/string_cmp.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/string_concat.s
        #        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/string_eq.s
)

set_target_properties(arwenrt PROPERTIES LINKER_LANGUAGE C)

if(IS_ARM64)

install(TARGETS arwenrt arwenstart trampoline
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)

else()

install(TARGETS arwenrt trampoline
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)

endif()
