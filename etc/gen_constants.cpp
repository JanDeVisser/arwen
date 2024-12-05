#include <cstddef>
#include <cstdio>
#import <fcntl.h>
#import <stdio.h>
#import <unistd.h>
#import <sys/fcntl.h>
#import <sys/mman.h>

#define S(T) printf("// alias %s = u%lu\n", #T, sizeof(T)*8);
#define P(X) printf("const %s: i32 = 0x%08X\n", #X, X);

int main() {
    S(size_t)
    S(ssize_t)
    S(off_t)
    S(ptrdiff_t)
    P(O_RDONLY)
    P(O_WRONLY)
    P(O_RDWR)
    P(O_SEARCH)
    P(O_EXEC)
    P(O_NONBLOCK)
    P(O_APPEND)
    P(O_CREAT)
    P(O_TRUNC)
    P(O_EXCL)
    P(O_SHLOCK)
    P(O_EXLOCK)
    P(O_DIRECTORY)
    P(O_NOFOLLOW)
    P(O_SYMLINK)
    P(O_EVTONLY)
    P(O_CLOEXEC)
    P(O_NOFOLLOW_ANY)
    P(SEEK_SET)
    P(SEEK_CUR)
    P(SEEK_END)
    P(SEEK_DATA)
    P(SEEK_HOLE)
    P(MAP_ANON)
    P(MAP_ANONYMOUS)
    P(MAP_FILE)
    P(MAP_FIXED)
    P(MAP_HASSEMAPHORE)
    P(MAP_PRIVATE)
    P(MAP_SHARED)
    P(MAP_NOCACHE)
    P(MAP_JIT)
    P(MAP_32BIT)
    P(PROT_NONE)
    P(PROT_READ)
    P(PROT_WRITE)
    P(PROT_EXEC)
}
