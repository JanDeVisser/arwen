#include <dlfcn.h>
#include <stdio.h>

int main()
{
  dlerror();
  void *lib = dlopen("../cmake-build-debug/lib/libarwenrt.dylib", RTLD_NOW | RTLD_GLOBAL);
  if (lib == NULL) {
    fprintf(stderr, "Could not open lib: %s\n", dlerror());
    return 1;
  }
  printf("lib opened successfully: %s\n", dlerror());
  dlerror();
  void *fnc = dlsym(lib, "my_puts");
  if (fnc == NULL) {
    fprintf(stderr, "Could not resolve function: %s\n", dlerror());
    return 1;
  }
  printf("%p\n", fnc);
  return 0;
}

