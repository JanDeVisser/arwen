# lia

A post-C/C++ systems programming language.

I am aware `lia` sounds a lot like `lua`. I realized this halfway through
another renaming session. This means that the language may change names again
sooner rather than later.

## How to build

You need

- `cmake` 3.27 or newer. Older versions may work too but your will have to
adjust the minimum required version on the first line of `CMakeLists.txt`.
- `gcc` 14.2 or newer. Theoretically `clang` should mostly work but making the
practice hasn't been done yet. On MacOS `gcc` 15 is available from `brew` at
the time of writing.
- [QBE](https://c9x.me/compile/). QBE is included in most Linux package managers
and MacOS `brew`.
- Python3 for the test runner.

### Building on Linux
```
    $ mkdir build
    $ cd build
    $ cmake .. # I add -GNinja but this is not necessary
    $ cd ..
    $ cmake --build build --target install
    $ export LD_LIBRARY_PATH=`pwd`/build/lib:$LD_LIBRARY_PATH
    $ cd test
    $ ./run_tests.py -a
```

### Building on MacOS
```
    $ mkdir build
    $ cd build
    $ cmake -DCMAKE_CXX_COMPILER=g++-15 ..
    $ cd ..
    $ cmake --build build --target install
    $ export LD_LIBRARY_PATH=`pwd`/build/lib:$LD_LIBRARY_PATH
    $ cd test
    $ ./run_tests.py -a
```

## The language

At this point, the best way to explore the current state of the language is
by perusing the `.lia` files in the `test` directory. The files with leading
numbers are part of the test suite and are representative of the current
state of the language.

