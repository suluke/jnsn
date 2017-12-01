# jnsn (pronounced _'Johnson'_)
> **J**avaScript ? **N**ope. **S**omething better? **N**ope.

Johnson is my handwritten-compiler side-project.
I like parsing [stuff](https://github.com/suluke/stuff), but I usually don't know what would be useful.
So this is my cheap attempt to implement some JavaScript-compiler-ish tools.
Maybe I'll find a useful purpose along the way.
Or not.
At least I'll have learned a ton about compilers.

Currently I envision the following sorts of useful outcomes for this project:
1. native-speed applications to handle steps in the JS-project bundling pipeline (*transpiler*, *linter*, *formatter*...)
2. a small proof-of-concept JS virtual machine (*JS VM*)
3. an LLVM-backend that compiles JS ahead of time (*AOT*)

## Building Instructions
This project is written in *C++17*.
That means that at least you will need a modern compiler to build jnsn.
The only supported (meta) build system is **CMake**.
It will produce two targets of interested in the generated build system:
1. all (*default*)
2. unittests

The former should be self-explanatory.
`unittests` uses a downloaded copy of the `googletest`  framework to compile and link against this projects unittest suite.
Furthermore, it also automatically runs the unittests.
 
