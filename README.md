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

## What can `jnsn` do so far?
For each step in the parsing/compilation pipeline, I usually write a small cli tool to be able to interactively test it.
Therefore you'll find the following binaries in `<build>/bin`:
* `lexer_cli`: Asks for code, outputs the token stream from the lexer
* `parser_cli`: Asks for code, outputs an unformatted JSON representation of the parsed AST
* `exec_cli`: Asks for code. Currently uses the `ast_exec` class to try to interpret parts of the parsed AST.
  Unfortunately, after realizing that JS needs at least two passes through the AST (for hoisting), I dropped development of `ast_exec`.
  I'll leave it in the repo as long as there is no proper execution engine (which I hope will be implemented on IR-level)
* `ir_cli`: Asks for code and outputs the IR `module` as created by `ast_to_ir`

If you are less interested in playing around and instead are interested in what each stage is currently capable of, take a look at the [unittests/](unittests/) directory.
Our unittests usually reflect best what we *should* be able to process.

## Building Instructions
This project is written in *C++17*.
That means that at least you will need a modern compiler to build `jnsn`.
The only supported (meta) build system is **CMake**.
It will produce two targets of interest in the generated build system:
1. all (*default*)
2. unittests

The former should be self-explanatory.
`unittests` uses a downloaded copy of the `googletest` framework to compile and link against this project's unittest suite.
Furthermore, it also automatically runs the unittests.
 
