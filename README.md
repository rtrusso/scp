# Quick Start

Use the following steps to bootstrap and validate the compiler.

## Windows

- Ensure GCC, GNU make, NASM, diff in path
- run src\go.cmd

## Linux

- apt-get install gcc make nasm diffutils
- run ./src/go.sh

Since this will take a long time, consider running go.sh under nohup
and/or in the background.

# Sunnyvale Compiler Platform

This codebase aspires to facilitate the rapid or experimental
development of traditional ("ahead-of-time") compilers for a wide
range of programming languages.

The codebase consists of these major components:

- Frontends for a subset of the Scheme and Java programming languages
- Lexer and parser generator tools
- an intermediate language: Symbolic Assembly or SASM
- A backend with code analysis and improvement phases (optimizer)
- Translation of SASM to practical assembly languages (x86, MIPS)
- A runtime library with basic memory management and a garbage collector

The idea is that to build a compiler for some programming language,
one has to mainly write just a frontend that emits SASM, and runtime
library, and take advantage of as many of the existing components as
possible to get that working as quickly and/or easily as
possible. Ideally, there should also be reuse of the runtime
components to the extent possible.

In this way it has similar goals to something like .NET or the JVM
without all that mucking about in virtual machines.

- [Status](readme/Status.md)
- [Dependencies](readme/Depend.md)
- [SASM Introduction](readme/SASM.md)
- [Tools](readme/Tools.md)
