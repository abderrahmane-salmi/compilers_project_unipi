
# LCI Project: MiniImp, MiniFun, and MiniTyFun Interpreters & Compilers

## Table of Contents
1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Setting Up the Environment](#setting-up-the-environment)
4. [MiniImp Interpreter](#miniimp-interpreter)
5. [MiniFun Interpreter](#minifun-interpreter)
6. [MiniTyFun Interpreter](#minityfun-interpreter)
7. [MiniImp Compiler](#miniimp-compiler)
8. [Project Structure](#project-structure)

---

## Overview  
This repository is for the project of the module **Languages, Compilers and Interpreters**, at the **University Of Pisa**, for the academic year **2024-25**, in which we implemented **interpreters** and a **compiler** for three educational languages: **MiniImp**, **MiniFun**, and **MiniTyFun**. MiniImp is a simple imperative language, MiniFun extends it with first-class functions, and MiniTyFun adds static type inference.
We also addressed parsing challenges and **ambiguity** **resolution** in MiniImp and MiniFun, ensuring correct syntax interpretation. 
Additionally, we developed a **MiniImp** **compiler** that translates programs into MiniRISC, a simplified assembly language. This process includes CFG generation, register allocation, liveness analysis, and optimization for a target machine with a limited number of registers.


------------

## Prerequisites

Before using the project, ensure the following:
- **OCaml and Dune**: The program is written in OCaml and uses Dune for building. Make sure OCaml and Dune are installed on your system.
- **MiniImp/MiniFun Program**: Prepare a MiniImp or MiniFun program file (e.g., `program.miniimp` or `program.minifun`) that you want to compile or interpret.

---

## Setting Up the Environment

If you are using **VS Code** or a **terminal**, you need to set up the OCaml environment using `opam env`. This step ensures that the necessary tools and libraries are available for building and running the program.

**For Command Prompt (CMD)**:

```bash
for /f "tokens=*" %i in ('opam env') do @%i
```

**For PowerShell & VS Code:**
```bash
(& opam env) -split '\r?\n' | ForEach-Object { Invoke-Expression $_ }
```

------------

## MiniImp Interpreter
The MiniImp interpreter takes a MiniImp program as input, along with an integer value as input to the program, and outputs the result.
1. Navigate to the MiniImp interpreter directory.
2. Build the program using dune.
3. Run the program by typing the run command.

```bash
cd miniimp_interpreter
dune build
dune exec ./main.exe <program.miniimp>
dune exec ./main.exe ./test/program.miniimp
```


------------

## MiniFun Interpreter
The MiniFun interpreter takes a MiniFun program as input, compiles it, and outputs the result.
1. Navigate to the MiniFun interpreter directory.
2. Build the program using dune.
3. Run the program by typing the run command.

```bash
cd minifun_interpreter
dune build
dune exec ./main.exe <program.minifun>
dune exec ./main.exe ./test/prgm.minifun
```


------------

## MiniTyFun Interpreter
The MiniTyFun interpreter takes a MiniTyFun program as input and outputs the result. Note that we did not implement a parser for MiniTyFun, as it shares the same parser as MiniFun.
1. Navigate to the MiniTyFun interpreter directory.
2. Build the program using ocamlc.
3. Run the program by typing the run command using ocamlrun.

```bash
cd minityfun_interpreter
ocamlc -o minityfun minityfun.ml
ocamlrun minityfun
```


------------

## MiniImp Compiler
The MiniImp compiler takes a MiniImp program as input and produces an optimized MiniRISC program as output. The compiler allows you to configure several options, including the number of registers available in the target machine, whether to check for undefined variables, and whether to apply optimizations.
1. Navigate to the MiniImp compiler directory.
2. Build the program using dune.
3. Run the program by typing the run command and specifying the arguments correctly:
	- `<num_registers>`: The number of registers available in the target machine. Must be an integer greater than or equal to 4.
	- `<check_undefined_vars>`: A boolean flag (true or false) to enable or disable the undefined variable check. If enabled, the compiler will check for undefined variables and fail if any are found.
	- `<enable_optimization>`: A boolean flag (true or false) to enable or disable optimization. If enabled, the compiler will perform register merging and other optimizations.
	- `<program.miniimp>`: The path to the MiniImp program file you want to compile.
	- `<output_path>`: The path where the compiled MiniRISC code will be written.

```bash
cd miniimp_compiler
dune build
dune exec ./main.exe <num_registers> <check_undefined_vars> <enable_optimization> <program.miniimp> <output_path>
dune exec ./main.exe 5 true true ./test/prgm.miniimp ./compiled/prgm.minirisc
```


------------

## Project Structure
```bash
├── miniimp_interpreter/   # MiniImp interpreter
├── minifun_interpreter/   # MiniFun interpreter
├── minityfun_interpreter/ # MiniTyFun interpreter
├── compiler_compiler/     # MiniImp Compiler
```
