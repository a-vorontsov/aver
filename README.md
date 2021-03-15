# Aver

Aver is a strongly and statically typed language with support for generics.

Developed during my final year project for King's College London under supervision from Dr Laurence Tratt.

## Build

Aver's compiler is written in OCaml which generates bytecode for a VM built using RPython.

### Compiler

OCaml and Dune

Follow the instructions at [https://ocaml.org/learn/tutorials/up_and_running.html] to set up and install OCaml, opam and Dune which are required to build the compiler for Aver.

### VM

Python2 and [RPython](https://rpython.readthedocs.io/en/latest/).

#### Installing RPython

To use RPython you need the PyPy source which includes the RPython framework.

```bash
$ cd <path_to>
$ hg clone https://foss.heptapod.net/pypy/pypy
$ export PYPY_SRC=<path_to>/pypy
```

---

Once all frameworks and tools have been installed, run the following commands to configure environment variables to a local file and build the compiler and the non-JIT optimised version of the interpreter:

```bash
./configure
make
```

To build the interpreter with JIT run the following command:

```bash
make build-jit
```

## Install

Install the built binaries to a single directory, `bin/`, at the root of this project where you'll find the compiler, `averc`, and bytecode interpreters `aver{-nojit}`.

```bash
make install
```

## Usage

Compile a `.av` file into bytecode by running the following command:

```sh
averc <file>.av
```

Run a compiled bytecode file using the following command:

### Without JIT optimisation:

```sh
aver-nojit <file>.avb
```

### With JIT optimisation:

```sh
aver <file>.avb
```
