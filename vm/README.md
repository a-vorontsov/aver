# Bytecode Interpreter

- Install pypy
- Clone pypy repo
- Run `pypy <pypy_path>/rpython/bin/rpython --opt=jit main.py` to compile and output main-c file
- Run `./main-c ../rpn-builder/a.avb` to see bytecode output
