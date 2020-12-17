import sys
import os
import array

from opcodes import OpCode, stack_effect
from symboltable import SymbolTable, Symbol, SymbolType
from frame import Frame
from function import Function

from vm import VM

from rpython.rlib import rfile
from rpython.rlib import jit


def make_function(name, program, pc):
    num_locals = 0
    stack_size = 0
    max_stack_size = 0
    bytecodes = []

    function_end = False
    while not function_end:
        bytecodes.append(program[pc])
        ops = program[pc]
        opcode = ops[0]

        stack_size += stack_effect(opcode)

        if stack_size > max_stack_size:
            max_stack_size = stack_size

        if opcode == OpCode.STORE_VAR:
            num_locals += 1
        elif opcode == OpCode.HALT:
            function_end = True

        pc += 1

    return Function(name, bytecodes, num_locals, max_stack_size), len(bytecodes)


def parse(program):
    parsed = []
    lines = program.split("\n")
    for line in lines:
        if line != "":
            parsed.append([int(x) for x in line.rstrip("\n").split(' ')])
    return parsed


def run(fp):
    program_contents = ""
    while True:
        read = os.read(fp, 4096)
        if len(read) == 0:
            break
        program_contents += read
    os.close(fp)
    program = parse(program_contents)

    i = 0
    functions = [None] * 1024
    while i < len(program):
        ops = program[i]
        opcode = ops[0]
        if opcode == OpCode.MAKE_FUNCTION:
            name = ops[1]
            func, bytecodes_length = make_function(name,
                                                   program, i+1)
            functions[name] = func
            i += bytecodes_length
        i += 1

    functions = [func for func in functions if func is not None]

    main_func = functions[0]
    init_frame = Frame(None, main_func, main_func.num_locals,
                       main_func.stack_size)
    vm = VM(functions)
    vm.run(init_frame)


def entry_point(argv):
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1

    try:
        run(os.open(filename, os.O_RDONLY, 0777))
    except:
        return 1

    return 0


def target(driver, args):
    if driver.config.translation.jit:
        driver.exe_name = "aver"
    else:
        driver.exe_name = "aver-nojit"
    return entry_point


def jitpolicy(driver):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()


if __name__ == '__main__':
    from rpython.translator.driver import TranslationDriver
    entry = target(TranslationDriver(), sys.argv)
    sys.exit(entry(sys.argv))
