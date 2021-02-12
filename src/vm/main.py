import sys
import os
import array

from opcodes import OpCode, stack_effect
from symboltable import Symbol, SymbolType
from frame import Frame
from function import Function

from objects.primitive_object import PrimitiveObject
from objects.boolean import Boolean
from objects.char import Char
from objects.float import Float
from objects.integer import Integer
from objects.string import String
from objects.null import Null

from vm import VM

from rpython.rlib import rfile, jit


def make_function(name, params, program, pc):
    num_locals = params
    stack_size = 0
    max_stack_size = 0
    bytecodes = []
    literals = [Null()]
    literal_vars = [None]

    function_end = False
    while not function_end:

        ops = program[pc]
        opcode = int(ops[0])

        if opcode == OpCode.CALL:
            params = int(ops[2])
            stack_size += stack_effect(opcode, params)
        else:
            stack_size += stack_effect(opcode)

        if stack_size > max_stack_size:
            max_stack_size = stack_size

        if opcode == OpCode.STORE_VAR:
            num_locals += 1
        elif OpCode.LOAD_CONST_I <= opcode and opcode <= OpCode.LOAD_CONST_S:
            if ops[1] in literal_vars:
                program[pc] = [str(OpCode.LOAD_CONST), str(
                    literal_vars.index(ops[1]))]
            else:
                if opcode == OpCode.LOAD_CONST_I:
                    literals.append(Integer(int(ops[1])))
                elif opcode == OpCode.LOAD_CONST_F:
                    literals.append(Float(float(ops[1])))
                elif opcode == OpCode.LOAD_CONST_B:
                    literals.append(Boolean(bool(ops[1])))
                elif opcode == OpCode.LOAD_CONST_C:
                    literals.append(Char(str(ops[1])[0]))
                elif opcode == OpCode.LOAD_CONST_S:
                    literals.append(String(str(ops[1])))
                literal_vars.append(ops[1])
                program[pc] = [str(OpCode.LOAD_CONST),
                               str(len(literal_vars) - 1)]

        elif opcode == OpCode.HALT:
            function_end = True

        int_ops = [int(op) for op in program[pc]]
        bytecodes.append(int_ops)
        pc += 1

    return Function(name, bytecodes, num_locals, literals, max_stack_size), len(bytecodes)


def parse(program):
    parsed = []
    lines = program.split("\n")
    for line in lines:
        if line != "":
            parsed.append([x for x in line.rstrip("\n").split("\t")])
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
        opcode = int(ops[0])
        if opcode == OpCode.MAKE_FUNCTION:
            name = int(ops[1])
            params = int(ops[2])
            func, bytecodes_length = make_function(name, params, program, i+1)
            functions[name] = func
            i += bytecodes_length
        i += 1

    functions = [func for func in functions if func is not None]

    main_func = functions[0]
    init_frame = Frame(None, main_func, main_func.num_locals, main_func.literals,
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
