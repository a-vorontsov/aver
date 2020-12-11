import sys
import os
import array
from opcodes import OpCode
from stack import Stack
from rpython.rlib import rfile
from rpython.rlib import jit

LINE_BUFFER_LENGTH = 1024


def get_location(pc, program):
    return "%s | %s" % (str(pc), program[pc])


jitdriver = jit.JitDriver(greens=['pc', 'program'], reds=[
    'stack', 'variable_store'], get_printable_location=get_location)


def readline():
    res = ''
    while True:
        buf = os.read(0, 16)
        if not buf:
            return res
        res += buf
        if res[-1] == '\n':
            return res[:-1]


@jit.unroll_safe
def mainloop(program, stdin):
    pc = 0
    stack = Stack()
    variable_store = [0] * 10

    while pc < len(program):
        jitdriver.jit_merge_point(
            pc=pc, program=program, stack=stack, variable_store=variable_store)
        code = program[pc]
        ops = code
        opcode = ops[0]

        if opcode == OpCode.LOAD_CONST:
            stack.push(ops[1])
        elif opcode == OpCode.LOAD_VAR:
            x = ops[1]
            stack.push(variable_store[x])
        elif opcode == OpCode.STORE_VAR:
            x = ops[1]
            variable_store[x] = stack.pop()
        elif opcode == OpCode.ADD:
            y = stack.pop()
            x = stack.pop()
            stack.push(x + y)
        elif opcode == OpCode.SUBTRACT:
            y = stack.pop()
            x = stack.pop()
            stack.push(x - y)
        elif opcode == OpCode.DIVIDE:
            y = stack.pop()
            x = stack.pop()
            stack.push(x / y)
        elif opcode == OpCode.MULTIPLY:
            y = stack.pop()
            x = stack.pop()
            stack.push(x * y)
        elif opcode == OpCode.MOD:
            y = stack.pop()
            x = stack.pop()
            stack.push(x % y)
        elif opcode == OpCode.CMPNEQ:
            y = stack.pop()
            x = stack.pop()
            if x == y:
                jump_to = ops[1]
                pc = pc + jump_to
        elif opcode == OpCode.CMPEQ:
            y = stack.pop()
            x = stack.pop()
            if x != y:
                jump_to = ops[1]
                pc = pc + jump_to
        elif opcode == OpCode.CMPGT:
            y = stack.pop()
            x = stack.pop()
            if x <= y:
                jump_to = ops[1]
                pc = pc + jump_to
        elif opcode == OpCode.CMPLT:
            y = stack.pop()
            x = stack.pop()
            if x >= y:
                jump_to = ops[1]
                pc = pc + jump_to
        elif opcode == OpCode.JMP:
            jump_to = ops[1]
            pc = pc + jump_to
        elif opcode == OpCode.PRINT:
            print stack.pop()
        elif opcode == OpCode.INPUT:
            line = readline()
            stack.push(int(line))
        elif opcode == OpCode.PASS:
            pass
        pc += 1


def parse(program):
    parsed = []
    lines = program.split("\n")
    for line in lines:
        if line != "":
            parsed.append([int(x) for x in line.rstrip("\n").split(' ')])
    return parsed


def run(fp, stdin):
    program_contents = ""
    while True:
        read = os.read(fp, 4096)
        if len(read) == 0:
            break
        program_contents += read
    os.close(fp)
    program = parse(program_contents)
    mainloop(program, stdin)


def entry_point(argv):
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1

    stdin, stdout, stderr = rfile.create_stdio()
    try:
        run(os.open(filename, os.O_RDONLY, 0777), stdin)
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
