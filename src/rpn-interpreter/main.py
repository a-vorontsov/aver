import sys, os
from opcodes import OpCode
from rpython.rlib import rfile

LINE_BUFFER_LENGTH = 1024

try:
    from rpython.rlib.jit import JitDriver
except ImportError:
    class JitDriver(object):
        def __init__(self,**kw): pass
        def jit_merge_point(self,**kw): pass
        def can_enter_jit(self,**kw): pass


def get_location(pc, program):
    return "%s | %s" % (str(pc), program[pc])

jitdriver = JitDriver(greens=['pc', 'program'], reds=['stack', 'variable_store'], get_printable_location=get_location)

def readline():
    res = ''
    while True:
        buf = os.read(0, 16)
        if not buf: return res
        res += buf
        if res[-1] == '\n': return res[:-1]

def mainloop(program, stdin):
    pc = 0
    stack = []
    variable_store = {}

    while pc < len(program):
        jitdriver.jit_merge_point(pc=pc, program=program, stack=stack, variable_store=variable_store)
        code = program[pc]
        ops = code.split(' ')
        opcode = ops[0]
        # print get_location(pc, program)
        if opcode == OpCode.LOAD_CONST:
            stack.append(int(ops[1]))
        elif opcode == OpCode.LOAD_VAR:
            x = str(ops[1])
            stack.append(variable_store[x])
        elif opcode == OpCode.STORE_VAR:
            x = str(ops[1])
            variable_store[x] = stack.pop()
        elif opcode == OpCode.ADD:
            y = stack.pop()
            x = stack.pop()
            stack.append(x+y)
        elif opcode == OpCode.SUBTRACT:
            y = stack.pop()
            x = stack.pop()
            stack.append(x-y)
        elif opcode == OpCode.DIVIDE:
            y = stack.pop()
            x = stack.pop()
            stack.append(x/y)
        elif opcode == OpCode.MULTIPLY:
            y = stack.pop()
            x = stack.pop()
            stack.append(x*y)
        elif opcode == OpCode.CMPNEQ:
            y = stack.pop()
            x = stack.pop()
            if x == y:
                jump_to = int(ops[1])
                pc = pc + jump_to
        elif opcode == OpCode.JMP:
            jump_to = int(ops[1])
            pc = pc + jump_to
        elif opcode == OpCode.PRINT:
            print stack.pop()
        elif opcode == OpCode.INPUT:
            line = readline()
            stack.append(int(line))
        pc += 1

def parse(program):
    parsed = []
    lines = program.split("\n")
    for line in lines:
        if line != "":
            parsed.append(line.rstrip("\n"))
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

def target(*args):
    return entry_point, None

def jitpolicy(driver):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()

if __name__ == "__main__":
    entry_point(sys.argv)
