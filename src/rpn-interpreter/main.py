import sys, os
from opcodes import OpCode
from rpython.rlib import rfile

LINE_BUFFER_LENGTH = 1024

# try:
#     from rpython.rlib.jit import JitDriver
# except ImportError:
#     class JitDriver(object):
#         def __init__(self,**kw): pass
#         def jit_merge_point(self,**kw): pass
#         def can_enter_jit(self,**kw): pass

# jitdriver = JitDriver(greens=['pc', 'program'], reds=['stack', 'variable_store'])
# def jitpolicy(driver):
#     from rpython.jit.codewriter.policy import JitPolicy
#     return JitPolicy()

def mainloop(program, stdin, stdout):
    pc = 0
    stack = []
    variable_store = {}

    while pc < len(program):
        # jitdriver.jit_merge_point(pc=pc, program=program, stack=stack, variable_store=variable_store)
        code = program[pc]
        ops = code.split('\t')
        if ops[0] == OpCode.LOAD_CONST:
            stack.append(int(ops[1]))
        elif ops[0] == OpCode.LOAD_VAR:
            x = str(ops[1])
            stack.append(variable_store[x])
        elif ops[0] == OpCode.STORE_VAR:
            x = str(ops[1])
            variable_store[x] = stack.pop()
        elif code == OpCode.ADD:
            y = stack.pop()
            x = stack.pop()
            stack.append(x+y)
        elif code == OpCode.SUBTRACT:
            y = stack.pop()
            x = stack.pop()
            stack.append(x-y)
        elif code == OpCode.DIVIDE:
            y = stack.pop()
            x = stack.pop()
            stack.append(x/y)
        elif code == OpCode.MULTIPLY:
            y = stack.pop()
            x = stack.pop()
            stack.append(x*y)
        elif code == OpCode.PRINT:
            print stack.pop()
        elif code == OpCode.INPUT:
            stdout.write("> ")
            line = stdin.readline(LINE_BUFFER_LENGTH).strip()
            stack.append(int(line))
        pc += 1

def parse(program):
    parsed = []
    lines = program.split("\n")
    for line in lines:
        if line != "":
            parsed.append(line.rstrip("\n"))
    return parsed

def run(fp, stdin, stdout):
    program_contents = ""
    while True:
        read = os.read(fp, 4096)
        if len(read) == 0:
            break
        program_contents += read
    os.close(fp)
    program = parse(program_contents)
    mainloop(program, stdin, stdout)

def entry_point(argv):
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1

    stdin, stdout, stderr = rfile.create_stdio()
    try:
        run(os.open(filename, os.O_RDONLY, 0777), stdin, stdout)
    except:
        return 1

    return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    entry_point(sys.argv)
