import sys
import os
try:
    from rpython.rlib.jit import JitDriver
except ImportError:
    class JitDriver(object):
        def __init__(self,**kw): pass
        def jit_merge_point(self,**kw): pass
        def can_enter_jit(self,**kw): pass

jitdriver = JitDriver(greens=['pc', 'program'], reds=['stack'])
def jitpolicy(driver):
    from rpython.jit.codewriter.policy import JitPolicy
    return JitPolicy()

def mainloop(program):
    pc = 0
    stack = []

    while pc < len(program):
        jitdriver.jit_merge_point(pc=pc, program=program, stack=stack)
        code = program[pc]
        ops = code.split(" ")
        if ops[0] == "LOAD":
            stack.append(int(ops[1]))
        elif code == "ADD":
            y = stack.pop()
            x = stack.pop()
            stack.append(x+y)
        elif code == "SUB":
            y = stack.pop()
            x = stack.pop()
            stack.append(x-y)
        elif code == "DIV":
            y = stack.pop()
            x = stack.pop()
            stack.append(x/y)
        elif code == "MULT":
            y = stack.pop()
            x = stack.pop()
            stack.append(x*y)

        pc += 1

    print(stack.pop())

def parse(program):
    parsed = []
    lines = program.split("\n")
    for line in lines:
        if line != "":
            parsed.append(line.rstrip("\n"))
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
    mainloop(program)

def entry_point(argv):
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1

    run(os.open(filename, os.O_RDONLY, 0777))
    return 0

def target(*args):
    return entry_point, None

if __name__ == "__main__":
    entry_point(sys.argv)
