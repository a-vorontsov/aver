import os

from opcodes import OpCode, bytecode_names
from symboltable import Symbol, SymbolType
from frame import Frame
from function import Function

from rpython.rlib import jit

MAX_CALL_STACK_SIZE = 1024


def get_printable_location(pc, func, self):
    ops = func.bytecode[pc]
    op = bytecode_names[ops[0]]
    return "pc: %s | bytecode name: %s | bytecode: %s" % (str(pc), op, ops)


jitdriver = jit.JitDriver(
    greens=["pc", "func", "self"],
    reds=["frame"],
    virtualizables=["frame"],
    get_printable_location=get_printable_location
)


class VM(object):
    def __init__(self, functions):
        self.call_stack_size = 0
        self.functions = functions

    def run(self, frame):
        self.call_stack_size += 1
        pc = frame.pc
        func = frame.func
        while True:
            if pc != -1:
                jitdriver.can_enter_jit(
                    pc=pc, func=func, self=self, frame=frame)

            jitdriver.jit_merge_point(pc=pc, func=func, self=self, frame=frame)

            ops = func.bytecode[pc]
            opcode = ops[0]

            # print get_printable_location(pc, func, self)

            if opcode == OpCode.LOAD_CONST:
                frame.stack_push(ops[1])
            elif opcode == OpCode.LOAD_VAR:
                x = ops[1]
                symbol = frame.local_get(x)

                if symbol is None:
                    print "Error"
                    print self.call_stack_size
                    print func.print_func()
                    print ops
                    assert False

                frame.stack_push(int(symbol.get_value()))
            elif opcode == OpCode.STORE_VAR:
                x = ops[1]
                symbol = frame.local_get(x)

                if symbol is not None:
                    val = frame.stack_pop()
                    new_symbol = symbol.set_value(val)
                    frame.local_set(x, new_symbol)
                else:
                    new_symbol = Symbol(SymbolType.INT, frame.stack_pop())
                    frame.local_set(x, new_symbol)

            elif opcode == OpCode.ADD:
                y = frame.stack_pop()
                x = frame.stack_pop()
                frame.stack_push(x + y)
            elif opcode == OpCode.SUBTRACT:
                y = frame.stack_pop()
                x = frame.stack_pop()
                frame.stack_push(x - y)
            elif opcode == OpCode.DIVIDE:
                y = frame.stack_pop()
                x = frame.stack_pop()
                frame.stack_push(x / y)
            elif opcode == OpCode.MULTIPLY:
                y = frame.stack_pop()
                x = frame.stack_pop()
                frame.stack_push(x * y)
            elif opcode == OpCode.MOD:
                y = frame.stack_pop()
                x = frame.stack_pop()
                frame.stack_push(x % y)
            elif opcode == OpCode.CMPNEQ:
                y = frame.stack_pop()
                x = frame.stack_pop()
                if x == y:
                    jump_to = ops[1]
                    pc = pc + jump_to
            elif opcode == OpCode.CMPEQ:
                y = frame.stack_pop()
                x = frame.stack_pop()
                if x != y:
                    jump_to = ops[1]
                    pc = pc + jump_to
            elif opcode == OpCode.CMPGT:
                y = frame.stack_pop()
                x = frame.stack_pop()
                if x <= y:
                    jump_to = ops[1]
                    pc = pc + jump_to
            elif opcode == OpCode.CMPLT:
                y = frame.stack_pop()
                x = frame.stack_pop()
                if x >= y:
                    jump_to = ops[1]
                    pc = pc + jump_to
            elif opcode == OpCode.JMP:
                jitdriver.can_enter_jit(
                    pc=pc, func=func, self=self, frame=frame)
                jump_to = ops[1]
                pc = pc + jump_to
            elif opcode == OpCode.PRINT:
                print frame.stack_pop()
            elif opcode == OpCode.INPUT:
                line = self.readline()
                frame.stack_push(int(line))
            elif opcode == OpCode.CALL:
                name = ops[1]
                params = ops[2]
                new_func = self.functions[name]
                new_frame = Frame(
                    frame, new_func, new_func.num_locals, new_func.stack_size)
                for i in range(params):
                    val = frame.stack_pop()
                    if val is None:
                        print "Error"
                        print self.call_stack_size
                        print func.print_func()
                        print ops
                        assert False
                    new_symbol = Symbol(SymbolType.INT, val)
                    new_frame.local_set(i, new_symbol)
                self.invoke_call(new_frame)
            elif opcode == OpCode.HALT:
                self.call_stack_size -= 1
                return 0
            elif opcode == OpCode.RETURN:
                self.call_stack_size -= 1
                parent = frame.parent
                if parent is not None:
                    parent.stack_push(frame.stack_pop())
                return 0
            elif opcode == OpCode.PASS:
                pass

            pc += 1

    def invoke_call(self, frame):
        self.run(frame)

    def readline(self):
        res = ''
        while True:
            buf = os.read(0, 16)
            if not buf:
                return res
            res += buf
            if res[-1] == '\n':
                return res[:-1]
