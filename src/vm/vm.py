import os

from opcodes import OpCode, bytecode_names
from frame import Frame
from function import Function

from objects.primitive_object import PrimitiveObject
from objects.boolean import Boolean
from objects.char import Char
from objects.float import Float
from objects.integer import Integer
from objects.string import String

from rpython.rlib import jit


def get_printable_location(pc, func, self):
    ops = func.bytecode[pc]
    op = bytecode_names[int(ops[0])]
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
            opcode = int(ops[0])

            # frame.stack_print()
            # print get_printable_location(pc, func, self)

            if opcode == OpCode.LOAD_CONST:
                literal = frame.literal_get(int(ops[1]))
                frame.stack_push(literal)
            elif opcode == OpCode.LOAD_CONST_I:
                frame.stack_push(Integer(int(ops[1])))
            elif opcode == OpCode.LOAD_CONST_F:
                frame.stack_push(Float(float(ops[1])))
            elif opcode == OpCode.LOAD_CONST_B:
                frame.stack_push(Boolean(bool(ops[1])))
            elif opcode == OpCode.LOAD_CONST_C:
                frame.stack_push(Char(str(ops[1])[0]))
            elif opcode == OpCode.LOAD_CONST_S:
                frame.stack_push(String(str(ops[1])))
            elif opcode == OpCode.LOAD_VAR:
                x = int(ops[1])
                value = frame.local_get(x)

                if value is None:
                    print "Error"
                    print self.call_stack_size
                    print func.print_func()
                    print ops
                    assert False

                frame.stack_push(value)
            elif opcode == OpCode.STORE_VAR:
                x = int(ops[1])
                new_value = frame.stack_pop()
                frame.local_set(x, new_value)

            elif opcode == OpCode.ADD:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.add(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.SUBTRACT:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.sub(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.DIVIDE:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.div(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.MULTIPLY:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.mul(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.MOD:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.mod(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPNEQ:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                if lhs.eq(rhs):
                    jump_to = int(ops[1])
                    pc = pc + jump_to
            elif opcode == OpCode.CMPEQ:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                if lhs.neq(rhs):
                    jump_to = int(ops[1])
                    pc = pc + jump_to
            elif opcode == OpCode.CMPGE:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                if lhs.lt(rhs):
                    jump_to = int(ops[1])
                    pc = pc + jump_to
            elif opcode == OpCode.CMPGT:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                if lhs.le(rhs):
                    jump_to = int(ops[1])
                    pc = pc + jump_to
            elif opcode == OpCode.CMPLE:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                if lhs.gt(rhs):
                    jump_to = int(ops[1])
                    pc = pc + jump_to
            elif opcode == OpCode.CMPLT:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                if lhs.ge(rhs):
                    jump_to = int(ops[1])
                    pc = pc + jump_to
            elif opcode == OpCode.JMP:
                jitdriver.can_enter_jit(
                    pc=pc, func=func, self=self, frame=frame)
                jump_to = int(ops[1])
                pc = pc + jump_to
            elif opcode == OpCode.PRINT:
                os.write(0, frame.stack_pop().get_string())
            elif opcode == OpCode.PRINTLN:
                print frame.stack_pop().get_string()
            elif opcode == OpCode.INPUT:
                line = self.readline()
                frame.stack_push(Integer(int(line)))
            elif opcode == OpCode.CALL:
                name = int(ops[1])
                params = int(ops[2])
                new_func = self.functions[name]
                new_frame = Frame(
                    frame, new_func, new_func.num_locals, new_func.literals, new_func.stack_size)
                for i in range(params):
                    val = frame.stack_pop()
                    if val is None:
                        print "Error"
                        print self.call_stack_size
                        print func.print_func()
                        print ops
                        assert False
                    new_frame.local_set(i, val)
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
