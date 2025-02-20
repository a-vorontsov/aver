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
from objects.array import Array
from objects.complex_object import ComplexObject

from rpython.rlib import jit


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
        last_pc = 0
        last_func = None

        pc = frame.pc
        func = frame.func
        while True:
            if pc < last_pc and func is last_func:
                jitdriver.can_enter_jit(
                    pc=pc, func=func, self=self, frame=frame)

            jitdriver.jit_merge_point(pc=pc, func=func, self=self, frame=frame)

            ops = func.bytecode[pc]
            opcode = ops[0]

            debug = True
            if debug:
                print "stack"
                frame.stack_print()
                print "locals"
                frame.local_print()
                print "literals"
                frame.literal_print()
                print get_printable_location(pc, func, self)
                print "---"

            if opcode == OpCode.LOAD_CONST:
                literal = frame.literal_get(ops[1])

                if literal is None:
                    print "Error in LOAD_CONST"
                    print "---"
                    print self.call_stack_size
                    print "---"
                    print func.print_func()
                    print "---"
                    print ops
                    print "---"
                    print "literals"
                    frame.literal_print()
                    assert False

                frame.stack_push(literal)
            elif opcode == OpCode.LOAD_VAR:
                x = ops[1]
                value = frame.local_get(x)

                if value is None:
                    print "Error in LOAD_VAR"
                    print self.call_stack_size
                    print func.print_func()
                    print ops
                    assert False

                frame.stack_push(value)
            elif opcode == OpCode.STORE_VAR:
                x = ops[1]
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

                try:
                    result = lhs.div(rhs)
                except ValueError:
                    print get_printable_location(pc, func, self)
                    raise ValueError
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

                result = lhs.neq(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPEQ:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.eq(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPGE:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.ge(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPGT:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.gt(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPLE:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.le(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPLT:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.lt(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPAND:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.cmpand(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.CMPOR:
                rhs = frame.stack_pop()
                lhs = frame.stack_pop()

                result = lhs.cmpor(rhs)
                frame.stack_push(result)
            elif opcode == OpCode.JUMP:
                jump_to = ops[1]
                new_pc = pc + jump_to
                if new_pc < pc:
                    jitdriver.can_enter_jit(pc=pc, func=func, self=self, frame=frame)
                pc = pc + jump_to
            elif opcode == OpCode.JUMP_TRUE:
                jump_to = ops[1]
                condition_result = frame.stack_pop()

                assert isinstance(condition_result, Boolean)
                result = condition_result.get_value()

                if result == False:
                    new_pc = pc + jump_to
                    if new_pc < pc:
                        jitdriver.can_enter_jit(pc=pc, func=func, self=self, frame=frame)
                    pc = pc + jump_to
            elif opcode == OpCode.PRINT:
                os.write(1, frame.stack_pop().get_string())
            elif opcode == OpCode.PRINTLN:
                print frame.stack_pop().get_string()
            elif opcode == OpCode.INPUT:
                line = self.readline()
                frame.stack_push(Integer(int(line)))
            elif opcode == OpCode.MAKE_ARRAY:
                n_elems = ops[1]
                value = Array(n_elems)
                for i in range(n_elems):
                    elem = frame.stack_pop()
                    value.set_value_at(i, elem)
                frame.stack_push(value)
            elif opcode == OpCode.MAKE_EMPTY_ARRAY:
                n_elems = ops[1]
                value = Array(n_elems)
                elem = frame.stack_pop()
                for i in range(n_elems):
                    value.set_value_at(i, elem)
                frame.stack_push(value)
            elif opcode == OpCode.LOAD_FROM_ARRAY:
                idx = frame.stack_pop()
                assert isinstance(idx, Integer)
                var = frame.stack_pop()
                assert isinstance(var, Array)
                tmp = var.get_value_at(idx.value)
                frame.stack_push(tmp)
            elif opcode == OpCode.STORE_TO_ARRAY:
                var = frame.stack_pop()
                assert isinstance(var, Array)
                print var.get_string()
                idx = frame.stack_pop()
                assert isinstance(idx, Integer)
                print idx.get_string()
                val = frame.stack_pop()
                print val.get_string()
                var.set_value_at(idx.value, val)
                frame.stack_push(var)
            elif opcode == OpCode.MAKE_OBJECT:
                size = ops[1]
                value = ComplexObject(size)
                frame.stack_push(value)
            elif opcode == OpCode.GET_FIELD:
                idx = ops[1]
                var = frame.stack_pop()
                assert isinstance(var, ComplexObject)
                tmp = var.get_value_at(idx)
                frame.stack_push(tmp)
            elif opcode == OpCode.SET_FIELD:
                idx = ops[1]
                val = frame.stack_pop()
                assert isinstance(val, PrimitiveObject)
                var = frame.stack_pop()
                assert isinstance(var, ComplexObject)
                var.set_value_at(idx, val)
                frame.stack_push(var)
            elif opcode == OpCode.CALL:
                name = ops[1]
                params = ops[2]
                new_func = self.functions[name]
                new_frame = Frame(
                    frame, new_func, new_func.num_locals, new_func.literals, new_func.stack_size)
                for i in range(params):
                    val = frame.stack_pop()
                    if val is None:
                        print "Error in CALL params"
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
            elif opcode == OpCode.POP:
                frame.stack_pop()

            last_pc = pc
            last_func = frame.func
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
            return res.rstrip("\r\n")
