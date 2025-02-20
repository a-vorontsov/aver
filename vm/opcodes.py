from rpython.rlib import jit


class OpCode(object):
    LOAD_CONST = 0
    LOAD_CONST_I = 1
    LOAD_CONST_F = 2
    LOAD_CONST_B = 3
    LOAD_CONST_C = 4
    LOAD_CONST_S = 5
    LOAD_VAR = 6
    ADD = 7
    MULTIPLY = 8
    DIVIDE = 9
    SUBTRACT = 10
    MOD = 11
    STORE_VAR = 12
    PRINT = 13
    PRINTLN = 14
    INPUT = 15
    CMPNEQ = 16
    CMPEQ = 17
    CMPLT = 18
    CMPLE = 19
    CMPGT = 20
    CMPGE = 21
    CMPAND = 22
    CMPOR = 23
    JUMP = 24
    JUMP_TRUE = 25
    MAKE_ARRAY = 26
    MAKE_EMPTY_ARRAY = 27
    LOAD_FROM_ARRAY = 28
    STORE_TO_ARRAY = 29
    MAKE_OBJECT = 30
    GET_FIELD = 31
    SET_FIELD = 32
    CALL = 33
    MAKE_FUNCTION = 34
    HALT = 35
    RETURN = 36
    PASS = 37
    POP = 38


_stack_effects = [
    1,  # LOAD_CONST
    1,  # LOAD_CONST_I
    1,  # LOAD_CONST_F
    1,  # LOAD_CONST_B
    1,  # LOAD_CONST_C
    1,  # LOAD_CONST_S
    1,  # LOAD_VAR
    -1,  # ADD
    -1,  # MULTIPLY
    -1,  # DIVIDE
    -1,  # SUBTRACT
    -1,  # MOD
    -1,  # STORE_VAR
    -1,  # PRINT
    -1,  # PRINTLN
    1,  # INPUT
    -2,  # CMPNEQ
    -2,  # CMPEQ
    -2,  # CMPLT
    -2,  # CMPLE
    -2,  # CMPGT
    -2,  # CMPGE
    -2, # CMPAND
    -2, # CMPOR
    0,  # JUMP
    -1, # JUMP_TRUE
    1,  # MAKE_ARRAY
    0,  # MAKE_EMPTY_ARRAY
    -1,  # LOAD_FROM_ARRAY
    -2,  # STORE_TO_ARRAY
    1,  # MAKE_OBJECT
    1,  # GET_FIELD
    -1,  # SET_FIELD
    1,  # CALL
    0,  # MAKE_FUNCTION
    0,  # HALT
    -1,  # RETURN
    0,  # PASS
    -1,  # POP
]


@jit.elidable
def stack_effect(opcode, param_length=0):
    assert opcode >= 0 and opcode < len(_stack_effects)
    return _stack_effects[opcode] + -param_length + 1


def _bytecode_names(cls):
    names = [attr for attr in dir(cls) if not callable(
        attr) and not attr.startswith("__")]
    values = {}
    for name in names:
        values[getattr(cls, name)] = name

    return values


bytecode_names = _bytecode_names(OpCode)
