from rpython.rlib import jit


class OpCode(object):
    LOAD_CONST_I = 0
    LOAD_CONST_F = 1
    LOAD_CONST_B = 2
    LOAD_CONST_C = 3
    LOAD_CONST_S = 4
    LOAD_VAR = 5
    ADD = 6
    MULTIPLY = 7
    DIVIDE = 8
    SUBTRACT = 9
    MOD = 10
    STORE_VAR = 11
    PRINT = 12
    PRINTLN = 13
    INPUT = 14
    CMPNEQ = 15
    CMPEQ = 16
    CMPLT = 17
    CMPLE = 18
    CMPGT = 19
    CMPGE = 20
    JMP = 21
    CALL = 22
    MAKE_FUNCTION = 23
    HALT = 24
    RETURN = 25
    PASS = 26
    LOAD_CONST = 27


_stack_effects = [
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
    0,  # JMP
    1,  # CALL
    0,  # MAKE_FUNCTION
    0,  # HALT
    -1,  # RETURN
    0,  # PASS
    1,  # LOAD_CONST
]


@jit.elidable
def stack_effect(opcode, param_length=0):
    assert opcode >= 0 and opcode < len(_stack_effects)
    return _stack_effects[opcode]


def _bytecode_names(cls):
    names = [attr for attr in dir(cls) if not callable(
        attr) and not attr.startswith("__")]
    values = {}
    for name in names:
        values[getattr(cls, name)] = name

    return values


bytecode_names = _bytecode_names(OpCode)
