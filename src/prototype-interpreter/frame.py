from symboltable import SymbolTable
from rpython.rlib import jit


class Frame(object):
    __slots__ = ("parent", "stack", "stacktop", "func", "pc", "locals")
    _immutable_fields_ = ("parent", "stack", "func", "pc", "locals")
    _virtualizable_ = ("parent", "stack[*]", "stacktop", "locals[*]")

    def __init__(self, parent, func, local_vars, stack_size):
        self = jit.hint(self, access_directly=True, fresh_virtualizable=True)
        self.parent = parent
        self.locals = [None] * local_vars
        self.stack = [0] * stack_size
        self.stacktop = 0
        self.func = func
        self.pc = 0

    def stack_set(self, position, item):
        assert position >= 0
        self.stack[position] = item

    def stack_get(self, position):
        assert position >= 0
        return self.stack[position]

    def stack_push(self, item):
        self.stack_set(self.stacktop, item)
        self.stacktop += 1

    def stack_pop(self):
        assert self.stacktop > 0
        self.stacktop -= 1
        item = self.stack_get(self.stacktop)
        self.stack_set(self.stacktop, 0)
        return item

    def local_set(self, name, value):
        assert name >= 0 and name < len(self.locals)
        self.locals[name] = value

    def local_get(self, name):
        assert name >= 0 and name < len(self.locals)
        return self.locals[name]

    def is_root_frame(self):
        return self.get_parent() is None
