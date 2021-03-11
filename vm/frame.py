from rpython.rlib.debug import make_sure_not_resized
from rpython.rlib import jit
from objects.primitive_object import PrimitiveObject


class Frame(object):
    __slots__ = ("parent", "stack", "stacktop",
                 "func", "pc", "locals", "literals")
    _immutable_fields_ = ("parent", "stack", "func",
                          "pc", "locals", "literals")
    _virtualizable_ = ("stack[*]", "stacktop", "locals[*]", "literals[*]")

    @jit.unroll_safe
    def __init__(self, parent, func, local_vars, literal_vars, stack_size):
        self = jit.hint(self, access_directly=True, fresh_virtualizable=True)
        self.parent = parent
        self.locals = [None] * (local_vars + 1)
        make_sure_not_resized(self.locals)

        self.stack = [None] * (stack_size + 1)
        make_sure_not_resized(self.stack)

        self.literals = [None] * (len(literal_vars) + 1)
        make_sure_not_resized(self.literals)
        for i in range(len(literal_vars)):
            self.literals[i] = literal_vars[i]

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
        self.stack_set(self.stacktop, None)
        return item

    def stack_peek(self):
        assert self.stacktop > 0
        item = self.stack_get(self.stacktop-1)
        return item

    def stack_print(self):
        result = ""
        for item in self.stack:
            if item is not None:
                result += "| %s |" % (item.get_string())
            else:
                result += "| None |"
        print result

    def local_set(self, name, value):
        assert name >= 0 and name < len(self.locals)
        self.locals[name] = value

    def local_print(self):
        result = ""
        for item in self.locals:
            if item is not None:
                result += "{ %s }" % (item.get_string())
            else:
                result += "{ None }"
        print result

    def local_get(self, name):
        assert name >= 0 and name < len(self.locals)
        return self.locals[name]

    def literal_get(self, index):
        assert index >= 0 and index < len(self.literals)
        return self.literals[index]

    def literal_set(self, index, value):
        assert index >= 0 and index < len(self.literals)
        self.literals[index] = value

    def literal_print(self):
        result = ""
        for item in self.literals:
            if item is not None:
                result += "[ %s ]" % (item.get_string())
            else:
                result += "[ None ]"
        print result
