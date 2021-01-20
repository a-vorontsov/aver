from rpython.rlib import jit


class Frame(object):
    __slots__ = ("parent", "stack", "stacktop", "func", "pc", "locals")
    _immutable_fields_ = ("parent", "stack", "func", "pc", "locals")
    _virtualizable_ = ("stack[*]", "stacktop", "locals[*]")

    def __init__(self, parent, func, local_vars, stack_size):
        self = jit.hint(self, access_directly=True, fresh_virtualizable=True)
        self.parent = parent
        self.locals = [None] * local_vars
        self.stack = [None] * stack_size
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
        print result

    def local_set(self, name, value):
        assert name >= 0 and name < len(self.locals)
        self.locals[name] = value

    def local_get(self, name):
        assert name >= 0 and name < len(self.locals)
        return self.locals[name]
