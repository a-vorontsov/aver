class Function(object):
    __slots__ = ("name", "bytecode", "num_locals", "literals", "stack_size")

    def __init__(self, name, bytecode, num_locals, literals, stack_size):
        self.name = name
        self.bytecode = bytecode
        self.num_locals = num_locals
        self.literals = literals
        self.stack_size = stack_size

    def print_func(self):
        print self.name
        for code in self.bytecode:
            print code
        print self.num_locals
        print "-------"
