class PrimitiveObject(object):
    __slots__ = ("value",)
    _immutable_fields_ = ("value",)

    def pprint(self):
        raise NotImplementedError()

    def add(self, rhs):
        raise NotImplementedError()

    def sub(self, rhs):
        raise NotImplementedError()

    def mul(self, rhs):
        raise NotImplementedError()

    def div(self, rhs):
        raise NotImplementedError()

    def mod(self, rhs):
        raise NotImplementedError()

    def eq(self, rhs):
        raise NotImplementedError()

    def eq(self, rhs):
        raise NotImplementedError()

    def neq(self, rhs):
        raise NotImplementedError()

    def lt(self, rhs):
        raise NotImplementedError()

    def gt(self, rhs):
        raise NotImplementedError()
