from primitive_object import PrimitiveObject
from boolean import Boolean
from null import Null


class Float(PrimitiveObject):
    __slots__ = ("value",)
    _immutable_fields_ = ("value",)

    def __init__(self, value):
        self.value = value

    def get_value(self):
        return self.value

    def get_string(self):
        return str(self.value)

    def pprint(self):
        print self.get_string()

    def add(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value + rhs.value
        return Float(float(result))

    def sub(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value - rhs.value
        return Float(float(result))

    def mul(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value * rhs.value
        return Float(float(result))

    def div(self, rhs):
        assert isinstance(rhs, Float)
        if rhs.value == 0.0:
            raise ValueError
        result = self.value / rhs.value
        return Float(float(result))

    def eq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, Null):
            return Boolean(False)
        else:
            assert isinstance(rhs, Float)
            result = self.value == rhs.value
            return Boolean(result)

    def neq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, Null):
            return Boolean(True)
        else:
            assert isinstance(rhs, Float)
            result = self.value != rhs.value
            return Boolean(result)

    def lt(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value < rhs.value
        return Boolean(result)

    def le(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value <= rhs.value
        return Boolean(result)

    def gt(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value > rhs.value
        return Boolean(result)

    def ge(self, rhs):
        assert isinstance(rhs, Float)
        result = self.value >= rhs.value
        return Boolean(result)
