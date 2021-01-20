from primitive_object import PrimitiveObject
from boolean import Boolean


class Integer(PrimitiveObject):
    _immutable_fields_ = ["value"]

    def __init__(self, value):
        self.value = value

    def get_value(self):
        return self.value

    def get_string(self):
        return str(self.value)

    def pprint(self, frame):
        val = frame.stack_pop()
        print val

    def add(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() + y.get_value()
        frame.stack_push(Integer(int(result)))

    def sub(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() - y.get_value()
        frame.stack_push(Integer(int(result)))

    def mul(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() * y.get_value()
        frame.stack_push(Integer(int(result)))

    def div(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        if y.get_value() == 0:
            assert False
        result = x.get_value() / y.get_value()
        frame.stack_push(Integer(int(result)))

    def mod(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() % y.get_value()
        frame.stack_push(Integer(int(result)))

    def eq(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() == y.get_value()
        return result

    def neq(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() != y.get_value()
        return result

    def lt(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() < y.get_value()
        return result

    def le(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() <= y.get_value()
        return result

    def gt(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() > y.get_value()
        return result

    def ge(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Integer) and isinstance(y, Integer)
        result = x.get_value() >= y.get_value()
        return result
