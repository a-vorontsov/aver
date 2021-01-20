from primitive_object import PrimitiveObject


class Float(PrimitiveObject):
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
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() + y.get_value()
        frame.stack_push(Float(float(result)))

    def sub(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() - y.get_value()
        frame.stack_push(Float(float(result)))

    def mul(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() * y.get_value()
        frame.stack_push(Float(float(result)))

    def div(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() / y.get_value()
        frame.stack_push(Float(float(result)))

    def eq(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() == y.get_value()
        return result

    def neq(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() != y.get_value()
        return result

    def lt(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() < y.get_value()
        return result

    def le(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() <= y.get_value()
        return result

    def gt(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() > y.get_value()
        return result

    def ge(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, Float) and isinstance(y, Float)
        result = x.get_value() >= y.get_value()
        return result
