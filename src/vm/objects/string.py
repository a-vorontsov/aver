from primitive_object import PrimitiveObject


class String(PrimitiveObject):
    _immutable_fields_ = ["value"]

    def __init__(self, value):
        self.value = value

    def get_value(self):
        return self.value

    def get_string(self):
        return self.value

    def pprint(self, frame):
        val = frame.stack_pop()
        print val

    def add(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, String) and isinstance(y, String)
        result = x.get_value() + y.get_value()
        frame.stack_push(String(str(result)))

    def eq(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, String) and isinstance(y, String)
        result = x.get_value() == y.get_value()
        return result

    def neq(self, frame):
        y = frame.stack_pop()
        x = frame.stack_pop()
        assert isinstance(x, String) and isinstance(y, String)
        result = x.get_value() != y.get_value()
        return result
