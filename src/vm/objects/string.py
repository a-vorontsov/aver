from primitive_object import PrimitiveObject


class String(PrimitiveObject):
    __slots__ = ("value",)
    _immutable_fields_ = ("value",)

    def __init__(self, value):
        self.value = value

    def get_value(self):
        return self.value

    def get_string(self):
        return self.value

    def pprint(self):
        print self.get_string()

    def add(self, rhs):
        assert isinstance(rhs, String)
        result = self.value + rhs.value
        return String(str(result))

    def eq(self, rhs):
        assert isinstance(rhs, String)
        result = self.value == rhs.value
        return result

    def neq(self, rhs):
        assert isinstance(rhs, String)
        result = self.value != rhs.value
        return result
