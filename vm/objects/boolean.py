from primitive_object import PrimitiveObject
import null


class Boolean(PrimitiveObject):
    __slots__ = ("value",)
    _immutable_fields_ = ("value",)

    def __init__(self, value):
        self.value = value

    def get_value(self):
        return self.value

    def get_string(self):
        return "true" if self.value else "false"

    def pprint(self):
        print self.get_string()

    def eq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, null.Null):
            return Boolean(False)
        else:
            assert isinstance(rhs, Boolean)
            result = self.value == rhs.value
            return Boolean(result)

    def neq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, null.Null):
            return Boolean(True)
        else:
            assert isinstance(rhs, Boolean)
            result = self.value != rhs.value
            return Boolean(result)

    def cmpand(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, null.Null):
            return Boolean(True)
        else:
            assert isinstance(rhs, Boolean)
            result = self.value and rhs.value
            return Boolean(result)

    def cmpor(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, null.Null):
            return Boolean(True)
        else:
            assert isinstance(rhs, Boolean)
            result = self.value or rhs.value
            return Boolean(result)
