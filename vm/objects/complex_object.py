from primitive_object import PrimitiveObject
from boolean import Boolean
from null import Null
from rpython.rlib.debug import make_sure_not_resized


class ComplexObject(PrimitiveObject):
    __slots__ = ("fields", "size")
    _immutable_fields_ = ("fields", "size")

    def __init__(self, initial_size):
        self.size = initial_size
        self.fields = [Null()] * initial_size
        make_sure_not_resized(self.fields)

    def get_value_at(self, index):
        assert index >= 0 and index < self.size
        return self.fields[index]

    def set_value_at(self, index, value):
        assert isinstance(value, PrimitiveObject)
        assert index >= 0 and index < self.size
        self.fields[index] = value

    def get_string(self):
        result = "{" + ",".join([item.get_string() if item is not None else "None"
                                 for item in self.fields]) + "}"

        return result

    def pprint(self):
        print self.get_string()

    def eq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, Null):
            return Boolean(False)
        else:
            assert isinstance(rhs, ComplexObject)
            result = self.fields == rhs.fields
            return Boolean(result)

    def neq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, Null):
            return Boolean(True)
        else:
            assert isinstance(rhs, ComplexObject)
            result = self.fields != rhs.fields
            return Boolean(result)
