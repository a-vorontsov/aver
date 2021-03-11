from primitive_object import PrimitiveObject
from null import Null
from rpython.rlib.debug import make_sure_not_resized


class Array(PrimitiveObject):
    __slots__ = ("list", "size")
    _immutable_fields_ = ("list", "size")

    def __init__(self, initial_size):
        self.size = initial_size
        self.list = [None] * initial_size
        make_sure_not_resized(self.list)

    def get_value_at(self, index):
        assert index >= 0 and index < self.size
        return self.list[index]

    def set_value_at(self, index, value):
        assert isinstance(value, PrimitiveObject)
        assert index >= 0 and index < self.size
        self.list[index] = value

    def set_list(self, new_list):
        assert len(new_list) == self.size
        self.list = new_list

    def get_string(self):
        result = "[" + ",".join([item.get_string()
                                 for item in self.list]) + "]"

        return result

    def pprint(self):
        print self.get_string()

    def add(self, rhs):
        assert isinstance(rhs, Array)
        result = self.list + rhs.list
        tmp = Array(len(self.list) + len(rhs.list))
        tmp.set_list(result)
        return tmp

    def eq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, Null):
            return False
        else:
            assert isinstance(rhs, Array)
            result = self.list == rhs.list
            return result

    def neq(self, rhs):
        assert isinstance(rhs, PrimitiveObject)
        if isinstance(rhs, Null):
            return True
        else:
            assert isinstance(rhs, Array)
            result = self.list != rhs.list
            return result
