from primitive_object import PrimitiveObject


class Null(PrimitiveObject):
    def get_string(self):
        return "null"

    def pprint(self):
        print "null"

    def eq(self, rhs):
        return isinstance(rhs, Null)

    def neq(self, rhs):
        return not isinstance(rhs, Null)
