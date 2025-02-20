from primitive_object import PrimitiveObject
import boolean

class Null(PrimitiveObject):
    def get_string(self):
        return "null"

    def pprint(self):
        print "null"

    def eq(self, rhs):
        return boolean.Boolean(isinstance(rhs, Null))

    def neq(self, rhs):
        return boolean.Boolean(not isinstance(rhs, Null))
