class Symbol(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def get_value(self):
        return self.value

    def set_value(self, value):
        self.value = value
        return self


class SymbolType(object):
    INT = 0
    FLOAT = 1
    BOOL = 2
    CHAR = 3
    STRING = 4
