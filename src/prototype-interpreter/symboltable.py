class SymbolTable(object):
    def __init__(self, size=100):
        self.table = [None] * size

    def set_symbol(self, position, symbol):
        self.table[position] = symbol

    def get_symbol(self, position):
        return self.table[position]

    def print_table(self):
        s = ""
        for i in range(len(self.table)):
            symbol = self.table[i]
            if symbol is not None:
                s = s + ("|%s, %s|") % (str(i), str(symbol.get_value()))
        print s


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
    FUNCTION = 1
