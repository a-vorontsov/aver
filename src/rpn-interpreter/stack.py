class Stack:
    def __init__(self):
        self.items = [0] * 1024
        self.top = 0

    def isEmpty(self):
        return self.top == 0

    def push(self, item):
        self.items[self.top] = item
        self.top += 1

    def pop(self):
        if not self.isEmpty():
            item = self.items[self.top-1]
            self.items[self.top] = 0
            self.top -= 1
            return item
        else:
            return 0

    def peek(self):
        return self.items[self.top-1]

    def size(self):
        return len(self.items)
