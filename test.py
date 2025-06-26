class A:
    def __init__(self, x: "B"):
        self.x = x

class B:
    def __init__(self, y: A):
        self.y = y