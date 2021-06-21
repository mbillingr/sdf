
class Object:
    def __init__(self, name: str):
        super().__init__()
        self.name: str = name
        self.description: str = name

    def __repr__(self):
        return self.name or self.__class__.__name__
