
class Object:
    def __init__(self, name: str):
        super().__init__()
        self.name: str = name
        self.description: str = name

    def __repr__(self):
        return self.name or self.__class__.__name__

    def clock_tick(self):
        pass


def find_object_by_name(name, objects):
    for obj in objects:
        if obj.name == name:
            return obj
    return None
