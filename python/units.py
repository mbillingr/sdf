from typing import Callable


class ConversionRegistry:
    def __init__(self):
        self.inversions = {}

    def make_conversion(self, convert: Callable, invert: Callable) -> Callable:
        self.inversions[convert] = invert
        return convert

    def invert(self, convert: Callable):
        return self.inversions.get(convert)


_REGISTRY = ConversionRegistry()
make_conversion = _REGISTRY.make_conversion
invert = _REGISTRY.invert
