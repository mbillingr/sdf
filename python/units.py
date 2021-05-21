from collections import deque
from typing import Callable, Optional

from combinators import compose, identity


class ConversionRegistry:
    def __init__(self):
        self.inversions = {}
        self.converters = {}

    def is_unit_converter(self, u: Callable):
        return u in self.inversions

    def register_conversion(self, from_unit: str, to_unit: str,
                            convert: Callable, invert:
            Optional[Callable] = None):
        if invert is None:
            if convert not in self.inversions:
                raise ValueError("unknown conversion function")
            invert = self.invert(convert)
        else:
            convert = self.make_conversion(convert, invert)

        self.converters.setdefault(from_unit, {})[to_unit] = convert
        self.converters.setdefault(to_unit, {})[from_unit] = invert
        return convert

    def make_converter(self, from_unit: str, to_unit: str) -> Callable:
        if from_unit == to_unit:
            return identity
        converter = self.converters.get(from_unit, {}).get(to_unit)
        converter = converter or self.find_converter(from_unit, to_unit)
        if not self.is_unit_converter(converter):
            raise ValueError(f"unknown conversion: {from_unit} -> {to_unit}")
        return converter

    def find_converter(self, from_unit: str, to_unit: str) -> Optional[
        Callable]:
        queue = deque([(from_unit,)])
        visited = set()

        # depth first search of the unit conversion graph
        while True:
            try:
                path = queue.popleft()
                unit = path[-1]
            except IndexError:
                return None

            if unit == to_unit:
                return self.build_converter_from_path(path)

            if unit not in self.converters:
                return None

            if unit in visited:
                continue

            visited.add(unit)

            for target in self.converters[unit].keys():
                queue.append(path + (target,))

    def build_converter_from_path(self, path: [str]) -> Callable:
        converters = []
        for from_unit, to_unit in zip(path, path[1:]):
            print(from_unit, to_unit)
            converters.append(self.converters.get(from_unit, {}).get(to_unit))
        return self.mul(*converters)

    def make_conversion(self, convert: Callable, invert: Callable) -> Callable:
        self.inversions[convert] = invert
        self.inversions[invert] = convert
        return convert

    def invert(self, convert: Callable):
        return self.inversions[convert]

    def mul(self, *units):
        assert all(self.is_unit_converter(u) for u in units)
        return self.make_conversion(compose(*units[::-1]),
                                    compose(*(self.invert(u) for u in units)))

    def div(self, u1, u2):
        assert self.is_unit_converter(u1)
        assert self.is_unit_converter(u2)
        return self.make_conversion(compose(self.invert(u2), u1),
                                    compose(self.invert(u1), u2))

    def pow(self, unit, n):
        assert self.is_unit_converter(unit)
        return self.make_conversion(compose(*[unit] * n),
                                    compose(*[self.invert(unit)] * n))

    def specializer(self, procedure, output, **implicit_input_units):
        implicit_output_unit = output

        def specializer(output, **specific_input_units):
            specific_output_unit = output

            output_converter = self.make_converter(implicit_output_unit,
                                                   specific_output_unit)

            input_converters = {k: self.make_converter(specific_input_units[k],
                                                       implicit_input_units[k])
                                for k in specific_input_units.keys()}

            def specialized_procedure(**kwargs):
                output = procedure(**{k: input_converters[k](arg)
                                      for k, arg in kwargs.items()})
                return output_converter(output)

            return specialized_procedure

        return specializer

    def derive_converter(self, from_expr: 'UnitExpression',
                         to_expr: 'UnitExpression'):
        # Exercise 2.11f asked to derive compound expressions of units.
        # In the Python implementation I represent units as strings, so
        # extracting compound expressions would require string parsing
        # with operator precedence. Instead, I decided to add another
        # layer of abstraction and use Python's operator precedence
        # parser: unit names are wrapped in a `Unit` type, which
        # supports arithmetic operations that result in `UnitExpression`
        # objects. These expression objects in turn wrap the unit
        # arithmetic functions introduced in the book.
        assert type(from_expr) == type(to_expr)
        return from_expr.convert_into(to_expr, self)


_REGISTRY = ConversionRegistry()
derive_converter = _REGISTRY.derive_converter
div = _REGISTRY.div
invert = _REGISTRY.invert
make_converter = _REGISTRY.make_converter
make_conversion = _REGISTRY.make_conversion
mul = _REGISTRY.mul
pow = _REGISTRY.pow
register_conversion = _REGISTRY.register_conversion
specializer = _REGISTRY.specializer


def units(*args):
    unit_objects = []
    for names in args:
        for name in names.split(","):
            obj = Unit(name.strip())
            unit_objects.append(obj)
    return unit_objects


class UnitExpression:
    def convert_into(self, other, registry):
        raise NotImplementedError(f"subclass responsibility {self}")

    def __mul__(self, other):
        return UnitMultiplication(self, other)

    def __truediv__(self, other):
        return UnitDivision(self, other)

    def __pow__(self, power, modulo=None):
        assert modulo is None
        return UnitPower(self, power)


class Unit(UnitExpression):
    def __init__(self, name: str):
        self.name = name

    def convert_into(self, other, registry):
        return registry.make_converter(self.name, other.name)


class UnitDivision(UnitExpression):
    def __init__(self, nom, denom):
        self.nom = nom
        self.denom = denom

    def convert_into(self, other, registry):
        return registry.div(self.nom.convert_into(other.nom, registry),
                            self.denom.convert_into(other.denom, registry))


class UnitMultiplication(UnitExpression):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def convert_into(self, other, registry):
        return registry.mul(self.a.convert_into(other.a, registry),
                            self.b.convert_into(other.b, registry))


class UnitPower(UnitExpression):
    def __init__(self, u, n):
        self.u = u
        self.n = n

    def convert_into(self, other, registry):
        return registry.pow(self.u.convert_into(other.u, registry), self.n)
