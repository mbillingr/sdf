from typing import Callable

from combinators import compose, identity


class ConversionRegistry:
    def __init__(self):
        self.inversions = {}
        self.converters = {}

    def register_conversion(self, from_unit: str, to_unit: str,
                            convert: Callable):
        if convert not in self.inversions:
            raise ValueError("unknown conversion function")
        self.converters[(from_unit, to_unit)] = convert
        self.converters[(to_unit, from_unit)] = self.invert(convert)

    def make_converter(self, from_unit: str, to_unit: str) -> Callable:
        if from_unit == to_unit:
            return identity
        return self.converters[(from_unit, to_unit)]

    def make_conversion(self, convert: Callable, invert: Callable) -> Callable:
        self.inversions[convert] = invert
        return convert

    def invert(self, convert: Callable):
        return self.inversions.get(convert)

    def mul(self, *units):
        return self.make_conversion(compose(*units[::-1]),
                                    compose(*(self.invert(u) for u in units)))

    def div(self, u1, u2):
        return self.make_conversion(compose(self.invert(u2), u1),
                                    compose(self.invert(u1), u2))

    def pow(self, unit, n):
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


_REGISTRY = ConversionRegistry()
make_conversion = _REGISTRY.make_conversion
invert = _REGISTRY.invert
