import math
from unittest.mock import Mock

import units
from units import UnitRegistry
from combinators import compose


def test_make_conversion_returns_conversion_procedure():
    registry = UnitRegistry()

    convert = Mock()
    unit_conversion = registry.make_conversion(convert, Mock())

    assert unit_conversion is convert


def test_invert_returns_associated_inversion_procedure():
    registry = UnitRegistry()

    invert = Mock()
    convert = registry.make_conversion(Mock(), invert)

    assert registry.invert(convert) is invert


def test_module_global_registry():
    invert = Mock()
    convert = units.make_conversion(Mock(), invert)
    assert units.invert(convert) is invert


def test_simple_unit_conversion_system():
    GAS_CONSTANT = 8.3144621

    def gas_law_volume(pressure, temperature, amount):
        return (amount * GAS_CONSTANT * temperature) / pressure

    def sphere_radius(volume):
        return math.pow(volume / (math.pi * 4 / 3), 1 / 3)

    unit = UnitRegistry()

    celsius_to_kelvin = unit.make_conversion(lambda c: c + 273.15,
                                             lambda k: k - 273.15)

    fahrenheit_to_celsius = unit.make_conversion(lambda f: (f - 32) * 5 / 9,
                                                 lambda c: c * 9 / 5 + 32)

    pound_to_newton = unit.make_conversion(lambda p: p/0.22480894387096,
                                           lambda n: n*0.22480894387096)

    inch_to_meter = unit.make_conversion(lambda i: i/39.37,
                                         lambda m: m*39.37)

    psi_to_nsm = compose(pound_to_newton,
                         unit.invert(inch_to_meter),
                         unit.invert(inch_to_meter))

    radius_inch = unit.invert(inch_to_meter)(
        sphere_radius(
            gas_law_volume(
                psi_to_nsm(14.7),
                compose(celsius_to_kelvin, fahrenheit_to_celsius)(68),
                1)))

    assert math.isclose(radius_inch, 7.04962, abs_tol=1e-5)
