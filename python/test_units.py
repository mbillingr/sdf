import math
from unittest.mock import Mock

import units
from combinators import compose
from units import ConversionRegistry


def test_make_conversion_returns_conversion_procedure():
    registry = ConversionRegistry()

    convert = Mock()
    unit_conversion = registry.make_conversion(convert, Mock())

    assert unit_conversion is convert


def test_invert_returns_associated_inversion_procedure():
    registry = ConversionRegistry()

    invert = Mock()
    convert = registry.make_conversion(Mock(), invert)

    assert registry.invert(convert) is invert


def test_module_global_registry():
    invert = Mock()
    convert = units.make_conversion(Mock(), invert)
    assert units.invert(convert) is invert


def test_simple_unit_conversion_system():
    registry = initialize_unit_registry()
    unit = registry['unit']
    inch_to_meter = registry['inch_to_meter']
    psi_to_nsm = registry['psi_to_nsm']
    celsius_to_kelvin = registry['celsius_to_kelvin']
    fahrenheit_to_celsius = registry['fahrenheit_to_celsius']

    radius_inch = unit.invert(inch_to_meter)(
        sphere_radius(
            gas_law_volume(
                psi_to_nsm(14.7),
                compose(celsius_to_kelvin, fahrenheit_to_celsius)(68),
                1)))

    assert math.isclose(radius_inch, 7.04962, abs_tol=1e-5)


def test_unit_specialization():
    registry = initialize_unit_registry()
    unit = registry['unit']
    inch_to_meter = registry['inch_to_meter']
    pound_to_newton = registry['pound_to_newton']
    celsius_to_kelvin = registry['celsius_to_kelvin']
    fahrenheit_to_celsius = registry['fahrenheit_to_celsius']

    unit.register_conversion(
        "fahrenheit", "celsius",
        fahrenheit_to_celsius)

    unit.register_conversion(
        "celsius", "kelvin",
        celsius_to_kelvin)

    unit.register_conversion(
        "fahrenheit", "kelvin",
        unit.chain(fahrenheit_to_celsius, celsius_to_kelvin))

    unit.register_conversion(
        "pound / inch ** 2",
        "newton / meter ** 2",
        unit.div(pound_to_newton, unit.pow(inch_to_meter, 2)))

    unit.register_conversion(
        "inch ** 3", "meter ** 3",
        unit.pow(inch_to_meter, 3))

    make_specialized_gas_law_volume = units.specializer(
        gas_law_volume,
        output="meter ** 3",
        pressure="newton / meter ** 2",
        temperature="kelvin",
        amount="mole")

    conventional_gas_law_volume = make_specialized_gas_law_volume(
        output="inch ** 3",
        pressure="pound / inch ** 2",
        temperature="fahrenheit",
        amount="mole")

    result = sphere_radius(conventional_gas_law_volume(14.7, 68, 1))
    assert math.isclose(result, 7.04962, abs_tol=1e-5)


def initialize_unit_registry():
    unit = ConversionRegistry()

    celsius_to_kelvin = unit.make_conversion(lambda c: c + 273.15,
                                             lambda k: k - 273.15)

    fahrenheit_to_celsius = unit.make_conversion(lambda f: (f - 32) * 5 / 9,
                                                 lambda c: c * 9 / 5 + 32)

    pound_to_newton = unit.make_conversion(lambda p: p / 0.22480894387096,
                                           lambda n: n * 0.22480894387096)

    inch_to_meter = unit.make_conversion(lambda i: i / 39.37,
                                         lambda m: m * 39.37)

    psi_to_nsm = compose(pound_to_newton,
                         unit.invert(inch_to_meter),
                         unit.invert(inch_to_meter))

    return locals()


def gas_law_volume(pressure, temperature, amount):
    return (amount * GAS_CONSTANT * temperature) / pressure


def sphere_radius(volume):
    return math.pow(volume / (math.pi * 4 / 3), 1 / 3)


GAS_CONSTANT = 8.3144621
