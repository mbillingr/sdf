import math

from chapter02 import units
from chapter02 import units_conventional_to_si as csi


def test_fahrenheit_to_celsius():
    assert math.isclose(csi.fahrenheit_to_celsius(-40), -40)
    assert math.isclose(units.invert(csi.fahrenheit_to_celsius)(-40), -40)

    assert math.isclose(csi.fahrenheit_to_celsius(0), -17.77777777777778)
    assert math.isclose(csi.fahrenheit_to_celsius(32), 0)
    assert math.isclose(csi.fahrenheit_to_celsius(100), 37.77777777777778)
    assert math.isclose(units.invert(csi.fahrenheit_to_celsius)(100), 212)


def test_celsius_to_kelvin():
    assert math.isclose(csi.celsius_to_kelvin(0), 273.15)
    assert math.isclose(csi.celsius_to_kelvin(100), 373.15)
    assert math.isclose(units.invert(csi.celsius_to_kelvin)(0), -273.15)
    assert math.isclose(units.invert(csi.celsius_to_kelvin)(100), -173.15)


def test_mps_to_kmh():
    assert math.isclose(csi.mps_to_kmh(1), 3.6)


def test_mps_to_mph():
    meters, seconds, miles, hours = units.units(
        "meters, seconds, miles, hours")
    converter = units.derive_converter(meters / seconds, miles / hours)
    assert math.isclose(converter(1), 2.23694, rel_tol=1e-5)


def test_division_conversion():
    meters, seconds, miles, hours = units.units(
        "meters, seconds, miles, hours")
    converter = units.derive_converter(meters / seconds, miles / hours)
    assert math.isclose(converter(1), 2.23694, rel_tol=1e-5)


def test_product_conversion():
    millimeters, centimeters = units.units("millimeters, centimeters")
    converter = units.derive_converter(centimeters * centimeters,
                                       millimeters * millimeters)
    assert math.isclose(converter(1), 100)


def test_power_conversion():
    millimeters, centimeters = units.units("millimeters, centimeters")
    converter = units.derive_converter(centimeters ** 3, millimeters ** 3)
    assert math.isclose(converter(1), 1000)
