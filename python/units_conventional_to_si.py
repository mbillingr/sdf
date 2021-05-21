import units


def factor_conversion(from_unit, to_unit, factor):
    return units.register_conversion(
        from_unit, to_unit,
        lambda x: x * factor,
        lambda y: y / factor)


# distance

meters_to_kilometers = factor_conversion("meters", "kilometers", 1e-3)
meters_to_centimeters = factor_conversion("meters", "centimeters", 100)
meters_to_millimeters = factor_conversion("meters", "millimeters", 1000)
miles_to_kilometers = factor_conversion("miles", "kilometers", 1.609344)

# temperature

fahrenheit_to_celsius = units.register_conversion(
    "fahrenheit", "celsius",
    lambda f: (f - 32) * 5 / 9,
    lambda c: c * 9 / 5 + 32)

celsius_to_kelvin = units.register_conversion(
    "celsius", "kelvin",
    lambda c: c + 273.15,
    lambda k: k - 273.15)

# time

minutes_to_seconds = factor_conversion("minutes", "seconds", 60)
hours_to_minutes = factor_conversion("hours", "minutes", 60)
days_to_hours = factor_conversion("days", "hours", 24)
weeks_to_days = factor_conversion("weeks", "days", 7)
years_to_days = factor_conversion("years", "days", 365)

# volume

liter_to_cubicmeter = factor_conversion("liter", "meter ** 3", 1e-3)
us_gallon_to_liter = factor_conversion("us_gallon", "liter", 3.78541)
uk_gallon_to_liter = factor_conversion("uk_gallon", "liter", 4.54609)

# speed

mps_to_kmh = units.register_conversion(
    "meters / seconds", "kilometers / hours",
    units.div(units.make_converter("meters", "kilometers"),
              units.make_converter("seconds", "hours")))

mps_to_mph = units.register_conversion(
    "meters / seconds", "miles / hours",
    units.div(units.make_converter("meters", "miles"),
              units.make_converter("seconds", "hours")))

mpss_to_mphh = units.register_conversion(
    "meters / seconds ** 2", "miles / hours ** 2",
    units.div(units.make_converter("meters", "miles"),
              units.pow(units.make_converter("seconds", "hours"), 2)))
