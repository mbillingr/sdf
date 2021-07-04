import random


def random_choice(items):
    return items and random.choice(items)


def random_bias(weight):
    return 1.0 / (random.randint(1, weight))


def flip_coin(bias):
    return random.random() >= bias


def random_number(min_or_max, max_if_min=None):
    if max_if_min is None:
        min = 1
        max = min_or_max
    else:
        min = min_or_max
        max = max_if_min
    return random.randint(min, max)


def clipped_random_number(min, max, min_chance, max_chance):
    min_cdf = min_chance
    max_cdf = 1 - max_chance

    a = (max / max_cdf - min / min_cdf) / ((1 - 1 / min_cdf) - (1 - 1 / max_cdf))
    a = round(a)

    b = min / min_cdf + (1 - 1 / min_cdf) * a
    b = round(b)

    x = random.randint(a, b)

    if x < min:
        return min

    if x > max:
        return max

    return x
