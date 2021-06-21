import random


def random_choice(items):
    return items and random.choice(items)


def random_bias(weight):
    return 1.0 / (random.randint(1, weight))


def flip_coin(bias):
    return random.random() >= bias
