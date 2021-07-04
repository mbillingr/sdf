from chapter03.adventure_game.adventure_substrate.messaging import narrate, possessive
from chapter03.adventure_game.adventure_substrate.random import flip_coin, random_choice, clipped_random_number
from .autonomous_agent import AutonomousAgent
from .person import BASE_HEALTH


class Troll(AutonomousAgent):
    def __init__(self, name, home, restlessness, hunger):
        super().__init__(name, home, restlessness, 1 / 10)
        self.hunger = hunger

    def clock_tick(self):
        super().clock_tick()
        self.eat_people()

    def eat_people(self):
        if flip_coin(self.hunger):
            people = self.people_here()
            if not people:
                narrate([possessive(self), "belly rumbles"], self)
            else:
                victim = random_choice(people)
                narrate([self, "takes a bite out of", victim], self)
                # Scale the damage of the troll bite so that there is
                # 1/3 chance that the damage is equal to a typical person's health,
                # 1/3 chance that the damage is a third of a typical person's health,
                # and 1/3 chance that it is any value in-between.
                # This is consistent with the original implementation, where people had
                # 3 health and a troll bite rolled 1, 2, or 3 with equal probability.
                min_damage = BASE_HEALTH // 3
                max_damage = BASE_HEALTH
                victim.suffer(clipped_random_number(min_damage, max_damage, 1 / 3, 1 / 3))
