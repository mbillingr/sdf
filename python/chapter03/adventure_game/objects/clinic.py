from .place import Place
from chapter03.adventure_game import world
from chapter03.adventure_game.adventure_substrate.random import random_choice
from chapter03.adventure_game.adventure_substrate.messaging import narrate


HEAL_AMOUNT = 42


class Clinic(Place):
    def __init__(self, name: str):
        super().__init__(name)
        world.the_clock.register(self)

    def clock_tick(self):
        super().clock_tick()
        injured_people = [person for person in self.people_in_place() if person.is_injured()]
        if injured_people:
            treated_person = random_choice(injured_people)
            treated_person.heal(HEAL_AMOUNT)
            narrate([treated_person, "receives medical treatment"], self)
