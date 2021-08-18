from dataclasses import dataclass
from typing import Any

from chapter05.common.environment import THE_EMPTY_ENVIRONMENT


def is_postponed(obj):
    return isinstance(obj, Postponed)


@dataclass
class Postponed:
    expression: Any
    environment: Any


def postpone(expression, environment):
    return Postponed(expression, environment)


def is_postponed_memo(obj):
    return isinstance(obj, PostponedMemo) and obj.value is None


def is_advanced_memo(obj):
    return isinstance(obj, PostponedMemo) and obj.value is not None


def postpone_memo(expression, environment):
    return PostponedMemo(expression, environment)


def advanced_value(x):
    return x.value


@dataclass
class PostponedMemo:
    expression: Any
    environment: Any
    value: Any = None

    def set_value(self, value):
        self.value = value
        self.expression = None
        self.environment = THE_EMPTY_ENVIRONMENT
        return value
