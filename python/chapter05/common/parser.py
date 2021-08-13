import re

from chapter05.common import symbols as S
from chapter05.common.primitive_types import symbol

TOKENIZE = re.compile(r'(\'|\(|\)|\s+|".*?")')


def read(raw_string=None):
    raw_string = input() if raw_string is None else raw_string
    tokens = TOKENIZE.split(raw_string)
    tokens = [
        token for token in tokens if token and not (token.isspace() or token == "\n")
    ]
    yield from Parser(tokens).parse()


class Parser:
    def __init__(self, tokens):
        self.tokens = iter(tokens)
        self.current_token = next(self.tokens)

    def advance(self):
        self.current_token = next(self.tokens)

    def parse(self):
        while True:
            yield self.parse_item()
            try:
                self.advance()
            except StopIteration:
                return

    def parse_item(self):
        token = self.current_token

        if token == ")":
            raise ValueError(f"unexpected token {token}")
        if token == "(":
            return tuple(self.parse_list())

        if token == "'":
            self.advance()
            return S.QUOTE, self.parse_item()

        if token.startswith('"') and token.endswith('"'):
            return token[1:-1]

        try:
            return int(token)
        except ValueError:
            pass

        try:
            return float(token)
        except ValueError:
            pass

        return symbol(token)

    def parse_list(self):
        end_delimiter = {"(": ")", "[": "]", "{": "}", "<": ">"}[self.current_token]
        self.advance()
        items = []
        while True:
            if self.current_token == end_delimiter:
                return items
            items.append(self.parse_item())
            self.advance()
