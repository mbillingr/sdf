
class Trie:
    def __init__(self):
        self.edges = {}
        self._value = ()

    def add_edge(self, predicate):
        assert predicate not in self.edges
        new_node = Trie()
        self.edges[predicate] = new_node
        return new_node

    def intern_path(self, path):
        current_node = self
        for predicate in path:
            if predicate in current_node.edges:
                current_node = current_node.edges[predicate]
            else:
                current_node = current_node.add_edge(predicate)
        return current_node

    def has_value(self):
        return bool(self._value)

    @property
    def value(self):
        return self._value[0]

    @value.setter
    def value(self, value):
        self._value = (value,)

    def set_path_value(self, path, value):
        current_node = self.intern_path(path)
        current_node.value = value
        return current_node

    def get_matching_tries(self, sequence):
        if not sequence:
            return [self]

        matches = []
        for predicate, next_node in self.edges.items():
            if predicate(sequence[0]):
                matches.extend(next_node.get_matching_tries(sequence[1:]))

        if not matches:
            raise KeyError(sequence[0])
        return matches

    def get_a_value(self, sequence):
        for node in self.get_matching_tries(sequence):
            if node.has_value():
                return node.value
        raise ValueError("no node has a value")

    def get_all_values(self, sequence):
        return [node.value for node in self.get_matching_tries(sequence) if node.has_value()]
