from abc import ABC

class AbstractFilter(ABC):
    """"""

    @abstractmethod
    def apply(self, target):
        raise NotImplemented("")

    def __add__(self, other: AbstractFilter):
        return FilterCombinator.combine(self, other)


class DummyFilter(AbstractFilter):
    """"""
    target_type = object

    def __init__(self, target_type=object):
        self.target_type = target_type
        
    def apply(self, target: target_type):
        return True


class FilterCombinator:

    def combine(*args):
        return True
