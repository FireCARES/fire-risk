import random


class DrawType(object):
    """
    Implements a base DrawType object.
    """
    def draw(self):
        raise NotImplementedError


class UniformDraw(DrawType):
    """
    Implements a uniform draw.
    >>> random.seed(1234)
    >>> ud = UniformDraw(72, 380)
    >>> ud.draw()
    369.66768899317873
    """
    def __init__(self, minimum, maximum):
        self.minimum = minimum
        self.maximum = maximum

    def draw(self):
        return random.uniform(self.minimum, self.maximum)
