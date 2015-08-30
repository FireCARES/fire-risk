import random
from scipy.stats import lognorm


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


class RandomSampleDraw(DrawType):
    """
    Implements a random sample draw.
    >>> random.seed(1234)
    >>> ud = RandomSampleDraw(range(0,100))
    >>> print ud.draw()
    96
    """
    def __init__(self, population, multiplier=1):
        self.population = population
        self.multiplier = multiplier

    def draw(self):
        return random.choice(self.population) * self.multiplier


class LogNormalDraw(DrawType):
    """
    Implements a log normal draw.
    >>> import numpy as np
    >>> np.random.seed(1234)
    >>> ud = LogNormalDraw(0.3381962232249362, -1.0844073333047395, 3.1682731892016429)
    >>> ud.draw()
    2.631505249260421
    """
    def __init__(self, shape, location, scale, multiplier=1):
        self.shape = shape
        self.location = location
        self.scale = scale
        self.multiplier = multiplier

    def draw(self):
        return lognorm.rvs(self.shape, self.location, self.scale) * self.multiplier
