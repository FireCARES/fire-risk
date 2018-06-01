from __future__ import division

import random
import numpy as np
import os
from fire_risk.utils import UniformDraw
from math import log
from math import ceil


class LowerBoundGreaterThanUpperBoundException(Exception):
    pass


class NotEnoughRecords(Exception):
    pass


class DIST(object):
    """
    The Differential In Standard Time (DIST) model.
    """

    room_area_draw = UniformDraw(72, 380)
    building_area_draw = UniformDraw(1088, 9004)
    alarm_time_draw = UniformDraw(90, 120)
    dispatch_time_draw = UniformDraw(40, 80)
    turnout_time_draw = UniformDraw(60, 100)
    arrival_time_draw = UniformDraw(300, 420)
    suppression_time_draw = UniformDraw(60, 180)
    floor_area_draw = None
    floor_extent = True
    minimum_number_of_records = 75

    @property
    def params(self):
        return ['room_area_draw', 'building_area_draw', 'alarm_time_draw', 'dispatch_time_draw', 'turnout_time_draw',
                'arrival_time_draw', 'suppression_time_draw', 'floor_area_draw', 'floor_extent',
                'minimum_number_of_records', 'object_of_origin', 'room_of_origin', 'floor_of_origin',
                'building_of_origin', 'beyond']

    def __init__(self, object_of_origin, room_of_origin, floor_of_origin, building_of_origin, beyond, **kwargs):
        """initialize attributes of the DISTOutput class.

        Args:
            extent_list(list): list of firespread extents, see DISTImport class.
            firespread_count(dict): dictionary of firespread counts by extent.
                see DISTImport class.

        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...            beyond=9, floor_extent=False)
        >>> test.object_of_origin
        93
        >>> test.room_of_origin
        283
        >>> test = DIST(object_of_origin=74, room_of_origin=0, floor_of_origin=0, building_of_origin=0,
        ...            beyond=0, floor_extent=False)
        Traceback (most recent call last):
            ...
        NotEnoughRecords

        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, room_area_draw=UniformDraw(20, 30), building_area_draw=UniformDraw(20,30),
        ...          alarm_time_draw=UniformDraw(20,30), dispatch_time_draw=UniformDraw(20,30),
        ...          turnout_time_draw=UniformDraw(20,30), arrival_time_draw=UniformDraw(20,30),
        ...          suppression_time_draw=UniformDraw(20,30), floor_extent=False)
        >>> test.room_area_draw.minimum
        20
        >>> test.room_area_draw.maximum
        30
        """

        self.object_of_origin = object_of_origin
        self.room_of_origin = room_of_origin + object_of_origin
        self.floor_of_origin = floor_of_origin
        self.building_of_origin = building_of_origin
        self.beyond = beyond

        for key, value in kwargs.items():
            if key in self.params:
                setattr(self, key, value)

        # TODO: Should raise error if self.floor_extent=True and floor_area_draw is None?
        if not self.floor_extent:
            self.building_of_origin += self.floor_of_origin
            self.floor_of_origin = 0

        if self.minimum_number_of_records:
            if (self.room_of_origin + self.floor_of_origin + self.building_of_origin +
                    self.beyond) < self.minimum_number_of_records:
                raise NotEnoughRecords

    @property
    def total_fires(self):
        """
        Returns the count of fires.

        >>> d = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, floor_extent=False)
        >>> d.total_fires
        395
        """

        return self.room_of_origin + self.floor_of_origin + self.building_of_origin + self.beyond

    @staticmethod
    def _task_time(uniform_values):
        """
        Returns the sum of time values.

        >>> random.seed(1234)
        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, room_area_draw=UniformDraw(20, 30), building_area_draw=UniformDraw(20,30),
        ...          alarm_time_draw=UniformDraw(20,30), dispatch_time_draw=UniformDraw(20,30),
        ...          turnout_time_draw=UniformDraw(20,30), arrival_time_draw=UniformDraw(20,30),
        ...          suppression_time_draw=UniformDraw(20,30), floor_extent=False)
        >>> values = test._draw_values()
        >>> round(test._task_time(values), 2)
        131.12
        >>> round(values['alarm_time'] + values['dispatch_time'] + values['turnout_time'] + values['arrival_time'] \
         + values['suppression_time'], 2)
        131.12
        """

        times = 'alarm_time dispatch_time turnout_time arrival_time suppression_time'.split()
        return sum(map(lambda value: uniform_values.get(value, 0), times))

    @staticmethod
    def draw_uniform(uniform_limits):
        """
        Draw a new value of of an attribute from its uniform distribution.

        :param uniform_limits: a tuple of of uniform limits.
        """
        return random.uniform(*uniform_limits)

    @staticmethod
    def draw_custom(draw_file_name, filter=False):
        """
        Draw a new value of an attribute from a custom distribution.

        This can be used when there is actual data for a given parameter

        """

        custom_values = []
        custom_cdf = []
        with open(os.path.join(os.path.dirname(__file__), draw_file_name), 'r') as f:
            for line in f:
                custom_values.append(float(line.split('\t')[0]))
                custom_cdf.append(float(line.split('\t')[1]))
            index_array = range(0, len(custom_values))

            lowerbound = 0
            if filter:
                lowerbound_index = int(ceil(np.interp(filter, custom_values, index_array)))
                lowerbound = custom_cdf[lowerbound_index]

            interp_value = np.random.uniform(lowerbound, 1)
            interp_index = int(ceil(np.interp(interp_value, custom_cdf, index_array)))
            return custom_values[interp_index]

    def _draw_values(self):
        """
        Draws values for room_area, building_area, alarm_time, dispatch_time, turnout_time, arrival_time,
        suppression_time and potentially the floor_area (if self.floor_extent is True).

        All values are expected to have a draw method.

        >>> random.seed(1234)
        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, room_area_draw=UniformDraw(20, 30), building_area_draw=UniformDraw(20,30),
        ...          alarm_time_draw=UniformDraw(20,30), dispatch_time_draw=UniformDraw(20,30),
        ...          turnout_time_draw=UniformDraw(20,30), arrival_time_draw=UniformDraw(20,30),
        ...          suppression_time_draw=UniformDraw(20,30), floor_extent=False)
        >>> values = test._draw_values()
        >>> round(values.get('room_area'), 2)
        29.66
        >>> round(values.get('building_area'), 2)
        24.41
        """

        values = dict(
            room_area=self.room_area_draw.draw(),
            building_area=self.building_area_draw.draw(),
            alarm_time=self.alarm_time_draw.draw(),
            dispatch_time=self.dispatch_time_draw.draw(),
            turnout_time=self.turnout_time_draw.draw(),
            arrival_time=self.arrival_time_draw.draw(),
            suppression_time=self.suppression_time_draw.draw()
        )

        if self.floor_area_draw and self.floor_extent:
            values.update(dict(floor_area=self.draw_uniform(self.floor_area_draw)))

        return values

    def gibbs_sample(self, iterations=10000, burn_in=500, thin=1, theta=7.08e-3, ao=1):
        """
        Run the gibbs sample.
        :param n_iter:
        :param burn_in:
        :param thin:
        :return:
        >>> random.seed(1234)
        >>> d = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, room_area_draw=UniformDraw(72, 380), building_area_draw=UniformDraw(1088,9004),
        ...          alarm_time_draw=UniformDraw(90, 120), dispatch_time_draw=UniformDraw(40, 80),
        ...          turnout_time_draw=UniformDraw(60, 100), arrival_time_draw=UniformDraw(300, 420),
        ...          suppression_time_draw=UniformDraw(60, 180),floor_extent=False)
        >>> d.gibbs_sample()
        13.0
        """
        # determine size of the chains necessary to hold data
        n_store = int(iterations / thin + 0.0001)
        chain_record = np.full((n_store, 4), -1000)
        params = dict(ao=ao, theta=theta)

        for i in range(iterations + burn_in):
            values = self._draw_values()
            room_area = values.get('room_area')
            building_area = values.get('building_area')
            floor_area = values.get('floor_area')
            task_time = self._task_time(values)

            dist_room = self.draw_DIST_room(room_area=room_area, task_time=task_time, **params)
            dist_beyond = self.draw_DIST_beyond(building_area=building_area, task_time=task_time, **params)
            dist_building = self.draw_DIST_building(building_area=building_area, task_time=task_time,
                                                    room_area=room_area, floor_area=floor_area,
                                                    floor_extent=self.floor_extent, **params)

            if i >= burn_in:
                for z, label in zip(range(chain_record.shape[1]), [dist_room, dist_building, dist_beyond, room_area]):
                    chain_record[i-burn_in, z] = label

        return round(self.DIST_score(chain_record[..., 0],
                                     chain_record[..., 1],
                                     chain_record[..., 2]))

    @staticmethod
    def draw_DIST_room(room_area, task_time, theta=7.08e-3, ao=1):
        """
        Draw a new value of DIST corresponding to confined to Room extent

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        >>> random.seed(1234)
        >>> round(DIST.draw_DIST_room(25.8222757306, 132.649225647), 2)
        311.16
        """
        lowerbound = ((log(ao)-log(ao))/theta)-task_time
        upperbound = ((log(room_area)-log(ao))/theta)-task_time

        if lowerbound > upperbound:
            raise LowerBoundGreaterThanUpperBoundException

        return random.uniform(lowerbound, upperbound)

    @staticmethod
    def draw_DIST_building(building_area, task_time, room_area=None, floor_area=None,
                           theta=7.08e-3, ao=1, floor_extent=False):
        """Draw a new value of DIST corresponding to confined to building extent

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        >>> random.seed(1234)
        >>> round(DIST.draw_DIST_building(25.8222757306, 132.649225647, room_area=5), 2)
        318.79
        >>> DIST.draw_DIST_building(25, 100, room_area=100)
        Traceback (most recent call last):
            ...
        LowerBoundGreaterThanUpperBoundException
        """

        if floor_extent:
            lowerbound = ((log(floor_area)-log(ao))/theta)-task_time

        else:
            lowerbound = ((log(room_area)-log(ao))/theta)-task_time

        upperbound = ((log(building_area)-log(ao))/theta)-task_time

        if lowerbound > upperbound:
            raise LowerBoundGreaterThanUpperBoundException

        return random.uniform(lowerbound, upperbound)

    @staticmethod
    def draw_DIST_beyond(building_area, task_time, theta=7.08e-3, ao=1):
        """Draw a new value of DIST corresponding to confined to Room extent

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        Note that the DIST_beyond extent functions differently. Rather than
        drawing from a uniform distribution, its value is deterministically
        calculated from the present values of the time attributes and the
        building area.

        >>> random.seed(1234)
        >>> round(DIST.draw_DIST_room(200, 100), 2)
        623.25
        """

        return ((log(building_area)-log(ao))/theta)-task_time

    @staticmethod
    def draw_sampled_room_area(list_of_room_areas):
        """Draw a new value of room area from a supplied list of room areas

        Note one should have upwards of 50-100 room areas before this starts
        to become valid.
        >>> random.seed(1234)
        >>> DIST.draw_sampled_room_area(range(1,10))
        9
        """
        return random.sample(list_of_room_areas, 1)[0]

    def draw_res_census_bldg_area(self, list_of_building_size_categories, list_of_number_of_units_in_each_category):
        """Draw a new value of residential building area from AHS data

        Data will only be available for "metropolitan statistical areas" and
        nationally.
        """
        raise NotImplementedError

    # diagnostic methods
    def traceplot(self, vector_of_drawn_values):
        """
        Generates a traceplot of a vector of drawn values.
        """
        raise NotImplementedError

    def densityplot(self, vector_of_drawn_values):
        """Plot the estimated probability density function of the drawn values.
        """
        raise NotImplementedError

    def summarystats(self, vector_of_drawn_values, list_of_quantiles=(0.025, 0.25, 0.5, 0.75, 0.975)):
        """
        Calculates and print summary statistics of given raw output.
        """
        raise NotImplementedError

    def save_raw_output(self, vector_of_drawn_values):
        """
        Saves the raw output to a temporary .csv file
        """
        raise NotImplementedError

    # Aggregate Raw output methods
    def raw_DIST_compute(self, DIST_room, DIST_bldg, DIST_beyond, DIST_floor=None):
        """Compute the raw DIST value from the raw constituent chains

        Note that inputs should be in the form of numpy vectors.
        Inputs should also, by design, be of equal length.

        Returns:
            Numpy vector of raw DIST values compiled from extent chains.

        """

        roomweight = self.room_of_origin / self.total_fires
        bldgweight = self.building_of_origin / self.total_fires
        beyondweight = self.beyond / self.total_fires

        if self.floor_extent:
            floorweight = self.floor_of_origin / self.total_fires

        raw_DIST = (roomweight * DIST_room + bldgweight*DIST_bldg +
                    beyondweight * DIST_beyond)
        if DIST_floor is not None:
            raw_DIST = raw_DIST + floorweight * DIST_floor

        return raw_DIST

    def DIST_score(self, DIST_room, DIST_bldg, DIST_beyond, DIST_floor=None):
        """Compute the single value DIST score from the raw constituent chains

        Note that inputs should be in the form of numpy vectors.

        """
        raw_DIST = self.raw_DIST_compute(DIST_room, DIST_bldg, DIST_beyond, DIST_floor)
        raw_DIST[raw_DIST < 0] = 0
        DIST_score = np.average(raw_DIST)
        return DIST_score


class DISTMediumHazard(DIST):
    """
    The Differential In Standard Time (DIST) model for the medium hazard cases.
    """

    floor_area_draw = 'Data/Med_floor_draw'
    """
    This file is a cdf based on the reported unit square footage of residential units
    with between 3 and 7 (inclusive) total floors from the American Housing Survey.
    """

    floor_num_draw = 'Data/Med_floor_num_draw'
    """
    This file is a cdf based on reported number of floors of individuals who live in
    residential units that have a total floor count between 3 and 7 (inclusive).
    """
    building_area_draw = 'Data/Med_building_draw'
    """
    This file is a cdf generated by taking the total number of units in a complex,
    dividing it by the total number of floors (predicts average units per floor), and
    then multiplying by the average square footage of a unit (about 850 square feet).
    """

    def __init__(self, object_of_origin, room_of_origin, floor_of_origin, building_of_origin, beyond,
                 **kwargs):

        """Initializing the inputs to the medium hazard DIST class.

            Most of the defaults need to be updated.
            :param number_of_floors_draw: The draw for the total number of floors in a building


        """

        super(DISTMediumHazard, self).__init__(object_of_origin, room_of_origin, floor_of_origin, building_of_origin,
                                               beyond, **kwargs)

    @staticmethod
    def _task_time(uniform_values):
        """
            Adds in the floor climb time by drawing the floor number that the fire occurs on.

            >>> random.seed(1234)
            >>> test = DISTMediumHazard(object_of_origin=93, room_of_origin=190, floor_of_origin=39,
            ...          beyond=9, room_area_draw=UniformDraw(20, 30), building_area_draw='Data/Med_building_draw',
            ...          alarm_time_draw=UniformDraw(20,30), dispatch_time_draw=UniformDraw(20,30),
            ...          turnout_time_draw=UniformDraw(20,30), arrival_time_draw=UniformDraw(20,30),
            ...          suppression_time_draw=UniformDraw(20,30), floor_extent=True, building_of_origin=64)
            >>> values = test._draw_values()
            >>> round(test._task_time(values), 2)
            159.67
            >>> floor_number = max(round(values['floor_climb'],0)- 2, 0)
            >>> round(values['alarm_time'] + values['dispatch_time'] + values['turnout_time'] + values['arrival_time'] \
             + values['suppression_time'] + .644*floor_number**2 + 30.222*floor_number, 2)
            159.67
        """
        # The climb is to the n-2 floor when the fire occurs on the nth floor.
        floor_climb = max(round(uniform_values['floor_climb'], 0) - 2, 0)

        # I integrated the regression equation to find total (rather than marginal) climb time
        climb_time = .644 * floor_climb ** 2 + 30.222 * floor_climb

        return super(DISTMediumHazard, DISTMediumHazard)._task_time(uniform_values) + climb_time

    def _draw_values(self):

        values = dict(
            room_area=self.room_area_draw.draw(),
            alarm_time=self.alarm_time_draw.draw(),
            dispatch_time=self.dispatch_time_draw.draw(),
            turnout_time=self.turnout_time_draw.draw(),
            arrival_time=self.arrival_time_draw.draw(),
            suppression_time=self.suppression_time_draw.draw()
        )

        values.update(dict(floor_climb=self.draw_uniform([1, self.draw_custom(self.floor_num_draw)])))

        """
        Drawing from custom distributions

        Note that the second (filter) argument is passed to draw_custom to prevent drawing
        a building area smaller than the floor or room areas.

        """
        if self.floor_area_draw and self.floor_extent:
            values.update(dict(floor_area=self.draw_custom(self.floor_area_draw, values['room_area'])))
            if hasattr(self.building_area_draw, 'draw'):
                values.update(dict(building_area=self.building_area_draw.draw()))
            else:
                values.update(dict(building_area=self.draw_custom(self.building_area_draw, values['floor_area'])))
        else:
            if hasattr(self.building_area_draw, 'draw'):
                values.update(dict(building_area=self.building_area_draw.draw()))
            else:
                values.update(dict(building_area=self.draw_custom(self.building_area_draw, values['room_area'])))
        return values


class DISTHighHazard(DISTMediumHazard):
    """
    The Differential In Standard Time (DIST) model for the high hazard cases.

    This is a separate subclass so that different defaults can be used.


     Adds in the floor climb time by drawing the floor number that the fire occurs on.

    >>> random.seed(1234)
    >>> np.random.seed(1234)
    >>> test = DISTHighHazard(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
    ...          beyond=9, room_area_draw=UniformDraw(20, 30), building_area_draw='Data/High_building_draw',
    ...          alarm_time_draw=UniformDraw(20,30), dispatch_time_draw=UniformDraw(20,30),
    ...          turnout_time_draw=UniformDraw(20,30), arrival_time_draw=UniformDraw(20,30),
    ...          suppression_time_draw=UniformDraw(20,30), floor_extent=True)
    >>> values = test._draw_values()
    >>> round(test._task_time(values),2)
    296.02
    >>> floor_number = max(round(values['floor_climb'],0)- 2, 0)
    >>> round(values['alarm_time'] + values['dispatch_time'] + values['turnout_time'] + values['arrival_time'] \
     + values['suppression_time'] + .644*floor_number**2 + 30.222*floor_number, 2)
    296.02
    """
    floor_area_draw = 'Data/High_floor_draw'
    """
    This file is a cdf based on the reported unit square footage of residential units
    with 8 or more total floors from the American Housing Survey.
    """
    floor_num_draw = 'Data/High_floor_num_draw'
    """
    This file is a cdf based on reported number of floors of individuals who live in
    residential units that have a total floor count greater than 8. This is capped at 21 floors.
    """
    building_area_draw = 'Data/High_building_draw'
    """
    This file is a cdf generated by taking the total number of units in a complex,
    dividing it by the total number of floors (predicts average units per floor), and
    then multiplying by the average square footage of a unit (about 850 square feet).
    """


if __name__ == "__main__":
    import doctest
    doctest.testmod()
