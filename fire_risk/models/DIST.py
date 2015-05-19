from __future__ import division

import random
import numpy as np

from math import log


class LowerBoundGreaterThanUpperBoundException(Exception):
    pass


class DIST(object):
    """
    The Differential In Standard Time (DIST) model.
    """
    def __init__(self, object_of_origin, room_of_origin, floor_of_origin, building_of_origin, beyond,
                 room_area_uniform_limits=(72, 380), building_area_uniform_limits=(1088, 9004),
                 alarm_time_uniform_limits=(90, 120), dispatch_time_uniform_limits=(40, 80),
                 turnout_time_uniform_limits=(60, 100), arrival_time_uniform_limits=(300, 420),
                 suppression_time_uniform_limits=(60, 180), floor_area_uniform_limits=None,
                 floor_extent=False):
        """initialize attributes of the DISTOutput class.

        Args:
            extent_list(list): list of firespread extents, see DISTImport class.
            firespread_count(dict): dictionary of firespread counts by extent.
                see DISTImport class.

        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...            beyond=9, room_area_uniform_limits=(72, 380), building_area_uniform_limits=(1088, 9004),
        ...             floor_extent=False)
        >>> test.object_of_origin
        93
        >>> test.room_of_origin
        283
        """
        self.object_of_origin = object_of_origin
        self.room_of_origin = room_of_origin + object_of_origin
        self.floor_of_origin = floor_of_origin
        self.building_of_origin = building_of_origin
        self.beyond = beyond
        self.floor_extent = floor_extent
        self.room_area_uniform_limits = room_area_uniform_limits
        self.building_area_uniform_limits = building_area_uniform_limits
        self.alarm_time_uniform_limits = alarm_time_uniform_limits
        self.dispatch_time_uniform_limits = dispatch_time_uniform_limits
        self.turnout_time_uniform_limits = turnout_time_uniform_limits
        self.arrival_time_uniform_limits = arrival_time_uniform_limits
        self.suppression_time_uniform_limits = suppression_time_uniform_limits
        self.floor_area_uniform_limits = floor_area_uniform_limits

        # TODO: Should raise error if self.floor_extent=True and floor_area_uniform_limits is None?
        if not self.floor_extent:
            self.building_of_origin += self.floor_of_origin

    @property
    def total_fires(self):
        """
        Returns the count of fires.

        >>> d = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, floor_extent=False)
        >>> d.total_fires
        434
        """

        return self.room_of_origin + self.floor_of_origin + self.building_of_origin + self.beyond

    @staticmethod
    def _task_time(uniform_values):
        """
        Returns the sum of time values.

        >>> random.seed(1234)
        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, room_area_uniform_limits=(20, 30), building_area_uniform_limits=(20,30),
        ...          alarm_time_uniform_limits=(20,30), dispatch_time_uniform_limits=(20,30),
        ...          turnout_time_uniform_limits=(20,30), arrival_time_uniform_limits=(20,30),
        ...          suppression_time_uniform_limits=(20,30), floor_extent=False)
        >>> values = test._draw_uniform_values()
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

    def _draw_uniform_values(self):
        """
        Draws uniform values for room_area, building_area, alarm_time, dispatch_time, turnout_time, arrival_time,
        suppression_time and potentially the floor_area (if self.floor_extent is True).

        >>> random.seed(1234)
        >>> test = DIST(object_of_origin=93, room_of_origin=190, floor_of_origin=39, building_of_origin=64,
        ...          beyond=9, room_area_uniform_limits=(20, 30), building_area_uniform_limits=(20,30),
        ...          alarm_time_uniform_limits=(20,30), dispatch_time_uniform_limits=(20,30),
        ...          turnout_time_uniform_limits=(20,30), arrival_time_uniform_limits=(20,30),
        ...          suppression_time_uniform_limits=(20,30), floor_extent=False)
        >>> values = test._draw_uniform_values()
        >>> round(values.get('room_area'), 2)
        29.66
        >>> round(values.get('building_area'), 2)
        24.41
        """

        values = dict(
            room_area=self.draw_uniform(self.room_area_uniform_limits),
            building_area=self.draw_uniform(self.building_area_uniform_limits),
            alarm_time=self.draw_uniform(self.alarm_time_uniform_limits),
            dispatch_time=self.draw_uniform(self.dispatch_time_uniform_limits),
            turnout_time=self.draw_uniform(self.turnout_time_uniform_limits),
            arrival_time=self.draw_uniform(self.arrival_time_uniform_limits),
            suppression_time=self.draw_uniform(self.suppression_time_uniform_limits)
        )

        if self.floor_area_uniform_limits and self.floor_extent:
            values.update(dict(floor_area=self.draw_uniform(self.floor_area_uniform_limits)))

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
        ...          beyond=9, room_area_uniform_limits=(72, 380), building_area_uniform_limits=(1088,9004),
        ...          alarm_time_uniform_limits=(90, 120), dispatch_time_uniform_limits=(40, 80),
        ...          turnout_time_uniform_limits=(60, 100), arrival_time_uniform_limits=(300, 420),
        ...          suppression_time_uniform_limits=(60, 180),floor_extent=False)
        >>> d.gibbs_sample()
        12.0
        """
        # determine size of the chains necessary to hold data
        n_store = int(iterations / thin + 0.0001)
        chain_record = np.full((n_store, 4), -1000)
        params = dict(ao=ao, theta=theta)

        for i in range(iterations + burn_in):
            uniform_values = self._draw_uniform_values()
            room_area = uniform_values.get('room_area')
            building_area = uniform_values.get('building_area')
            floor_area = uniform_values.get('floor_area')
            task_time = self._task_time(uniform_values)

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


if __name__ == "__main__":
    import doctest
    doctest.testmod()
