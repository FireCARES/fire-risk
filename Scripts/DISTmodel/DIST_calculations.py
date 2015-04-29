from __future__ import division
import random
from math import log

class DISTCalculate(object):
    """Contains the calculation methods for the DIST model.

    The DISTCalculate class contains the calculation methods for the DIST model.
    Additionally, it possesses attributes tracking present values of random
    draws for Gibbs sampling.

    The idea behind the class is that it will store the calculation methods
    for the distributions, as well as the random draw methods, and the present
    state of the variables being used in Gibbs sampling.

    Attributes:
        alarm_time(float): current value of alarm response time (s)
        dispatch_time(float): current value of dispatch time (s)
        turnout_time(float): current value of turnout time (s)
        arrival_time(float): current value of arrival time (s)
        suppression_time(float): current value of suppression time (s)
        room_area(float): current value of room_area (sq. ft.)
        building_area(float): current value of building_area (sq. ft.)
        theta(float): value of theta as used by the model
        ao(float): value of ao as used by the model. This defaults to 1 sq. ft.
        floor_extent(boolean): indicate whether the floor firespread extent is
            included in the analysis
        DIST_


    Additional attributes are invoked as necessary in class methods
    """
    def __init__(self, theta=7.08e-3, ao=1,floor_extent=False):
        """Initialize the DISTCalculate class

        Note that the variables set to None in the function need to be drawn
        before they assume their proper float value as described in the class
        docstring.
        """
        self.alarm_time = None
        self.dispatch_time = None
        self.turnout_time = None
        self.arrival_time = None
        self.suppression_time = None
        self.room_area = None
        self.bldg_area = None
        self.ao = ao
        self.theta = theta
        self.floor_extent = floor_extent
        self.DIST_room = None
        self.DIST_bldg = None 
        self.DIST_beyond = None 
        if floor_extent:
            self.DIST_floor = None
            self.floor_area = None
    
    #Time drawing methods

    def draw_uniform_alarm_time(self,lowerbound,upperbound):
        """Draw a new value of alarm time from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_alarm_time(20,30)
        >>> print round(test.alarm_time,2)
        29.66

        """
        self.alarm_time = random.uniform(lowerbound,upperbound)

    def draw_uniform_dispatch_time(self,lowerbound,upperbound):
        """Draw a new value of dispatch time from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_dispatch_time(20,30)
        >>> print round(test.dispatch_time,2)
        29.66

        """
        self.dispatch_time = random.uniform(lowerbound,upperbound)

    def draw_uniform_turnout_time(self,lowerbound,upperbound):
        """Draw a new value of turnout time from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_turnout_time(20,30)
        >>> print round(test.turnout_time,2)
        29.66

        """
        self.turnout_time = random.uniform(lowerbound,upperbound)

    def draw_uniform_arrival_time(self,lowerbound,upperbound):
        """Draw a new value of arrival time from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_arrival_time(20,30)
        >>> print round(test.arrival_time,2)
        29.66

        """
        self.arrival_time = random.uniform(lowerbound,upperbound)

    def draw_uniform_suppression_time(self,lowerbound,upperbound):
        """Draw a new value of suppression time from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_suppression_time(20,30)
        >>> print round(test.suppression_time,2)
        29.66

        """
        self.suppression_time = random.uniform(lowerbound,upperbound)

    #Room area drawing methods

    def draw_uniform_room_area(self,lowerbound,upperbound):
        """Draw a new value of room area from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_room_area(20,30)
        >>> print round(test.room_area,2)
        29.66

        """
        self.room_area = random.uniform(lowerbound,upperbound)

    def draw_sampled_room_area(self,list_of_room_areas):
        """Draw a new value of room area from a supplied list of room areas

        Note one should have upwards of 50-100 room areas before this starts
        to become valid.
        
        >>> random.seed(1234)
        >>> test.draw_sampled_room_area(range(1,10))
        >>> print test.room_area
        9

        """
        self.room_area = random.sample(list_of_room_areas,1)[0]

    #Building area drawing methods
    def draw_uniform_bldg_area(self,lowerbound,upperbound):
        """Draw a new value of building area from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_bldg_area(20,30)
        >>> print round(test.bldg_area,2)
        29.66

        """
        self.bldg_area = random.uniform(lowerbound,upperbound)

    def draw_res_census_bldg_area(
            self,list_of_building_size_categories,
            list_of_number_of_units_in_each_category):
        """Draw a new value of residential building area from AHS data

        Data will only be available for "metropolitan statistical areas" and
        nationally.
        """
        raise NotImplementedError

    #Floor area drawing methods
    def draw_uniform_floor_area(self,lowerbound,upperbound):
        """Draw a new value of building area from its uniform distribution

        >>> random.seed(1234)
        >>> test.draw_uniform_floor_area(20,30)
        >>> print round(test.floor_area,2)
        29.66

        """
        self.floor_area = random.uniform(lowerbound,upperbound)


    #Define Method for drawing the values of DIST for different spread extents

    def draw_DIST_room(self):
        """Draw a new value of DIST corresponding to confined to Room extent 

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        >>> random.seed(1234)
        >>> call_list = ['alarm_time','dispatch_time','turnout_time','arrival_time',
        ... 'suppression_time','room_area','bldg_area']
        >>> names = ['draw_uniform_{}'.format(x) for x in call_list]
        >>> for x in names:
        ...     callname = getattr(test, x)
        ...     callname(20,30)
        >>> test.draw_DIST_room()
        >>> print round(test.DIST_room,2)
        -94.1

        """
        tasktime = (self.alarm_time+self.dispatch_time+self.turnout_time+
                   self.arrival_time+self.suppression_time)
        lowerbound = ((log(self.ao)-log(self.ao))/self.theta)-tasktime
        upperbound = ((log(self.room_area)-log(self.ao))/self.theta)-tasktime
        if lowerbound > upperbound:
            raise LowerBoundgreaterthanUpperBoundexception
        self.DIST_room = random.uniform(lowerbound,upperbound)
                     
    def draw_DIST_floor(self):
        """Draw a new value of DIST corresponding to confined to floor extent 

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        >>> random.seed(1234)
        >>> call_list = ['alarm_time','dispatch_time','turnout_time','arrival_time',
        ... 'suppression_time','room_area','floor_area','bldg_area']
        >>> names = ['draw_uniform_{}'.format(x) for x in call_list]
        >>> for x in names:
        ...     callname = getattr(test, x)
        ...     callname(30,30)
        >>> test.draw_DIST_floor()
        >>> print round(test.DIST_floor,2)
        330.4
        """
        tasktime = (self.alarm_time+self.dispatch_time+self.turnout_time+
                   self.arrival_time+self.suppression_time)
        lowerbound = ((log(self.room_area)-log(self.ao))/self.theta)-tasktime
        upperbound = ((log(self.floor_area)-log(self.ao))/self.theta)-tasktime
        if lowerbound > upperbound:
            raise LowerBoundgreaterthanUpperBoundexception
        self.DIST_floor = random.uniform(lowerbound,upperbound)
                     
    def draw_DIST_bldg(self):
        """Draw a new value of DIST corresponding to confined to building extent 

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        >>> random.seed(1234)
        >>> call_list = ['alarm_time','dispatch_time','turnout_time','arrival_time',
        ... 'suppression_time','room_area','bldg_area']
        >>> names = ['draw_uniform_{}'.format(x) for x in call_list]
        >>> for x in names:
        ...     callname = getattr(test, x)
        ...     callname(30,30)
        >>> test.draw_DIST_bldg()
        >>> print round(test.DIST_bldg,2)
        330.4

        >>> random.seed(1234)
        >>> Dcalc = DISTCalculate(floor_extent=True)
        >>> call_list = ['alarm_time','dispatch_time','turnout_time','arrival_time',
        ... 'suppression_time','floor_area','room_area','bldg_area']
        >>> names = ['draw_uniform_{}'.format(x) for x in call_list]
        >>> for x in names:
        ...     callname = getattr(Dcalc, x)
        ...     callname(20,20)
        >>> Dcalc.draw_DIST_bldg()
        >>> print round(Dcalc.DIST_bldg,2)
        323.13
        """
        tasktime = (self.alarm_time+self.dispatch_time+self.turnout_time+
                   self.arrival_time+self.suppression_time)
        if self.floor_extent:
            lowerbound = ((log(self.floor_area)-log(self.ao))/self.theta)-tasktime
        else:
            lowerbound = ((log(self.room_area)-log(self.ao))/self.theta)-tasktime
        upperbound = ((log(self.bldg_area)-log(self.ao))/self.theta)-tasktime
        if lowerbound > upperbound:
            raise LowerBoundgreaterthanUpperBoundexception
        self.DIST_bldg = random.uniform(lowerbound,upperbound)
                     
    def draw_DIST_beyond(self):
        """Draw a new value of DIST corresponding to confined to Room extent 

        Note that prior to using this method one must draw all of the time
        attributes and area attributes. See the doctest.

        Note that the DIST_beyond extent functions differently. Rather than 
        drawing from a uniform distribution, its value is deterministically
        calculated from the present values of the time attributes and the 
        building area.

        >>> random.seed(1234)
        >>> call_list = ['alarm_time','dispatch_time','turnout_time','arrival_time',
        ... 'suppression_time','room_area','bldg_area']
        >>> names = ['draw_uniform_{}'.format(x) for x in call_list]
        >>> for x in names:
        ...     callname = getattr(test, x)
        ...     callname(40,40)
        >>> test.draw_DIST_beyond()
        >>> print round(test.DIST_beyond,2)
        321.03
        """
        tasktime = (self.alarm_time+self.dispatch_time+self.turnout_time+
                   self.arrival_time+self.suppression_time)
        self.DIST_beyond = ((log(self.bldg_area)-log(self.ao))/self.theta)-tasktime


if __name__=="__main__":
   import doctest
   doctest.testmod(extraglobs={'test': DISTCalculate()})
