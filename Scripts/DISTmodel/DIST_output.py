from __future__ import division
import numpy as np
import copy
import matplotlib.pyplot as plt


class DISTOutput(object):
    """Contains the output methods for the DIST model.

    The DistOutput class contains the output methods for the DIST model.
    Note that this class does not contain the raw output values, which are
    tracked within the wrapper class. Instead, it contains the methods for
    manipulating the raw output into presentable formats, including diagnostic
    graphs, summaries, and the final DIST score.

    Attributes:
        extent_list(list): list of firespread extents.
        firespread_count(dict): relevant counts of firespread extents.
            'relevant' refers to the fact that this will contain either
            room, building (bldg), and spread beyond (beyond) extents,
            or could optionally include floor if floor_extent is True.
        floor_extent(boolean): flag designating whether floor extent is
            to be included in the output calculations. This might become
            relevant when doing occupancy calculations where floor is a 
            usefully delimiting extent.
        

    """
    def __init__(self,extent_list,firespread_count,floor_extent=False):
        """initialize attributes of the DISTOutput class.

        Args:
            extent_list(list): list of firespread extents, see DISTImport class.
            firespread_count(dict): dictionary of firespread counts by extent.
                see DISTImport class.

        >>> extent_list = ['object','room','floor','bldg','beyond']
        >>> firespread_count = {x: 30 for x in extent_list}
        >>> Dout = DISTOutput(extent_list,firespread_count,True)
        >>> print Dout.firespread_count
        {'floor': 30, 'beyond': 30, 'bldg': 30, 'room': 60}

        >>> extent_list = ['object','room','floor','bldg','beyond']
        >>> firespread_count = {x: 30 for x in extent_list}
        >>> Dout = DISTOutput(extent_list,firespread_count)
        >>> print Dout.firespread_count
        {'beyond': 30, 'bldg': 60, 'room': 60}

        """
        self.floor_extent = floor_extent
        self.firespread_count = copy.deepcopy(firespread_count)
        self.firespread_count['room'] = (self.firespread_count['object']+
                self.firespread_count['room'])
        self.firespread_count.pop('object', None)
        if not self.floor_extent:
            self.firespread_count['bldg'] = (self.firespread_count['floor']+
                    self.firespread_count['bldg'])
            self.firespread_count.pop('floor', None)

    #diagnostic methods
    def traceplot(self,vector_of_drawn_values):
        """generate a traceplot of a vector of drawn values.

        """
        raise NotImplementedError
        

    def densityplot(self,vector_of_drawn_values):
        """Plot the estimated probability density function of the drawn values.

        """
        raise NotImplementedError

    def summarystats(self,vector_of_drawn_values,
                     list_of_quantiles=[0.025,0.25,0.5,0.75,0.975]):
        """Calculate and print summary statistics of given raw output.
        
        """
        raise NotImplementedError
    
    def save_raw_output(self,vector_of_drawn_values):
        """Save raw output to a temporary .csv file

        """
        raise NotImplementedError

    #Aggregate Raw output methods

    def raw_DIST_compute(self,DIST_room,DIST_bldg,DIST_beyond,DIST_floor=None):
        """Compute the raw DIST value from the raw constituent chains

        Note that inputs should be in the form of numpy vectors.
        Inputs should also, by design, be of equal length.

        Returns:
            Numpy vector of raw DIST values compiled from extent chains.

        """
        total_fires = sum(self.firespread_count.values())
        roomweight = self.firespread_count['room']/total_fires
        bldgweight = self.firespread_count['bldg']/total_fires
        beyondweight = self.firespread_count['beyond']/total_fires
        if self.floor_extent:
            floorweight = self.firespread_count['floor']/total_fires
        raw_DIST = (roomweight*DIST_room+bldgweight*DIST_bldg+
                    beyondweight*DIST_beyond)
        if DIST_floor is not None:
            raw_DIST = raw_DIST + floorweight*DIST_floor
        return raw_DIST

    def DIST_score(self,DIST_room,DIST_bldg,DIST_beyond,DIST_floor=None):
        """Compute the single value DIST score from the raw constituent chains

        Note that inputs should be in the form of numpy vectors. 

        """
        raw_DIST = self.raw_DIST_compute(DIST_room,DIST_bldg,DIST_beyond,DIST_floor)
        raw_DIST[raw_DIST < 0] = 0
        DIST_score = np.average(raw_DIST)
        return DIST_score

    #Output to file methods to be added
        

if __name__=="__main__":
   import doctest
   doctest.testmod()
