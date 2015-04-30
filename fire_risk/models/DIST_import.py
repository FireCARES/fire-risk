import psycopg2
import random

class DISTImport(object):
    """Contains the import methods for the DIST model.

    The DistImport class contains the import methods for the DIST model.
    Additionally, it stores the relevant input parameters used by other aspects
    of the model in attributes.

    Attributes:
        extent_list (list): ordered list of the extents of fire spread.
        firespread_count (dict): dictionary of fire spread attributes.
            

    """
    def __init__(self):
        self.extent_list = ['object','room','floor','bldg','beyond']
        self.firespread_count = {x: 0 for x in self.extent_list}

    def get_firespread_count(self):
        """Return the firespread_count attribute
        >>> test = DISTImport()
        >>> test.get_firespread_count()
        {'bldg': 0, 'object': 0, 'beyond': 0, 'room': 0, 'floor': 0}
        """
        return self.firespread_count

    def get_extent_list(self):
        """Return the extent_list attribute
        >>> test = DISTImport()
        >>> test.get_extent_list()
        ['object', 'room', 'floor', 'bldg', 'beyond']
        """
        return self.extent_list

    # Fire spread import methods
    def flatfile_import(self,flatfiles):
        """Parse a set of NFIRS incident flat files for structure fires.

        Args:
            flatfiles (list): a list of file pathnames for files to be parsed.
        Returns:
            changes the values of the firespread_count attributes to calculated
            values
        """

        raise NotImplementedError
        
    def pgdb_import(self,dbname,user,host,password,tablename):
        """Parse a relevant NFIRS database table for structure fire spread count.

        Returns:
            changes the relevant values of the firespread_count

        #>>> test.pgdb_import()
        #>>> test.get_firespread_count()
        #{'bldg': 411340L, 'object': 72775L, 'beyond': 324971L, 'room': 353886L, 'floor': 88510L}
        """
        try: 
            conn = psycopg2.connect("dbname='{}' user='{}' host='{}' \
                                    password='{}'".format(dbname,user,
                                    host,password))
        except:
            print "I am unable to connect to the database."

        cur = conn.cursor()
        cur.execute("""SELECT fire_sprd, count(*) from {0} where fire_sprd IN \
                    ('1','2','3','4','5') group by fire_sprd \
                    order by fire_sprd desc""".format(tablename))
        rows = cur.fetchall()
        for row, extent in zip(rows, self.extent_list):
            self.firespread_count[extent]=row[1]

        
    def set_firespread_count(self,list_of_five_values):
        """Manually set the values of firespread_count

        >>> test = DISTImport()
        >>> test.set_firespread_count([300,200,50,100,10])
        >>> test.get_firespread_count()
        {'bldg': 100, 'object': 300, 'beyond': 10, 'room': 200, 'floor': 50}
        """
        for value,extent in zip(list_of_five_values,self.extent_list):
            self.firespread_count[extent]=value

    # Room area distribution import methods
    def room_area_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the uniform room area distribution.

        >>> test = DISTImport()
        >>> test.room_area_set_uniform_limits(20,30)
        
        """
        self.room_area_limits = (lowerbound,upperbound)

    def room_area_get_uniform_limits(self):
        """Return the uniform distribution limits of the room areas

        >>> test = DISTImport()
        >>> test.room_area_get_uniform_limits()
        self.room_area_limits is undefined!

        >>> test.room_area_set_uniform_limits(20,30)
        >>> test.room_area_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.room_area_limits
        except AttributeError: 
            print "self.room_area_limits is undefined!"

    def room_area_data_import(self,databasevariables):
        """Import set of room areas from an external source.

        Args:
            Insert database variables here

        Returns:
            generate a room area values attribute that can be manipulated.
        """
        raise NotImplementedError

    #Floor Area distribution import methods
    #Note that floor area is optional in the model. it will largely pertain
    #to high-rise buildings where the floor area is more likely to be reported
    #and sensible
    def floor_area_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the uniform floor area distribution.

        >>> test = DISTImport()
        >>> test.floor_area_set_uniform_limits(20,30)
        
        """
        self.floor_area_limits = (lowerbound,upperbound)

    def floor_area_get_uniform_limits(self):
        """Return the uniform distribution limits of the floor areas

        >>> test = DISTImport()
        >>> test.floor_area_get_uniform_limits()
        self.floor_area_limits is undefined!

        >>> test.floor_area_set_uniform_limits(20,30)
        >>> test.floor_area_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.floor_area_limits
        except AttributeError: 
            print "self.floor_area_limits is undefined!"

    def floor_area_data_import(self,databasevariables):
        """Import set of floor areas from an external source.

        Args:
            Insert database variables here

        Returns:
            generate a floor area values attribute that can be manipulated.
        """
        raise NotImplementedError

    #Building Area distribution import methods
    def bldg_area_set_uniform_limits(self, lowerbound, upperbound):
        """Set the limits of the uniform room area distribution.
        >>> test = DISTImport()
        >>> test.bldg_area_set_uniform_limits(20,30)
        
        """
        self.bldg_area_limits = (lowerbound,upperbound)

    def bldg_area_get_uniform_limits(self):
        """Return the uniform distribution limits of the bldg areas

        >>> test = DISTImport()
        >>> test.bldg_area_get_uniform_limits()
        self.bldg_area_limits is undefined!

        >>> test.bldg_area_set_uniform_limits(20,30)
        >>> test.bldg_area_get_uniform_limits()
        (20, 30)

        
        """
        try: 
            return self.bldg_area_limits
        except AttributeError: 
            print "self.bldg_area_limits is undefined!"
        
    def bldg_area_data_import(self,databasevariables):
        """Import set of building areas from external source.

        Args:
            Insert relevant variables here

        Returns:
            generate a bldg area values attribute that can be manipulated.
        """
        raise NotImplementedError
    
    def bldg_area_res_census_data_import(self,options):
        """Import AHS estimates of housing stock building size

        This might either query some census API or, if necessary,
        take a manual .csv. Whichever implementation works best.
        Note that unit sizes are only available in select geographic areas
        and nationally. See census tables C-02-AH and C-02-AH-M on
        factfinder.census.gov

        Args:
            Up for debate.

        Returns:
            list_of_two_equal_sized_lists (list of lists):
                first list contains the square footage of unit category
                second list contains the building area extent counts (
        """
        raise NotImplementedError

    #Time input methods
    def alarm_time_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the alarm time uniform distribution
        >>> test = DISTImport()
        >>> test.alarm_time_set_uniform_limits(20,30)

        """
        self.alarm_time_limits = (lowerbound,upperbound)
    def alarm_time_get_uniform_limits(self):
        """Return the uniform distribution limits of the alarm time

        >>> test = DISTImport()
        >>> test.alarm_time_get_uniform_limits()
        self.alarm_time_limits is undefined!

        >>> test.alarm_time_set_uniform_limits(20,30)
        >>> test.alarm_time_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.alarm_time_limits
        except AttributeError: 
            print "self.alarm_time_limits is undefined!"

    def dispatch_time_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the dispatch time uniform distribution
        >>> test = DISTImport()
        >>> test.dispatch_time_set_uniform_limits(20,30)

        """
        self.dispatch_time_limits = (lowerbound,upperbound)
    def dispatch_time_get_uniform_limits(self):
        """Return the uniform distribution limits of the dispatch time

        >>> test = DISTImport()
        >>> test.dispatch_time_get_uniform_limits()
        self.dispatch_time_limits is undefined!

        >>> test.dispatch_time_set_uniform_limits(20,30)
        >>> test.dispatch_time_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.dispatch_time_limits
        except AttributeError: 
            print "self.dispatch_time_limits is undefined!"

    def turnout_time_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the turnout time uniform distribution

        >>> test = DISTImport()
        >>> test.turnout_time_set_uniform_limits(20,30)

        """
        self.turnout_time_limits = (lowerbound,upperbound)

    def turnout_time_get_uniform_limits(self):
        """Return the uniform distribution limits of the turnout time

        >>> test = DISTImport()
        >>> test.turnout_time_get_uniform_limits()
        self.turnout_time_limits is undefined!

        >>> test.turnout_time_set_uniform_limits(20,30)
        >>> test.turnout_time_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.turnout_time_limits
        except AttributeError: 
            print "self.turnout_time_limits is undefined!"

    def arrival_time_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the arrival time uniform distribution

        >>> test = DISTImport()
        >>> test.arrival_time_set_uniform_limits(20,30)

        """
        self.arrival_time_limits = (lowerbound,upperbound)
    def arrival_time_get_uniform_limits(self):
        """Return the uniform distribution limits of the arrival time

        >>> test = DISTImport()
        >>> test.arrival_time_get_uniform_limits()
        self.arrival_time_limits is undefined!

        >>> test.arrival_time_set_uniform_limits(20,30)
        >>> test.arrival_time_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.arrival_time_limits
        except AttributeError: 
            print "self.arrival_time_limits is undefined!"

    def suppression_time_set_uniform_limits(self,lowerbound,upperbound):
        """Set the limits of the suppression time uniform distribution

        >>> test = DISTImport()
        >>> test.suppression_time_set_uniform_limits(20,30)

        """
        self.suppression_time_limits = (lowerbound,upperbound)
    def suppression_time_get_uniform_limits(self):
        """Return the uniform distribution limits of the suppression time

        >>> test = DISTImport()
        >>> test.suppression_time_get_uniform_limits()
        self.suppression_time_limits is undefined!

        >>> test.suppression_time_set_uniform_limits(20,30)
        >>> test.suppression_time_get_uniform_limits()
        (20, 30)

        """
        try: 
            return self.suppression_time_limits
        except AttributeError: 
            print "self.suppression_time_limits is undefined!"

if __name__=="__main__":
   import doctest
   doctest.testmod(extraglobs={'test': DISTImport()})
