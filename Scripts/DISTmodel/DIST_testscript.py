import numpy as np
import DIST_import
import DIST_calculations
import DIST_output

floor_extent=False

#import all the values
Dimport = DIST_import.DISTImport()
#Dimport.pgdb_import('nfirs2','postgres','localhost','password','table(or_view?)ofstructurefires')
Dimport.set_firespread_count([93,190,39,64,9])
Dimport.room_area_set_uniform_limits(72,380)
Dimport.bldg_area_set_uniform_limits(1088,9004)
Dimport.alarm_time_set_uniform_limits(90,120)
Dimport.dispatch_time_set_uniform_limits(40,80)
Dimport.turnout_time_set_uniform_limits(60,100)
Dimport.arrival_time_set_uniform_limits(300,420)
Dimport.suppression_time_set_uniform_limits(60,180)

Dcalc = DIST_calculations.DISTCalculate(floor_extent=floor_extent)
Dout = DIST_output.DISTOutput(
        Dimport.get_extent_list(),Dimport.get_firespread_count(), floor_extent
        )

#set Gibbs chain settings
n_iter = 10000
burn_in = 500
thin = 1
iter_total = n_iter+burn_in

#determine size of the chains necessary to hold data
n_store = int(n_iter/thin+0.0001)

#obtain a list of attributes to be recorded
#Note that you must always include DIST_room, DIST_bldg, and DIST_beyond
#in that order
record_list = ['DIST_room','DIST_bldg','DIST_beyond','room_area']

#initialize space for the chains
chain_record = np.full((n_store,len(record_list)),-1000)

#begin gibbs sampling
call_list = ['alarm_time','dispatch_time','turnout_time','arrival_time',
'suppression_time','room_area','bldg_area']
for i in range(iter_total):
    drawfunctions = ['draw_uniform_{}'.format(x) for x in call_list]
    getfunctions = ['{}_get_uniform_limits'.format(x) for x in call_list]
    for x,y in zip(drawfunctions,getfunctions):
        draws = getattr(Dcalc, x)
        gets = getattr(Dimport,y)
        draws(*gets())
    Dcalc.draw_DIST_room()
    Dcalc.draw_DIST_bldg()
    Dcalc.draw_DIST_beyond()
    if(i >= burn_in):
        for z,label in zip(range(chain_record.shape[1]),record_list):
            chain_record[i-burn_in,z] = getattr(Dcalc, label)
            
#output the DIST score. For 'default' values (present values in fields)
#the value returned should be 13.0, I don't know how to doctest a script
print round(Dout.DIST_score(chain_record[...,0],chain_record[...,1],
                            chain_record[...,2]))
