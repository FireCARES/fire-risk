#Weinschenk
#12-14

from __future__ import division
import numpy as np
import pandas as pd
from pylab import *
from matplotlib import rcParams
rcParams.update({'figure.autolayout': True})
import random

incident = pd.read_csv('data/arlington_incidents.csv', header=0)
total_incidents = len(incident['incident_class_code'])
total_fires = 0
for i in incident['incident_class_code']:
	if i == 1:
	   total_fires = total_fires + 1 
years_of_data = 6

#random pull off historical data for ignition
fire_call_year = int(total_incidents/years_of_data)
ignition = zeros(fire_call_year, dtype=bool)
for num in range(0,fire_call_year):
	rand = random.randrange(1,len(incident['incident_class_code']),1)
	if incident['incident_class_code'][rand] == 1:
	 	ignition[num] = True
print sum(ignition), 'projected fires' #prints number of fires per year

#determine location of fire and structure type




#firegrowth model
fire_size = zeros(sum(ignition))
room_of_origin = 0
floor_of_origin = 0
structure_loss = 0
for num in range(0,sum(ignition)): 
	alpha = np.random.uniform(0.0029,0.047)
	time_to_alarm = np.random.uniform(30,60)
	time_to_dispatch = np.random.uniform(40,80)
	time_to_turnout = np.random.uniform(60,100)
	time_to_arrival = np.random.uniform(300,420)
	time_to_suppress = np.random.uniform(60,180)
	running_time = time_to_alarm+time_to_dispatch+time_to_turnout+time_to_arrival+time_to_suppress
	fire_size[num] = alpha*(running_time)**2

	#assesing damage typical resident structure
	if fire_size[num] < 2000:
		room_of_origin = room_of_origin + 1
	elif fire_size[num] > 2000 and fire_size[num] < 10000:
		floor_of_origin =floor_of_origin + 1
	else:
		structure_loss =structure_loss + 1

print room_of_origin, 'fire(s) room of origin |', floor_of_origin, ' fire(s) floor of origin |', structure_loss, 'fire(s) with total structure loss'

#firefighter response model


