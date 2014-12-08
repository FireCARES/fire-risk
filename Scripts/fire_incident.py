#Weinschenk
#12-14

from __future__ import division
import numpy as np
import pandas as pd
from pylab import *
from matplotlib import rcParams
rcParams.update({'figure.autolayout': True})
import random

incident = pd.read_csv('../Data/arlington_incidents.csv', header=0)
total_incidents = len(incident['incident_class_code'])
total_fires = 0
for i in incident['incident_class_code']:
	if i == 1:
	   total_fires = total_fires + 1 

for num in range(1,1000):
	rand = random.randrange(1,len(incident['incident_class_code']),1)
	if incident['incident_class_code'][rand] == 1:
	 	fire = True
	 	print rand,fire
	else:
	 	fire = False
