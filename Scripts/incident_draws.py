#Anderson
#2-19

from __future__ import division
import numpy as np
import pandas as pd
from pylab import *
import random

#import the data
incidents = pd.read_csv('../Data/ArlingtonCensusFireDataYearly.csv')
#aggregate the yearly number of residential structure fires that ACFD responded to
yeardist = incidents.groupby('year').aggregate('sum')['COUNT']
#aggregate the total number of residential fires that occurred in each census tract
tractincidents = incidents.groupby('GEO.id2').aggregate('sum')['COUNT']
#delete the incidents object to save memory (someday this may be huge)
del incidents
#add 1 to all tract incidents to allow zero incident tracts a small probability of selection
tractincidents = tractincidents + 1
tractincidents.sort()
#build the cumulative distribution for selecting tracts
tractshare = tractincidents/tractincidents.sum()
tractcum = tractshare.cumsum()

###figure out how to draw from the cumulative distribution
randdraw = 0.01 #pretend this comes frmo an rng
tractind = np.where(randdraw <= tractcum)[0].min()
#slice the distribution and retrieve the tract (index) corresponding to drawn value.
tractdraw = tractcum[tractind:tractind+1].index.tolist()
print tractdraw

###derive the normal distribution approximation to use as a stopping rule
yrmean = yeardist.mean()
print yrmean
print yeardist
yrvar=yeardist.var()
####perform normal draws
yrdraw = round(np.random.normal(yrmean,sqrt(yrvar)))
print yrdraw
