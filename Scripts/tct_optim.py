from __future__ import division
import matplotlib
matplotlib.use('Agg')
import numpy as np
from pylab import *
import pandas as pd

# Optimization class for finding tct
class tctOptim:
    # Initialization
    # Inputs: total number of fires, true number of fires in each bin (list)
    def __init__(self,total_fires,true_bins):
        self.total_fires = total_fires
        self.true_bins = true_bins

    # Sampler function for model parameters
    # Inputs:
    def sampler(self):
        # Draw n samples from each distribution where n is the total number of fires
        self.alpha = np.random.uniform(0.0029,0.047,size=self.total_fires)
        self.exp = np.random.uniform(1,2,size=self.total_fires)
        time_to_alarm = np.random.uniform(30,60,size=self.total_fires)
        time_to_dispatch = np.random.uniform(40,80,size=self.total_fires)
        time_to_turnout = np.random.uniform(60,100,size=self.total_fires)
        time_to_arrival = np.random.uniform(300,420,size=self.total_fires)
        time_to_suppress = np.random.uniform(60,180,size=self.total_fires)
        self.running_time = time_to_alarm + time_to_dispatch + time_to_turnout + time_to_arrival + time_to_suppress

    # Binning function for fires
    # Inputs: optional tct
    def binFun(self,tct=0.0):

        # Bin counters
        room_of_origin = 0
        floor_of_origin = 0
        structure_loss = 0
        fire_size = np.zeros(total_fires)

        # Loop through all fires and assign to a bin
        for num in range(0,self.total_fires):
            running_time_cor = self.running_time[num] + tct
            fire_size[num] = self.alpha[num]*(running_time_cor)**self.exp[num]

            # Assesing damage typical resident structure
            if fire_size[num] < 2000:
                room_of_origin = room_of_origin + 1
            elif fire_size[num] > 2000 and fire_size[num] < 10000:
                floor_of_origin =floor_of_origin + 1
            else:
                structure_loss =structure_loss + 1

        return [room_of_origin,floor_of_origin,structure_loss]

    # Error function
    # Inputs: tct (s)
    def eCalc(self,tct):
        # Get bins with corrected time
        pred_bins = self.binFun(tct=tct)
        eTot = 0.0

        # Calculate the sum of squared errors
        for i in range(len(self.true_bins)):
            eTot += (self.true_bins[i]-pred_bins[i])**2
        return eTot

    # Golden section search for tct
    # Inputs: optional print out
    def goldenSection(self,verb=False):
        # Set low and high bounds of search
        it_L = -500.
        it_H = 1000.
        f_calls = 0
        count = 1

        # Low and high error calculations
        eTotal_L = self.eCalc(it_L)
        eTotal_H = self.eCalc(it_H)
        f_calls = f_calls + 2
        if verb == True:
            print("Initial Error Calculations for tct")
            print 'Low Guess: '+str(eTotal_L)
            print 'High Guess: '+str(eTotal_H)
            print("#\tvalue\terror")

        # Golden Ratio
        gold = (1.0 + 5**.5) / 2.0        

        # Calculate first golden point
        it_A = (gold * it_L + it_H)/(gold + 1.0) 
        eTotal_A = self.eCalc(it_A)
        f_calls = f_calls + 1
        if verb == True:
            print "%i\t%.2f\t%.2f" %(count, it_A, eTotal_A)
        count += 1

        while abs(it_L-it_H)>1.0 and count < 50:
            # Calculate next golden point for comparison
            it_B = it_L + it_H - it_A
            eTotal_B = self.eCalc(it_B)
            f_calls = f_calls + 1
            if verb == True:
                print "%i\t%.2f\t%.6f" %(count, it_B, eTotal_B)
                # print it_L,it_A,it_B,it_H
                # print eTotal_L,eTotal_A,eTotal_B,eTotal_H
            count += 1

            # Decide new point assignment based on whether A or B is greater
            if it_A < it_B:
                if eTotal_B>eTotal_A:
                    it_H = it_B
                    eTotal_H = eTotal_B 
                elif eTotal_B<=eTotal_A:
                    it_L = it_A
                    eTotal_L = eTotal_A
                    it_A = it_B
                    eTotal_A = eTotal_B
            elif it_A > it_B:
                if eTotal_B>eTotal_A:
                    it_L = it_B
                    eTotal_L = eTotal_B 
                elif eTotal_B<=eTotal_A:
                    it_H = it_A
                    eTotal_H = eTotal_A
                    it_A = it_B
                    eTotal_A = eTotal_B
        
        return it_A

# Read in data and get total fires
data = pd.read_csv('../Data/ArlingtonCensusFire.csv', header=0)
total_fires = len(data['inc_type'])
room_origin = 0 
floor_origin = 0 
structure = 0
for i in data['fire_sprd']:
    if i == 1 or i == 2:
       room_origin = room_origin + 1 
    elif i == 3 or i == 4:
        floor_origin = floor_origin + 1
    else:
        structure = structure + 1
print 'Total fires:',total_fires

# True bins
bAct = [room_origin,floor_origin,structure]
print 'Actual bins:', bAct

# Call optimizer class
tOpt = tctOptim(total_fires,bAct)

# Sample set of times and alphas
tOpt.sampler()

# Print bins of sample
print 'Single Bin Sample:',tOpt.binFun()

# Single optimization run
tct = tOpt.goldenSection()
print 'Single correction time:',tct
print 'Single corrected bin:',tOpt.binFun(tct=tct)


# Array of correction times
n = 1000
tct = np.zeros(n)

# Resample and optimize for tct n times
for i in range(n):
    if i%20 == 0:
        print 'Sample',i
    tOpt.sampler()
    tct[i] = tOpt.goldenSection()

# Plot a histogram of tct

plt.figure()
plt.hist(tct,bins=20)
plt.xlabel('t correction (s)',size=18)
plt.ylabel('Count',size=18)
savefig('../Figures/t_correct_histogram.pdf',format='pdf')
