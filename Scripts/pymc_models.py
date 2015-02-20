#!/usr/bin/env python

""" Module for setting up statistical models"""

from __future__ import division
import pandas as pd
import pymc as mc
import numpy as np

# import pymc_data


def response_time_correction():
    """Estimating a time correction for fire department response to fire
    using a fire growth model and estimates of fire fighter response time
    """

    # Get NFIRS data - this is a placeholder for now
    nfirs_bins = np.array([250,62,3])/(250+62+3)

    # Get incident data for total fire calculation
    incident = pd.read_csv('../Data/arlington_incidents.csv', header=0)
    total_incidents = len(incident['incident_class_code'])
    total_fires = 0
    for i in incident['incident_class_code']:
        if i == 1:
           total_fires = total_fires + 1 
    years_of_data = 6
    fire_call_year = int(total_incidents/years_of_data)    

    # Priors
    correction_time = mc.Uniform('tct', lower=-1000., upper=1000., value=0.)
    sigma = mc.Uniform('sigma', lower=0., upper=100., value=1.)
    
    # Damage estimates
    @mc.deterministic
    def y_mean(tct=correction_time,total_fires=total_fires):

        # Fire Growth model
        fire_size = np.zeros(total_fires)
        room_of_origin = 0
        floor_of_origin = 0
        structure_loss = 0
        for num in range(0,total_fires): 
            alpha = np.random.uniform(0.0029,0.047)
            time_to_alarm = np.random.uniform(30,60)
            time_to_dispatch = np.random.uniform(40,80)
            time_to_turnout = np.random.uniform(60,100)
            time_to_arrival = np.random.uniform(300,420)
            time_to_suppress = np.random.uniform(60,180)
            
            # Calculate total run time and fire size
            running_time = time_to_alarm+time_to_dispatch+time_to_turnout+time_to_arrival+time_to_suppress+correction_time
            fire_size[num] = alpha*(running_time)**2

            #assesing damage typical resident structure
            if fire_size[num] < 2000:
                room_of_origin = room_of_origin + 1
            elif fire_size[num] > 2000 and fire_size[num] < 10000:
                floor_of_origin =floor_of_origin + 1
            else:
                structure_loss =structure_loss + 1

        return np.array([room_of_origin, floor_of_origin, structure_loss])/(room_of_origin+floor_of_origin+structure_loss)

    # Likelihood
    # The likelihood is N(mu_i, sigma^2), where sigma
    # is pulled from a uniform distribution.
    y_obs = mc.Normal('y_obs', value=nfirs_bins,
                      mu=y_mean, tau=sigma**-2,
                      observed=True)

    return vars()
