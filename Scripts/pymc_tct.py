#!/usr/bin/env python

import matplotlib
matplotlib.use("Agg")
import pandas as pd
import pylab as pl
import pymc as mc

import pymc_models
# import pymc_graphics
# import pymc_data

# Get incident data for total fire calculation
incident = pd.read_csv('../Data/arlington_incidents.csv', header=0)
total_incidents = len(incident['incident_class_code'])
total_fires = 0
for i in incident['incident_class_code']:
    if i == 1:
       total_fires = total_fires + 1 
# years_of_data = 6
# fire_call_year = int(total_incidents/years_of_data) 

# Generate model
vars = pymc_models.response_time_correction(total_fires)

# Fit model with MAP estimates
map = mc.MAP(vars)
map.fit(method='fmin_powell', verbose=2)

m = mc.MCMC(vars)
# Configure and run MCMC simulation
m.sample(iter=2000, burn=0, thin=1)
# m.sample(iter=500, burn=0, thin=1)

# Plot results
# pl.figure()
# graphics.plot_ps_radiation_model(m)
# pl.savefig('../Figures/heat_flux_localization_fds_'+str(fire_size)+'kW.pdf')

# Plot resulting distributions and convergence diagnostics
mc.Matplot.plot(m,
                format='pdf',
                path='./test')

m.summary()

# m.write_csv('../Figures/'+str(fire_size)+'.csv')
