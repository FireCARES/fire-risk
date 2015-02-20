#!/usr/bin/env python

import matplotlib
matplotlib.use("Agg")

import pylab as pl
import pymc as mc

import pymc_models
# import pymc_graphics
# import pymc_data

# Generate model
vars = pymc_models.response_time_correction()

# Fit model with MAP estimates
map = mc.MAP(vars)
map.fit(method='fmin_powell', verbose=2)

m = mc.MCMC(vars)
# Configure and run MCMC simulation
m.sample(iter=5000, burn=2500, thin=1)
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
