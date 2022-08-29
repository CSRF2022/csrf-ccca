# A test run following instructions in the readme file of
  #https://github.com/duplisea/ccca

# install ccca package
# library(devtools)
# install_github("duplisea/ccca")

library(ccca)

# Run the basic model fit and projection simulation
# -------------------------------------------------
#
#   A default parameters file (params) comes with the package. It is a list
# with options needed to fit the P/B model and run projections. The mapply
# step just moves each list object into the global environment rather than
# calling the list element each time in Monte Carlo simulations so it is
# faster and it also does not alter the base parameterisation in the list
# so you can return to it later.

data(params)
mapply(assign, names(params), params, MoreArgs=list(envir = globalenv()))

# Two critical steps:
# 1. Fit a SP model to get the production/biomass (PB)
# 2. Get the relationship between P/B and the env variable (EV)

# Determine the annual P/B ratio of the population creating a dataframe
# with the index bumped by q

PB= PB.f(turbot, ref.years=ref.years, q=q)

# Fit a relationship between the P/B and the E variable according to what
# you selected in params

PvsE= PBE.fit.f(PB,model.type=model.type, knots=knots, poly.degree=poly.degree)

# Fit a normal distribution to the E time series and pull the parameters
# out.

Enorm= norm.fit.f(E=PB$E)
Edist.a=Enorm$estimate[1]
Edist.b=Enorm$estimate[2]

# Develop the E projection values based on your choice of projection
# parameters

Eproj= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift, proj.years=proj.years, N)

#Develop the PB projection values based on the P/B vs E fit and the
# E projection you specified above.
# When add.residuals=1 then a residual is sampled for each time step of each
# projection and is added to the P/B values.

# If the P/B vs E fit is relatively unbiased then it will not make
# much difference to the median but it will increase the future uncertainty.
# If you set it =0 then the residual is not added and you will have
# less uncertainty in the future. If the P/B vs E model is biased,
# this bias will be more likely to be carried forward if residuals
# are not sampled (=0).

PBproj= PB.for.projection.f(PvsE=PvsE,Eproj,add.residuals=add.resids)

# The fishing strategy. In this case is the the mean exploitation
# (Index corrected by catchability) during the last five years.

Fstrat= F.strategy(PB, 2014:2018, moratorium=F)

# Run the projection given all the above calculations and parameters.
# The trajectory over the projection period and the probability that B is
# greater than the objective after the specified time range.

Bproj= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj, Fstrat, K=K, theta=1)
Fout= Fseq.f(PB,PBproj=PBproj,Fseq=fs,time.frame=time.frame, N=N, K=K)
PofF= PofF.f(PB,Fout,ref.pt=ref.pt)

# Summarise the output of the projections by calculating quantiles and
# putting results in a dataframe. Calculates the probability of being
# at or above a reference point.

Bproj.summary= Bproj.summary.f(PB,Bproj,PBproj,Eproj)
P= rankprob.f(Bproj,PB,ref.pt)

# We have done all of the above again but for different climate scenario
# mean shift, one warm and one cold, one with status quo mean
# temperature but increased variance and finally a null model run
# where P/B does not depend on E. It is not shown here but you do it
# by modifying the Emean.shift and Evariance parameters and re-running
# the above steps. These projection objects have names warm, cold, E.var.inc
# to distinguish them from the base run and they appear in the figures

# Run mutliple simulations for each combination of E scenario and F.
# First the E and PB scenarios are set up and then multiple projections
# are made based on the exploitation rate series specified in the parameters
# file.

ECCF= Eproj.list.f(Emean.shifts=Emean.shifts, N=N.CCF, proj.years=proj.years, Edist.a=Edist.a,
                   Edist.b=Edist.b)
PBCCF= PBproj.list.f(PvsE=PvsE, Eprojection=ECCF)
CCF.raw= P.R.for.EF.f(E.CCF=ECCF, PB.CCF=PBCCF, Fs=fs, PB=PB, ref.pt=ref.pt, Bstart.mult=Bstart.mult,
                      K=K, theta=theta)

# Plots of the data and projections
# ---------------------------------

#The Kobe plot of the historical data with the E variable colour coded on
#top of the points. There is function called Kobe.f to do this.

Kobe.f(PB=PB,E=PB$E)
colramp.legend(col1="red", col2="blue", ncol=length(PB$E), 2.5, 3.5, 2.7, 4.5)

# ![Figure 3: a Kobe plot of biomass relative to the reference period
#   (1995-2000) biomass and the relative exploitation rate in the reference
#   period. The water temperature at 250 m is colour coded on top
#   (blue-red:cold-warm).](README_files/figure-markdown_strict/kobe-1.png)
#
# The P/B vs E data plot and fitted model. ![Figure 4: The relationship
#                                            between the stock P/B ratio and the environmental variable (temperature
#                                                                                                        at 250 m) with an adaptive GAM model fitted to the
#                                            relationship.](README_files/figure-markdown_strict/pvseplot-1.png)
#
# The normal distrubtion fitted to the E series which is sampled for
# future climate projections altering the mean and variance for different
# climate scenarios. ![Figure 5: The distributions of temperature used for
#                      projection scenarios. Black is the baseline where the distribution was
#                      fitted to all years; red is shift in the mean 0.5 C warmer than the
#                      baseline; blue line is 0.5 C colder than the baseline; green line has
#                      the same mean as the baseline but a 50% increase in standard
#                      deviation.](README_files/figure-markdown_strict/gammaplot-1.png)
#
# The distribution of the P/B values for the future projections. It is the
# P/B that results by sampling E distribution function to simulate a
# future climate and while altering the mean and variance if desired. The
# P/B distribution is then deermined by running sampled E value through
# the fitted P/B vs E relationship. ![Figure 6: the P/B distributions used
#                                     in projections for the different climate scenarios. Black is the
#                                     baseline where the temperature distribution was fitted to all years; red
#                                     is shift in the mean 0.5 C warmer than the baseline; blue line is 0.5 C
#                                     colder than the baseline; green line has the same mean as the baseline
#                                     but a 50% increase in standard
#                                     deviation](README_files/figure-markdown_strict/PBprojplot-1.png)
#
# The plot of the projection given your E scenario for both density
# independent and dependent models. The biomass reference level is
# depicted as the horizontal dashed line. Confidence intervals are 90%.
# ![Figure 7: the projection of the baseline temperature scenario. Status
#   quo exploitation rate (mean 2014-2018) was
#   assumed.](README_files/figure-markdown_strict/projplotbase-1.png)
#
# What is clear from the projections is that a decrease in temperature
# from the baseline is not good for the stock while an increase in
# temperature is certainly not good. What is more surprising is that even
# if mean temperature is the same as the baseline but there is an
# important increase in variance of the temperature, this is perhaps the
# worst case for the stock. There is a larger perentage of the P/B density
# &lt;0 for the increased variance scenario than for any other scenario
# (Fig. 6).
# \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
#
# The probability that the biomass is greater than the reference level
# each year of the projection. The horizontal dashed line represents the
# risk tolerance of not achieving the objective. i.e. For the objective to
# be met, the biomass line should be above the risk line at the end of the
# time.frame period specified in the parameters list. ![Figure 8: the
#                                                       probability of achieving the objective each year during a 10 year
#                                                       projection of the status quo fishing rate and baseline temperature
#                                                       scenario.](README_files/figure-markdown_strict/PgtBrefplot-1.png)
#
# The maximum exploitation rate that will achieve the objective in the
# specified time period given the E scenario projected.
#
# ![Figure 9: The maximum exploitation rate that would allow the stock to
#   achieve the objective in the specified period of time at the specified
#   risk level for density independent and density dependent models and the
#   baseline temperature
#   scenario.](README_files/figure-markdown_strict/Fvalsbase-1.png)
#
# ![Figure 10: a contour plot showing the probability of achieveing the
#   objective in the specified time period under different exploitatin rates
#   and future temperature scenarios for the density independent model. The
#   actual time series of temperature an exploitation rate is shown as the
#   blue line
#   overlay.](README_files/figure-markdown_strict/contplot.di-1.png)

# Climate conditioning factors and deriving risk based advice
# -----------------------------------------------------------
#
#   You can determine climate conditioning buffer factors from the contour
# plot (Fig 10) for the particular objective. So if the acceptable risk
# level is 50% (a target) then one can follow the 0.5 contour to see what
# the maximum exploitation rate could be for various future temperatures.
# The buffering factor would be the ratio of the long term mean
# temperature exploitation on the 50% risk contour with that at the
# proposed future temperature. The buffering factor is the incremental
# risk buffer. If one were interested in incremental risk of status quo
# fishing then they would plot the exploitation rate of long term mean
# temperature and status quo exploitation and determine on what risk
# contour it landed. Then do the same with the hypothesised future median
# temperature and status quo exploitation rate. The difference between the
# two risk levels is the incremental risk.
#
# Incremental risk of assuming turbot production is affected by climate
# change vs not being affected can also be calculated by looking at how
# much the exploitation rate would need to change under one of the climate
# scenarios vs the null model. This incremental risk is the risk of
# assuming climate change is not important given your objective and future
# climate scenario.

# The contour plot and climate conditioning factors essentially show the
# dependence of reference points on the environmental conditions. Ideally
# a reference point is relatively invariant to external factors but the
# reality is that none are. The domed contours over the temperature
# dimension esssentially show degree of variance caused by change in the
# environment and climate change in what are essentially dynamic reference
# points.

