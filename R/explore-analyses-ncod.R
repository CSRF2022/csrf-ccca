# Second go through the methodology for Northern Cod.
# Try different settings in the param file

# Robyn Forrest. Sep 1, 2022

# Load the library

# library(devtools)
# install_github("duplisea/ccca")
library(ccca)
library(tidyverse)

# The data are in a file called ncod.rda
# The package pre-loads the data
# (several other species files in the package data folder)
# Model needs survey, catch and an E variable (e.g. Temp)
#View(ncod)
View(ncod)
#View(ShrimpGSL)

# the parameters and data come with the ccca package
# the mapply function puts them in the global environment
# Can change the values (e.g., params$Emean.shift=0.5)
View(params)

# change some things in params
data(params)
params$ref.years <- 1981:1989 # pick some years with average index before 1992
params$risk <- 0.75 # Set a higher prob of being above Blim
params$K <- 5 # assume stock more depleted than ncod

mapply(assign, names(params), params, MoreArgs=list(envir = globalenv()))
params

# Note there can't be missing years but there is no check for it
# But there can be NAs for some variables in some years

# Plot the N cod data
matplot(ncod$Year, cbind(ncod$Index,ncod$Catch),type="l",lty=c(1,1),lwd=2,xlab="Year",ylab="Survey biomass and catch (kt)",col=c("black","green"), main="Northern cod")
yaxis2.f(ncod$Year, ncod$E,ylabel=expression('NAO'),type="l",cex=1.1,lwd=2,lty=1,col="red")
legend("topleft",bty="n",legend=c("Survey","Catch","NAO"),lwd=2,lty=c(1),cex=0.7,col=c("black","green","red"))

# =========================================================================
# Determine the annual P/B ratio of the population creating a
# dataframe with the index bumped by q

PB= PB.f(ncod, ref.years=ref.years, q=q)

Indexq <- ncod$Index/q

# TODO: May want to smooth the index (Kalman filter or some other smoother)

# Simple surplus production function
# Pt = Bt+1 - Bt + Ct

# The diff function simply calculates Indexq+1 - Indexq
PB2 <- (diff(Indexq)+ncod$Catch[-length(ncod$Catch)])/Indexq[-length(Indexq)]
PB$PB

# Long version
PB3 <- vector(length=length(ncod$Catch)-1)
for(i in seq_along(PB3)) {
  P <- Indexq[i+1] - Indexq[i] + ncod$Catch[i]
  PB3[i] <- P/Indexq[i]
}

PBcompare <- cbind(PB2,PB3)
PBcompare #matches

# Kobe plot of PB vs E
 # I don't understand the legend of the ccca plot
 # Make a new version in this repo without it (Kobe2.f in Kobe2_plot.R)
# colours are the value of the environmental variable
# Added the years
source("R/Kobe2_plot.R")
Kobe2.f(PB=PB,E=PB$E, offset=-0.05)
colramp.legend(col1="red", col2="blue", ncol=length(PB$E), 2.5, 3.5, 2.7, 4.5)

# Make a Kobe plot of PB vs rel B
Kobe3.f(PB=PB,E=PB$E, offset=-0.05)
colramp.legend(col1="red", col2="blue", ncol=length(PB$E), 2.8, 1.3, 3., 2.)

#=========================================================================

# Fit a relationship between the P/B and the E variable
#   according to what you selected in params

# This might be the fun part. Lots of ways to explore the statistical fit

# model.type = the kind of model to fit ("poly", "gam", "gam.adaptive","avg","mpi","mpd","cx","cv","micx","micv","mdcx","mdcv","custom")
# knots = the number of knots for adaptive GAM
# poly.degree = the degree of the polynomial to fit
# custom.type = the type of model (ie. gam , scam , or lm) to use in custom model
# formula = the formula to use in custom model

# TODO: need to evaluate best fit model (AIC? BIC?)

# Function description:
# The various model fits: polynomial "poly", GAM "gam", adaptive GAM "gam.adaptive", various scam fits
#     and resamples from the PB values "avg" can be chosen. avg just fits a linear model with slope = 0
#     and then resamples the residuals which is effectively the same as just sampling the P/B values directly,
#     i.e. it does not force a relationship between P/B and E and therefore the future is just a resampling
#     of the past. scam (shape constrained additive models) fits force certain characteristics in the shape such as monotonicity,
#     convex, concave, increasing or decreasing.

# PBE.fit.f= function(PB , model.type, knots = NULL , poly.degree = NULL , custom.type = NULL , formula = NULL ){
#   PB=na.omit(PB)
#   switch(model.type,
#          poly= lm(PB~poly(E,degree=poly.degree),data= PB),
#          gam= gam(PB~s(E), data=PB),
#          gam.adaptive= gam(PB~s(E,k=knots,bs="ad"), data=PB),
#          mpi= scam(PB~s(E, bs="mpi"),data=PB),
#          mpd= scam(PB~s(E, bs="mpd"),data=PB),
#          cx= scam(PB~s(E, bs="cx"),data=PB),
#          cv= scam(PB~s(E, bs="cv"),data=PB),
#          micx= scam(PB~s(E, bs="micx"),data=PB),
#          micv= scam(PB~s(E, bs="micv"),data=PB),
#          mdcx= scam(PB~s(E, bs="mdcx"),data=PB),
#          mdcv= scam(PB~s(E, bs="mdcv"),data=PB),
#          avg= lm(PB - 0*E ~ 1, data=PB) ,
#          custom = do.call(custom.type , list(formula, data = PB))
#   )
# }

# The example in the package is a gam
PvsE= PBE.fit.f(PB,model.type=model.type, knots=knots,
                poly.degree=poly.degree)
model.type
knots
poly.degree

# Run a null model (average relationship, see switch function above)
PvsE.null= PBE.fit.f(PB,model.type="avg", knots=knots, poly.degree=poly.degree)

# plot the relationship
na.year= nrow(PB)
plot(na.omit(cbind(PB$E,PB$PB)),pch=20,xlab="E",ylab="P/B",
     col="darkgrey",type="n")
text(PB$E[-na.year],PB$PB[-na.year],PB$Year[-na.year],cex=.7)
pred.x= seq(min(PB$E)*.90,max(PB$E)*1.05,length=1000)
lines(pred.x,predict(PvsE.null,newdata=data.frame(E=pred.x)),lwd=2,col="grey")
lines(pred.x,predict(PvsE,newdata=data.frame(E=pred.x)),lwd=2)

# try a different model
model.type <- "gam.adaptive"
PvsE.ga= PBE.fit.f(PB,model.type=model.type, knots=knots,
                   poly.degree=poly.degree)
lines(pred.x,predict(PvsE.ga,newdata=data.frame(E=pred.x)),lwd=2, col=2)

model.type <- "poly"
poly.degree <- 2
PvsE.poly2= PBE.fit.f(PB,model.type=model.type, knots=knots,
                   poly.degree=poly.degree)
lines(pred.x,predict(PvsE.poly2,newdata=data.frame(E=pred.x)),lwd=2, col=3)

poly.degree <- 3
PvsE.poly3= PBE.fit.f(PB,model.type=model.type, knots=knots,
                      poly.degree=poly.degree)
lines(pred.x,predict(PvsE.poly3,newdata=data.frame(E=pred.x)),lwd=2, col=4)

model.type <- "mdcv"
PvsE.mdcv= PBE.fit.f(PB,model.type=model.type, knots=knots,
                      poly.degree=poly.degree)
lines(pred.x,predict(PvsE.mdcv,newdata=data.frame(E=pred.x)),lwd=2, col=6)

legend("topright",bty="n",legend=c("null","gam", "gam.adaptive","polynomial 2","polynomial 3","mdcv"), col=c("lightgray",1:4,6),lwd=2)

AIC.null<-AIC(PvsE.null)
AIC.gam <- AIC(PvsE)
AIC.ga <- AIC(PvsE.ga)
AIC.poly2 <- AIC(PvsE.poly2)
AIC.poly3 <- AIC(PvsE.poly3)
AIC.mdcv <- AIC(PvsE.mdcv)

allAIC <- data.frame(
  Model = c("null","gam", "gam.adaptive","polynomial 2","polynomial 3","mdcv"),
  AIC =c(AIC.null,AIC.gam,AIC.ga,AIC.poly2,AIC.poly3,AIC.mdcv)
  )

allAIC

#=============================================================================
# Fit a normal distribution to the E time series and pull the parameters out.
# This is for projecting future climate variables

# uses function norm.fit.f, where E is the environmental variable
# norm.fit.f= function(E){
#   E= E[!is.na(E)]
#   lnf= fitdistr(E,densfun="normal")
#   lnf
# }

Enorm= norm.fit.f(E=PB$E)
Enorm
Edist.a=Enorm$estimate[1] #mean
Edist.b=Enorm$estimate[2] #sd


# Plot the distributions
Nrand=1000000
Edist.a=Enorm$estimate[1]
Edist.b=Enorm$estimate[2]
Enormplot=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=0,E.var.inc=1))
plot(Enormplot, xlab=expression('NAO'), ylab="Density",lwd=2,main="")

#=============================================================================
# Develop the E projection values based on your choice of projection parameters
# param E.dist.a The mean of the normal distribution
# param E.dist.b The standard deviation of the normal distribution
# param Emean.shift The shift if the mean. This is done rather than changing the mean directly so it is generic for variety of distributions
# param proj.years The number of years to project into the future
# param N The number of different realisations of the future to create

# Eprojnorm.f= function(Edist.a, Edist.b, Emean.shift=1, proj.years, N){
#   E= matrix(rnorm(proj.years*N,mean=Edist.a,sd=Edist.b)+Emean.shift,ncol=N,nrow=proj.years)
#   E
# }

# NOTE: There is also a multivariate normal version that I haven't
# looked at called Eprojnorm.f.mv

# Default values
Emean.shift #0
proj.years #10
N #2000

Eproj= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b,
                   Emean.shift=Emean.shift,
                   proj.years=proj.years, N)

# Shift to a higher mean (this is nao, not necessarily warmer)
Emean.shift.higher=0.5
Eproj.higher= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b,
                        Emean.shift=Emean.shift.higher,
                        proj.years=proj.years, N)

# Shift to a lower mean
Emean.shift.lower=-0.5
Eproj.lower= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b,
                        Emean.shift=Emean.shift.lower,
                        proj.years=proj.years, N)

# Increase the variance
Emean.shift=0.0
E.var.inc=1.5
Edist.b= Edist.b*E.var.inc
Eproj.var= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift,
                       proj.years=proj.years, N)

# Plot the shifted normal distributions
Nrand=1000000
Edist.a=Enorm$estimate[1]
Edist.b=Enorm$estimate[2]
Ebase=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=0,E.var.inc=1))
Ehigher=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=Emean.shift.higher,E.var.inc=1))
Elower=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=Emean.shift.lower,E.var.inc=1))
Evar=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=0,E.var.inc=1.5))
plot(Ebase, xlab=expression('NAO'), ylab="Density",lwd=2,main="")
lines(Ehigher, lwd=2,col="red")
lines(Elower, lwd=2,col="blue")
lines(Evar, lwd=2,col="green")

#==========================================================================
# Develop the P/B projection values based on the P/B vs E fit and the
# E projection you specified above. When add.residuals=1 then a
# residual is sampled for each time step of each projection and is
# added to the P/B values. If the P/B vs E fit is relatively unbiased then
# it will not make much difference to the median but it will increase the
# future uncertainty. If you set it =0 then the residual is not added and
# you will have less uncertainty in the future.
# If the P/B vs E model is biased, this bias will be more likely to be
# carried forward if residuals are not sampled (=0).

add.resids # default is to add residuals

PBproj= PB.for.projection.f(PvsE=PvsE,Eproj,add.residuals=add.resids)
PBproj.null= PB.for.projection.f(PvsE=PvsE.null,Eproj,add.residuals=add.resids)
PBproj.higher= PB.for.projection.f(PvsE=PvsE,Eproj.higher,add.residuals=add.resids)
PBproj.lower= PB.for.projection.f(PvsE=PvsE,Eproj.lower,add.residuals=add.resids)
PBproj.var= PB.for.projection.f(PvsE=PvsE,Eproj.var,add.residuals=add.resids)


# The distribution of the P/B values for the future projections.
# It is the P/B that results by sampling E distribution function to
# simulate a future climate and while altering the mean and variance
# if desired. The P/B distribution is then determined by running sampled
# E value through the fitted P/B vs E relationship.
plot(density(PBproj.null),xlab="P/B",ylab="Density",lwd=2,main="",col="grey")
lines(density(PBproj.lower),lwd=2,col="blue")
lines(density(PBproj.higher),lwd=2,col="red")
lines(density(PBproj.var),lwd=2,col="green")
lines(density(PBproj),lwd=2,col="black")
legend("topright",legend=c("Null", "Base","lower","higher","Var"), col=c("grey", "black","blue","red","green"), lty=1,lwd=2,bty="n")

#=========================================================================
# Fishing strategy
# In this case is the the mean exploitation (Index corrected by catchability) during the last five years.
# This function just takes the mean relative F from the PB object
# or turns off fishing for the stated years if moratorium=T
Fstrat= F.strategy(PB, 2015:2017, moratorium=F)
Fstrat

#=========================================================================
# Now run the projection given all the above calculations and parameters.
# The trajectory over the projection period and the probability that B
# is greater than the objective after the specified time range.

# What does this function do?
# Looks like a simple surplus production model
# With and without density dependence
# Uses the reference Fstrat
Bproj= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj, Fstrat, K=K, theta=1)

# Now get the final Biomass under a sequence of F values for each
#   climate scenario (the 2000 MC runs)
# RF added Bstart.mult and theta args, which were missing and caused errors
# Fout isn't a very good name for this output, which is actually final Biomass scenarios
Fout= Fseq.f(PB,Bstart.mult=Bstart.mult,PBproj=PBproj,Fseq=fs,time.frame=time.frame, N=N, K=K, theta=1)

# Now get the probability of being at or above the reference B value
# at the end of the time frame specified.
PofF= PofF.f(PB,Fout,ref.pt=ref.pt)
View(PofF)
# Now have the probability of being above the reference biomass
# in the final projection year for a set of constant F policies (PofF)
#=============================================================================

# Now we are back at the annual biomasses under Fstrat

# Summarise the output of the projections by calculating quantiles
#  and putting results in a dataframe.
# Calculates the probability of being at or above a reference point.
# Not sure what Fs are used here. Maybe reference F?
Bproj.summary= Bproj.summary.f(PB,Bproj,PBproj,Eproj)
P= rankprob.f(Bproj,PB,ref.pt)

#=============================================================================

# Now do it all again for the other climate scenarios

# Null
PvsE.null= PBE.fit.f(PB,model.type="avg", knots=knots, poly.degree=poly.degree)
PBproj.null= PB.for.projection.f(PvsE=PvsE.null,Eproj,add.residuals=add.resids)
Bproj.null= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj.null, Fstrat, K=K, theta=1)
# RF added Bstart.mult and theta args
Fout.null= Fseq.f(PB,Bstart.mult=Bstart.mult,PBproj=PBproj.null,Fseq=fs,time.frame=time.frame, N=N, K=K,theta=1)
PofF.null= PofF.f(PB,Fout.null,ref.pt=ref.pt)
Bproj.summary.null= Bproj.summary.f(PB,Bproj.null,PBproj.null,Eproj)
P.null= rankprob.f(Bproj.null,PB,ref.pt)

# higher
Emean.shift.higher=0.5
Eproj.higher= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift.higher, proj.years=proj.years, N)
PBproj.higher= PB.for.projection.f(PvsE=PvsE,Eproj.higher,add.residuals=add.resids)
Bproj.higher= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj.higher, Fstrat, K=K, theta=1)
# RF added Bstart.mult and theta args
Fout.higher= Fseq.f(PB, Bstart.mult=Bstart.mult, PBproj=PBproj.higher,Fseq=fs,time.frame=time.frame, N=N, K=K, theta=1)
PofF.higher= PofF.f(PB,Fout.higher,ref.pt=ref.pt)
Bproj.summary.higher= Bproj.summary.f(PB,Bproj.higher,PBproj.higher,Eproj.higher)
P.higher= rankprob.f(Bproj.higher,PB,ref.pt)

# lower
Emean.shift.lower=-0.5
Eproj.lower= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift.lower, proj.years=proj.years, N)
PBproj.lower= PB.for.projection.f(PvsE=PvsE,Eproj.lower,add.residuals=add.resids)
Bproj.lower= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj.lower, Fstrat, K=K, theta=1)
# RF added Bstart.mult and theta arg
Fout.lower= Fseq.f(PB,Bstart.mult=Bstart.mult,PBproj=PBproj.lower,Fseq=fs,time.frame=time.frame, N=N, K=K, theta=1)
PofF.lower= PofF.f(PB,Fout.lower,ref.pt=ref.pt)
Bproj.summary.lower= Bproj.summary.f(PB,Bproj.lower,PBproj.lower,Eproj.lower)
P.lower= rankprob.f(Bproj.lower,PB,ref.pt)

# Increase variance
Emean.shift=0.0
E.var.inc=1.5
Edist.b= Edist.b*E.var.inc
Eproj.var= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift,
                       proj.years=proj.years, N)
PBproj.var= PB.for.projection.f(PvsE=PvsE,Eproj.var,add.residuals=add.resids)
Bproj.var= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj.var, Fstrat, K=K, theta=1)
# RF added Bstart.mult and theta arg
Fout.var= Fseq.f(PB, Bstart.mult=Bstart.mult, Fseq=fs,PBproj=PBproj.var,time.frame=time.frame, N=N, K=K, theta=1)
PofF.var= PofF.f(PB,Fout.var,ref.pt=ref.pt)
Bproj.summary.var= Bproj.summary.f(PB,Bproj.var,PBproj.var,Eproj.var)
P.var= rankprob.f(Bproj.var,PB,ref.pt)

# need to reset the e distribution parameters to run the full F, E simulation
Edist.a=Enorm$estimate[1]
Edist.b=Enorm$estimate[2]

#===========================================================================
#Run multiple simulations for each combination of E scenario and F.

# First the E and PB scenarios are set up and then multiple projections
# are made based on the exploitation rate series specified in the
# parameters file.

# Note that the Emean.shifts and N.CCF arguments are in the params object
# This function just loops through the Emean.shifts vector to get normal
# distributions of the E object for the projections using Eprojnorm.f()
# Produces a list of E matrices proj.years X N
# So we have N projections for each series from cooler to higherer
ECCF= Eproj.list.f(Emean.shifts=Emean.shifts, N=N.CCF, proj.years=proj.years, Edist.a=Edist.a,
                   Edist.b=Edist.b)

# Now get the predicted PB for all those E series
# Uses the PvsE predictive relationship from earlier, with all the E
# series
# So we get a proj.years X N matrix of PB predictions for each
#  future Emean scenario
PBCCF= PBproj.list.f(PvsE=PvsE, Eprojection=ECCF)

# Now we use these two lists to get the probability of being above Bref
#  in the final year across the range of Fs ... for EACH Emean shift scenario
# This is a matrix like the P matrix above, but all the climate scenarios
# are rbinded.
# It's essentially like rbinding P, P.lower, P.higher etc but across a big
# range of Emean shifts instead of just +0.5 and -0.5
CCF.raw= P.R.for.EF.f(E.CCF=ECCF, PB.CCF=PBCCF, Fs=fs, PB=PB, ref.pt=ref.pt, Bstart.mult=Bstart.mult,
                      K=K, theta=theta)
View(CCF.raw)

# ===========================================================================================
# That's it for analysis. Now need to look at the final plots to see how these
# results are recommended to be used (the last three figs of the readme at
#  https://github.com/duplisea/ccca)

# 1. Time series plots (use the Fstrat fishing mortality)
# The plot of the projection given your E scenario for both density
#  independent and dependent models. The biomass reference level is
#  depicted as the horizontal dashed line. Confidence intervals are 90%.
# Because these are time series into the projection period, they use
#   the Bproj object, which was projected with the Fstrat fishing mortality

# Just look at one to begin with
# The null case (no relationship between E and PB)
Bref= ref.pt*sum(PB$Index.q * PB$refererence.years)/sum(PB$refererence.years)
plot(Bproj.summary.null$year,Bproj.summary.null$B.di.CI.med,type="n",ylim=c(0,max(Bproj.summary.null$B.di.CI.high)),xlab="",ylab="")
confint(Bproj.summary.null$year,Bproj.summary.null$B.di.CI.low,Bproj.summary.null$B.di.CI.high,col="grey")
confint(Bproj.summary.null$year,Bproj.summary.null$B.dd.CI.low,Bproj.summary.null$B.dd.CI.high,col="lightblue")
lines(Bproj.summary.null$year,Bproj.summary.null$B.dd.CI.med,lwd=2,col="blue")
lines(Bproj.summary.null$year,Bproj.summary.null$B.dd.CI.low,lwd=1,lty=2,col="blue")
lines(Bproj.summary.null$year,Bproj.summary.null$B.dd.CI.high,lwd=1,lty=2,col="blue")
lines(Bproj.summary.null$year,Bproj.summary.null$B.di.CI.med,lwd=2)
lines(Bproj.summary.null$year,Bproj.summary.null$B.di.CI.low,lwd=1,lty=2,col="black")
lines(Bproj.summary.null$year,Bproj.summary.null$B.di.CI.high,lwd=1,lty=2,col="black")
lines(Bproj.summary.null$year,rep(Bref,length(Bproj.summary.null$year)),lty=2)
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="Null",bty="n",cex=0.75)

# The base case (Future E is average)
Bref= ref.pt*sum(PB$Index.q * PB$refererence.years)/sum(PB$refererence.years)
plot(Bproj.summary$year,Bproj.summary$B.di.CI.med,type="n",ylim=c(0,max(Bproj.summary.null$B.di.CI.high)),xlab="",ylab="")
confint(Bproj.summary$year,Bproj.summary$B.di.CI.low,Bproj.summary$B.di.CI.high,col="grey")
confint(Bproj.summary$year,Bproj.summary$B.dd.CI.low,Bproj.summary$B.dd.CI.high,col="lightblue")
lines(Bproj.summary$year,Bproj.summary$B.dd.CI.med,lwd=2,col="blue")
lines(Bproj.summary$year,Bproj.summary$B.dd.CI.low,lwd=1,lty=2,col="blue")
lines(Bproj.summary$year,Bproj.summary$B.dd.CI.high,lwd=1,lty=2,col="blue")
lines(Bproj.summary$year,Bproj.summary$B.di.CI.med,lwd=2)
lines(Bproj.summary$year,Bproj.summary$B.di.CI.low,lwd=1,lty=2,col="black")
lines(Bproj.summary$year,Bproj.summary$B.di.CI.high,lwd=1,lty=2,col="black")
lines(Bproj.summary$year,rep(Bref,length(Bproj.summary$year)),lty=2)
legend("topleft",legend="Mean NAO",bty="n",cex=0.75)
yaxis2.f(Bproj.summary$year,Bproj.summary$ E.CI.med,ylabel="",type="l",cex=1,,lwd=2,lty=1,col="red")

# Future E is higherer ... mean of normal dist shifts by 0.5 deg
Bref= ref.pt*sum(PB$Index.q * PB$refererence.years)/sum(PB$refererence.years)
plot(Bproj.summary.higher$year,Bproj.summary.higher$B.di.CI.med,type="n",ylim=c(0,max(Bproj.summary.null$B.di.CI.high)),xlab="",ylab="")
confint(Bproj.summary.higher$year,Bproj.summary.higher$B.di.CI.low,Bproj.summary.higher$B.di.CI.high,col="grey")
confint(Bproj.summary.higher$year,Bproj.summary.higher$B.dd.CI.low,Bproj.summary.higher$B.dd.CI.high,col="lightblue")
lines(Bproj.summary.higher$year,Bproj.summary.higher$B.dd.CI.med,lwd=2,col="blue")
lines(Bproj.summary.higher$year,Bproj.summary.higher$B.dd.CI.low,lwd=1,lty=2,col="blue")
lines(Bproj.summary.higher$year,Bproj.summary.higher$B.dd.CI.high,lwd=1,lty=2,col="blue")
lines(Bproj.summary.higher$year,Bproj.summary.higher$B.di.CI.med,lwd=2)
lines(Bproj.summary.higher$year,Bproj.summary.higher$B.di.CI.low,lwd=1,lty=2,col="black")
lines(Bproj.summary.higher$year,Bproj.summary.higher$B.di.CI.high,lwd=1,lty=2,col="black")
lines(Bproj.summary.higher$year,rep(Bref,length(Bproj.summary.higher$year)),lty=2)
legend("topleft",legend="NAO higher",bty="n",cex=0.75)
yaxis2.f(Bproj.summary.higher$year,Bproj.summary.higher$ E.CI.med,ylabel="",type="l",cex=1,,lwd=2,lty=1,col="red")

# Future E is cooler ... mean of normal dist shifts by -0.5 deg
Bref= ref.pt*sum(PB$Index.q * PB$refererence.years)/sum(PB$refererence.years)
plot(Bproj.summary.lower$year,Bproj.summary.lower$B.di.CI.med,type="n",ylim=c(0,max(Bproj.summary.lower$B.di.CI.high)),xlab="",ylab="")
confint(Bproj.summary.lower$year,Bproj.summary.lower$B.di.CI.low,Bproj.summary.lower$B.di.CI.high,col="grey")
confint(Bproj.summary.lower$year,Bproj.summary.lower$B.dd.CI.low,Bproj.summary.lower$B.dd.CI.high,col="lightblue")
lines(Bproj.summary.lower$year,Bproj.summary.lower$B.dd.CI.med,lwd=2,col="blue")
lines(Bproj.summary.lower$year,Bproj.summary.lower$B.dd.CI.low,lwd=1,lty=2,col="blue")
lines(Bproj.summary.lower$year,Bproj.summary.lower$B.dd.CI.high,lwd=1,lty=2,col="blue")
lines(Bproj.summary.lower$year,Bproj.summary.lower$B.di.CI.med,lwd=2)
lines(Bproj.summary.lower$year,Bproj.summary.lower$B.di.CI.low,lwd=1,lty=2,col="black")
lines(Bproj.summary.lower$year,Bproj.summary.lower$B.di.CI.high,lwd=1,lty=2,col="black")
lines(Bproj.summary.lower$year,rep(Bref,length(Bproj.summary.lower$year)),lty=2)
legend("topleft",legend="NAO lower",bty="n",cex=0.75)
yaxis2.f(Bproj.summary.lower$year,Bproj.summary.lower$ E.CI.med,ylabel="",type="l",cex=1,,lwd=2,lty=1,col="red")

# E variance increases ... sd of normal dist shifts by 1.5 deg
Bref= ref.pt*sum(PB$Index.q * PB$refererence.years)/sum(PB$refererence.years)
plot(Bproj.summary.var$year,Bproj.summary.var$B.di.CI.med,type="n",ylim=c(0,max(Bproj.summary.var$B.di.CI.high)),xlab="",ylab="")
confint(Bproj.summary.var$year,Bproj.summary.var$B.di.CI.low,Bproj.summary.var$B.di.CI.high,col="grey")
confint(Bproj.summary.var$year,Bproj.summary.var$B.dd.CI.low,Bproj.summary.var$B.dd.CI.high,col="lightblue")
lines(Bproj.summary.var$year,Bproj.summary.var$B.dd.CI.med,lwd=2,col="blue")
lines(Bproj.summary.var$year,Bproj.summary.var$B.dd.CI.low,lwd=1,lty=2,col="blue")
lines(Bproj.summary.var$year,Bproj.summary.var$B.dd.CI.high,lwd=1,lty=2,col="blue")
lines(Bproj.summary.var$year,Bproj.summary.var$B.di.CI.med,lwd=2)
lines(Bproj.summary.var$year,Bproj.summary.var$B.di.CI.low,lwd=1,lty=2,col="black")
lines(Bproj.summary.var$year,Bproj.summary.var$B.di.CI.high,lwd=1,lty=2,col="black")
lines(Bproj.summary.var$year,rep(Bref,length(Bproj.summary.var$year)),lty=2)
legend("topleft",legend="NAO sd x 1.5",bty="n",cex=0.75)
yaxis2.f(Bproj.summary.var$year,Bproj.summary.var$ E.CI.med,ylabel="",type="l",cex=1,,lwd=2,lty=1,col="red")

# Not sure I agree with the summary that lowerer temperatures are worse for
# the stock. Looks a bit better than the null and mean cases to me

# Now look at the probability that the biomass is greater than the
# reference level each year of the projection.
# The horizontal dashed line represents the risk tolerance of not achieving
# the objective. i.e. For the objective to be met, the biomass line
# should be above the risk line at the end of the time.frame period specified
# in the parameters list.

# ** So these show the probabilities of stock recovery under the current
#      F strategy **

# Null case - no relationship between E and Production
matplot(P.null[,1],P.null[,-1],type='l',xlab="",ylab="",lwd=3,ylim=c(0,1),lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="Null",bty="n",cex=0.75)
abline(h=1-risk,lty=2,col="grey")
box()

# Mean temperature
matplot(P[,1],P[,-1],type='l',xlab="",ylab="",lwd=3,ylim=c(0,1),lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="Mean NAO",bty="n",cex=0.75)
abline(h=1-risk,lty=2,col="grey")
box()

# higher
matplot(P.higher[,1],P.higher[,-1],type='l',xlab="",ylab="",lwd=3,ylim=c(0,1),lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="NAO higher",bty="n",cex=0.75)
abline(h=1-risk,lty=2,col="grey")
box()

# lower
matplot(P.lower[,1],P.lower[,-1],type='l',xlab="",ylab="",lwd=3,ylim=c(0,1),lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="NAO lower",bty="n",cex=0.75)
abline(h=1-risk,lty=2,col="grey")
box()

# Var
matplot(P.var[,1],P.var[,-1],type='l',xlab="",ylab="",lwd=3,ylim=c(0,1),lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="NAO sd x 1.5",bty="n",cex=0.75)
abline(h=1-risk,lty=2,col="grey")
box()

#=============================================================================
# Now we want the maximum fishing mortality that will achieve the
# objective in the specified time period given the E scenario projected.

# Uses a simple gam to predict fishing mortality that will meet
# stated risk tolerance level

# So in the higher and var cases, there is no fishing level that will
# rebuild  the stock with 50% prob of being above F target

# Null case (no E vs PB relationship)
matplot(PofF.null[,1],PofF.null[,-1],xlab="Exploitation rate", ylab="Probability of being at or above biomass objective in 10 years" ,ylim=c(0,1),
        type="l",lwd=3,xaxs="i",yaxs="i",lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="Null",bty="n",cex=0.75)
di.intersection= predict(gam(f~s(P.di),data=PofF.null),newdata=data.frame(P.di=1-risk))
dd.intersection= predict(gam(f~s(P.dd),data=PofF.null),newdata=data.frame(P.dd=1-risk))
rect(0,0,di.intersection,1-risk,lty=2,border="darkgrey")
rect(0,0,dd.intersection,1-risk,lty=2,border="darkgrey")
box()

# mean
matplot(PofF[,1],PofF[,-1],xlab="Exploitation rate", ylab="Probability of being at or above biomass objective in 10 years" ,ylim=c(0,1),
        type="l",lwd=3,xaxs="i",yaxs="i",lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="Mean NAO",bty="n",cex=0.75)
di.intersection= predict(gam(f~s(P.di),data=PofF),newdata=data.frame(P.di=1-risk))
dd.intersection= predict(gam(f~s(P.dd),data=PofF),newdata=data.frame(P.dd=1-risk))
rect(0,0,di.intersection,1-risk,lty=2,border="darkgrey")
rect(0,0,dd.intersection,1-risk,lty=2,border="darkgrey")
box()

# higher
matplot(PofF.higher[,1],PofF.higher[,-1],xlab="Exploitation rate", ylab="Probability of being at or above biomass objective in 10 years" ,ylim=c(0,1),
        type="l",lwd=3,xaxs="i",yaxs="i",lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="NAO higher",bty="n",cex=0.75)
di.intersection= predict(gam(f~s(P.di),data=PofF.higher),newdata=data.frame(P.di=1-risk))
dd.intersection= predict(gam(f~s(P.dd),data=PofF.higher),newdata=data.frame(P.dd=1-risk))
rect(0,0,di.intersection,1-risk,lty=2,border="darkgrey")
rect(0,0,dd.intersection,1-risk,lty=2,border="darkgrey")
box()

# lower
matplot(PofF.lower[,1],PofF.lower[,-1],xlab="Exploitation rate", ylab="Probability of being at or above biomass objective in 10 years" ,ylim=c(0,1),
        type="l",lwd=3,xaxs="i",yaxs="i",lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="NAO lower",bty="n",cex=0.75)
di.intersection= predict(gam(f~s(P.di),data=PofF.lower),newdata=data.frame(P.di=1-risk))
dd.intersection= predict(gam(f~s(P.dd),data=PofF.lower),newdata=data.frame(P.dd=1-risk))
rect(0,0,di.intersection,1-risk,lty=2,border="darkgrey")
rect(0,0,dd.intersection,1-risk,lty=2,border="darkgrey")
box()

# increased variance
matplot(PofF.var[,1],PofF.var[,-1],xlab="Exploitation rate", ylab="Probability of being at or above biomass objective in 10 years" ,ylim=c(0,1),
        type="l",lwd=3,xaxs="i",yaxs="i",lty=1,col=c("black","blue"))
legend("topright",legend=c("Density independent","Density dependent"),lwd=2,col=c("black","blue"),bty="n",cex=0.75)
legend("topleft",legend="NAO sd x 1.5",bty="n",cex=0.75)
di.intersection= predict(gam(f~s(P.di),data=PofF.var),newdata=data.frame(P.di=1-risk))
dd.intersection= predict(gam(f~s(P.dd),data=PofF.var),newdata=data.frame(P.dd=1-risk))
rect(0,0,di.intersection,1-risk,lty=2,border="darkgrey")
rect(0,0,dd.intersection,1-risk,lty=2,border="darkgrey")
box()

#=============================================================================
# Now a contour plot showing the probability of achieving the objective
# in the specified time period under different exploitatin rates and future
# temperature scenarios for the density independent model.
# The actual time series of temperature an exploitation rate is shown as
# the blue line overlay

CCF.contour=interp(x=CCF.raw$E.med,y=CCF.raw$Fval,z=CCF.raw$P.di)
contour(CCF.contour,xlab="Median NAO",ylab="Exploitation rate",xaxs="i",yaxs="i")
risk.equi.exp.rate.di= contourLines(CCF.contour$x,CCF.contour$y,CCF.contour$z,nlevels=1,levels=1-risk)
confint(risk.equi.exp.rate.di[[1]]$x,risk.equi.exp.rate.di[[1]]$y*0,risk.equi.exp.rate.di[[1]]$y,col=rgb(0, 1, 0,0.5))
lines(PB$E,PB$F.rel,col="slateblue",lwd=1)
points(PB$E,PB$F.rel,col="slateblue",pch=20)
year.endpoints= match(range(PB$Year),PB$Year)
points(PB$E[year.endpoints],PB$F.rel[year.endpoints],pch=22,cex=3,bg="white",col="slateblue")
text(PB$E[year.endpoints],PB$F.rel[year.endpoints],PB$Year[year.endpoints],cex=.5)

