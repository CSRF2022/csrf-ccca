# Some preliminary explorations of what the functions are actually doing.
# See READMEccca.Rmd for fully worked Greenland halibut example

# Robyn Forrest. August 29, 2022

# Load the library

# library(devtools)
# install_github("duplisea/ccca")
library(ccca)
library(tidyverse)

# the parameters come with the ccca package
# the mapply function puts them in the global environment
# Can change the values (e.g., params$Emean.shift=0.5)
data(params)
mapply(assign, names(params), params, MoreArgs=list(envir = globalenv()))
params

# The data are in a file called turbot.rda
# The package pre-loads the data
# (several other species files in the package data folder)
# Model needs survey, catch and an E variable (e.g. Temp)
#View(turbot)
#View(ncod)
#View(ShrimpGSL)

# Note there can't be missing years but there is no check for it
# But there can be NAs for some variables in some years

# Plot the Greenland Halibut data
matplot(turbot$Year, cbind(turbot$Index,turbot$Catch),type="l",lty=c(1,1),lwd=2,xlab="Year",ylab="Survey biomass and catch (kt)",col=c("black","green"), main="Greenland halibut")
yaxis2.f(turbot$Year, turbot$E,ylabel=expression('Temperature ('^o*C*')'),type="l",cex=1.1,lwd=2,lty=1,col="red")
legend("topleft",bty="n",legend=c("Survey","Catch","Temperature"),lwd=2,lty=c(1),cex=0.7,col=c("black","green","red"))

# Plot the N cod data
matplot(ncod$Year, cbind(ncod$Index,ncod$Catch),type="l",lty=c(1,1),lwd=2,xlab="Year",ylab="Survey biomass and catch (kt)",col=c("black","green"), main="Northern cod")
yaxis2.f(ncod$Year, ncod$E,ylabel=expression('Temperature ('^o*C*')'),type="l",cex=1.1,lwd=2,lty=1,col="red")
legend("topleft",bty="n",legend=c("Survey","Catch","Temperature"),lwd=2,lty=c(1),cex=0.7,col=c("black","green","red"))

# =========================================================================
# Determine the annual P/B ratio of the population creating a
# dataframe with the index bumped by q

# What does the PB.f function do?
# A simple index of abundance inflated by dividing by q
# Relative F is determined by dividing catch by inflated index

# COULD USE BIOMASS SERIES FROM A STOCK ASSESSMENT
# OR POSSIBLY Q FROM STOCK ASSESSMENT - see if scale is important

PB= PB.f(turbot, ref.years=ref.years, q=q)

#View(PB)

# Recreate what the function does
# ref.years and q are from the parameter dataset
ref.years
q

Indexq <- turbot$Index/q
Indexq # q is 1 in this case

# TODO: May want to smooth the index (Kalman filter or some other smoother)

# Simple surplus production function
# Pt = Bt+1 - Bt + Ct

# The diff function simply calculates Indexq+1 - Indexq
PB2 <- (diff(Indexq)+turbot$Catch[-length(turbot$Catch)])/Indexq[-length(Indexq)]
PB$PB

# Long version
PB3 <- vector(length=length(turbot$Catch)-1)
for(i in seq_along(PB3)) {
  P <- Indexq[i+1] - Indexq[i] + turbot$Catch[i]
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
Kobe2.f(PB=PB,E=PB$E, offset=0.075)
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

# Could use another distribution, e.g., with a fatter tail, e.g., gamma, lognormal
# Gamma and lognormal are built in,
#  but there is also a function called E.dist.fit.f that can be used
#   for other distributions
Egamma =Egamma.fit.f(E=PB$E)
Egamma.a=Egamma$estimate[1] #shape
Egamma.b=Egamma$estimate[2] #rate

Elnorm =lnorm.fit.f(E=PB$E)
Elnorm.a=Elnorm$estimate[1] #meanlog
Elnorm.b=Elnorm$estimate[2] #sdlog

# Plot the distributions
Nrand=1000000
source("R/rlnorm_plot.R")
Edist.a=Enorm$estimate[1]
Edist.b=Enorm$estimate[2]
Enormplot=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=0,E.var.inc=1))
plot(Enormplot, xlab=expression('Temperature('^o*C*')'), ylab="Density",xlim=c(0,6),lwd=2,main="")
Egammaplot=density(Egamma.plot.f(Nrand=Nrand, shape=Egamma.a, rate=Egamma.b,Emean.shift=0,E.variance=1))
lines(Egammaplot, col=2, lwd=2)
Elnormplot=density(lnorm.plot.f(Nrand=Nrand, Edist.a=Elnorm.a, Edist.b=Elnorm.b,Emean.shift=0,E.var.inc=1))
lines(Elnormplot, col=3, lwd=2)
legend("topright", legend=c("Normal","Gamma","Lognormal"), col=1:3, lwd=2,bty="n")

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

# Shift to a warmer mean
Emean.shift.warm=0.5
Eproj.warm= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b,
                        Emean.shift=Emean.shift.warm,
                        proj.years=proj.years, N)

# Shift to a cooler mean
Emean.shift.cold=-0.5
Eproj.cold= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b,
                        Emean.shift=Emean.shift.cold,
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
Ewarm=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=Emean.shift.warm,E.var.inc=1))
Ecold=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=Emean.shift.cold,E.var.inc=1))
Evar=density(norm.plot.f(Nrand=Nrand, Edist.a=Edist.a, Edist.b=Edist.b,Emean.shift=0,E.var.inc=1.5))
plot(Ebase, xlab=expression('Temperature('^o*C*')'), ylab="Density",xlim=c(0,6),lwd=2,main="")
lines(Ewarm, lwd=2,col="red")
lines(Ecold, lwd=2,col="blue")
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
PBproj.warm= PB.for.projection.f(PvsE=PvsE,Eproj.warm,add.residuals=add.resids)
PBproj.cold= PB.for.projection.f(PvsE=PvsE,Eproj.cold,add.residuals=add.resids)
PBproj.var= PB.for.projection.f(PvsE=PvsE,Eproj.var,add.residuals=add.resids)

#View(PBproj)

# What does this function do?
# Basically, makes a prediction of PB based on PvsE prediction
# Adds residuals or not

# PB.for.projection.f= function(PvsE, Eproj, add.residuals=add.resids){
#   median.prediction= Eproj*-9999
#   for (i in 1:ncol(Eproj)){
#     newdat= Eproj[,i]
#     median.prediction[,i]= predict(PvsE, newdata=data.frame(E=newdat))
#   }
#   error.prediction= PvsE.resids.f(PvsE,proj.years=nrow(Eproj),N=ncol(Eproj))*add.residuals
#   PB.prediction= median.prediction+error.prediction
#   PB.prediction
# }

# PvsE.resids.f= function(PvsE, proj.years, N){
#   resids= matrix(sample(residuals(PvsE),proj.years*N,replace=T),ncol=N,nrow=proj.years)
#   resids
# }

# The distribution of the P/B values for the future projections.
# It is the P/B that results by sampling E distribution function to
# simulate a future climate and while altering the mean and variance
# if desired. The P/B distribution is then determined by running sampled
# E value through the fitted P/B vs E relationship.
plot(density(PBproj.null),xlab="P/B",ylab="Density",lwd=2,main="",col="grey")
lines(density(PBproj.cold),lwd=2,col="blue")
lines(density(PBproj.warm),lwd=2,col="red")
lines(density(PBproj.var),lwd=2,col="green")
lines(density(PBproj),lwd=2,col="black")
legend("topright",legend=c("Null", "Base","Cold","Warm","Var"), col=c("grey", "black","blue","red","green"), lty=1,lwd=2,bty="n")

#=========================================================================
# Fishing strategy
# In this case is the the mean exploitation (Index corrected by catchability) during the last five years.
# This function just takes the mean relative F from the PB object
# or turns off fishing for the stated years if moratorium=T
Fstrat= F.strategy(PB, 2014:2018, moratorium=F)
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

# Performs the projection with climate scenario and fishing strategy
# @param PB the data and model fit coming from applying the PB model (PB.f)
# @param Bstart.mult the proporption of the last data year's biomass
#     used to start the projection
# @param PBproj The matrix of projected PB values based on climate scenario
# @param Fstrat The fishing strategy
# @param K The multiplier of maximum observed biomass to be carrying capacity
# @param theta the skewness of the density dependence factor (1=Schaeffer)

# Density independent
# Bt+1 = Bt + Bt*PBt - Bt*Ft

# Density dependent
# If PB < 0:  Bt+1 = Bt + Bt*PBt - Bt*Ft
# If PB >= 0: Bt+1 = Bt + Bt*PBt*(1-(Bt/K)^theta) - Bt*Ft

# projection.f= function(PB, Bstart.mult, PBproj, Fstrat, K, theta=1){
#   if(theta<=0) stop('theta must be >0 and probably should be <=1')
#   K= K*max(PB$Index.q, na.rm=T)
#   N= ncol(PBproj)
#   proj.years= nrow(PBproj)
#   proj.di= matrix(ncol=N,nrow=proj.years)
#   proj.dd= matrix(ncol=N,nrow=proj.years)
#   for (MC in 1:N){
#     B.di= tail(PB$Index.q,1)*Bstart.mult
#     B.dd= B.di
#     for (i in 1:proj.years){
#       PB.ratio= PBproj[i,MC]
#       #Bt+1=Bt+Bt*PBt-Ct
#       B.di= max(c(.001,(B.di+B.di*PB.ratio-B.di*Fstrat))) #density independent
#       if(PB.ratio<0) B.dd= max(c(.001,(B.dd+B.dd*PB.ratio-B.dd*Fstrat)))
#       if(PB.ratio>=0) B.dd= max(c(.001,(B.dd+B.dd*PB.ratio*(1-(B.dd/K)^theta)-B.dd*Fstrat)))
#       proj.di[i,MC]= B.di
#       proj.dd[i,MC]= B.dd
#     }
#   }
#   proj.out= list(proj.di=proj.di,proj.dd=proj.dd)
#   proj.out
# }

# Now get the final Biomass under a sequence of F values for each
#   climate scenario (the 2000 MC runs)
# RF added Bstart.mult and theta args, which were missing and caused errors
# Fout isn't a very good name for this output, which is actually final Biomass scenarios
Fout= Fseq.f(PB,Bstart.mult=Bstart.mult,PBproj=PBproj,Fseq=fs,time.frame=time.frame, N=N, K=K, theta=1)
# View(Fout$f) # the sequence of constant F for the projections
# View(Fout$f.di) # the final biomass (density independent) under each F
# View(Fout$f.dd) # the final biomass (density dependent) under each F

# Now get the probability of being at or above the reference B value
# at the end of the time frame specified.
PofF= PofF.f(PB,Fout,ref.pt=ref.pt)
View(PofF)
# @param PB the PB model output
# @param Fprob the output of Fseq.f, i.e. the final B value at the end of a specified period
# @param ref.pt the multiplier of the reference period giving the
#   reference point. Only required if using reference years.
# @param ref.pt.fixed the Blim or limit reference point you want to use

ref.pt

# PofF.f=function (PB, Fprob, ref.pt = NULL , ref.pt.fixed = NULL) {
#   if (all(is.null(ref.pt.fixed) , is.null(ref.pt))) stop('At least one Reference Point must be specified.')
#   if (!is.null(ref.pt.fixed)) Bref = ref.pt.fixed else
#     Bref = Bref.f(PB = PB, ref.pt.multiplier = ref.pt) # gets average biomass for ref years * multiplier
#   # Combine the ref biomass (col 1) and all the biomasses under each F (cols 2:2001)
#   obj.prob.di = cbind(rep(Bref, nrow(Fprob$f.di)), Fprob$f.di)
#   obj.prob.dd = cbind(rep(Bref, nrow(Fprob$f.dd)), Fprob$f.dd)
#   P.di = vector(length = nrow(obj.prob.di)) #Make vector for probs
#   P.dd = P.di
#   N = ncol(Fprob$f.di)
#   # For each F scenario (row) get the prob of being above the ref biomass
#   for (i in 1:nrow(obj.prob.di)) {
#     vec.di = obj.prob.di[i, ] #ith row of matrix
#       # (i.e. F scenario i, where the first element is the ref biomass)
#     P.di[i] = 1 - rank(vec.di)[1]/N  # get the rank of the ref biomass and
#       # convert to probability by dividing by N. This gives the proportion
#       # of trials above the ref. Shouldn't it be divided by N+1?
#     vec.dd = obj.prob.dd[i, ]
#     P.dd[i] = 1 - rank(vec.dd)[1]/N
#   }
#   years = (tail(PB$Year, 1) + 1):(tail(PB$Year, 1) + length(P.di)) #what for?
#   P = data.frame(f = Fprob$f, P.di = P.di, P.dd = P.dd)
#   P
# }
#
# Bref.f= function(PB, ref.pt.multiplier){
#   Bref= ref.pt.multiplier*sum(PB$Index.q * PB$refererence.years,na.rm=T)/sum(PB$refererence.years,na.rm=T)
#   Bref
# }

# Now have the probability of being above the reference biomass
# in the final projection year for a set of constant F policies (PofF)
#=============================================================================

# Now we seem to be back at the annual biomasses under Fstrat

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

# Warm
Emean.shift.warm=0.5
Eproj.warm= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift.warm, proj.years=proj.years, N)
PBproj.warm= PB.for.projection.f(PvsE=PvsE,Eproj.warm,add.residuals=add.resids)
Bproj.warm= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj.warm, Fstrat, K=K, theta=1)
# RF added Bstart.mult and theta args
Fout.warm= Fseq.f(PB, Bstart.mult=Bstart.mult, PBproj=PBproj.warm,Fseq=fs,time.frame=time.frame, N=N, K=K, theta=1)
PofF.warm= PofF.f(PB,Fout.warm,ref.pt=ref.pt)
Bproj.summary.warm= Bproj.summary.f(PB,Bproj.warm,PBproj.warm,Eproj.warm)
P.warm= rankprob.f(Bproj.warm,PB,ref.pt)

# Cold
Emean.shift.cold=-0.5
Eproj.cold= Eprojnorm.f(Edist.a=Edist.a, Edist.b=Edist.b, Emean.shift=Emean.shift.cold, proj.years=proj.years, N)
PBproj.cold= PB.for.projection.f(PvsE=PvsE,Eproj.cold,add.residuals=add.resids)
Bproj.cold= projection.f(PB=PB, Bstart.mult=Bstart.mult, PBproj=PBproj.cold, Fstrat, K=K, theta=1)
# RF added Bstart.mult and theta arg
Fout.cold= Fseq.f(PB,Bstart.mult=Bstart.mult,PBproj=PBproj.cold,Fseq=fs,time.frame=time.frame, N=N, K=K, theta=1)
PofF.cold= PofF.f(PB,Fout.cold,ref.pt=ref.pt)
Bproj.summary.cold= Bproj.summary.f(PB,Bproj.cold,PBproj.cold,Eproj.cold)
P.cold= rankprob.f(Bproj.cold,PB,ref.pt)

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

# I DO NOT KNOW WHAT IS GOING ON HERE OR HOW THIS RELATES TO PREVIOUS CODE
# IS THIS JUST BATCHING UP THE PREVIOUS CODE?

# The Emean.shifts and N.CCF arguments are in the params object
# @param mean.shift a vector of shifts in the mean of the gamma distribution of E
# @param N The number of different realisations of the future to create
# @param proj.years The number of years to project into the future
# @param shape The gamma distribution shape parameter
# @param rate The gamma distribution rate parameter
ECCF= Eproj.list.f(Emean.shifts=Emean.shifts, N=N.CCF, proj.years=proj.years, Edist.a=Edist.a,
                   Edist.b=Edist.b)
PBCCF= PBproj.list.f(PvsE=PvsE, Eprojection=ECCF)
CCF.raw= P.R.for.EF.f(E.CCF=ECCF, PB.CCF=PBCCF, Fs=fs, PB=PB, ref.pt=ref.pt, Bstart.mult=Bstart.mult,
                      K=K, theta=theta)





#==========================================================================
#========== EXTRA PLOTS ETC ===============================================
# Plot the predicted E variable against time
plot(PB$Year,PB$E,type="l", xlim=c(PB$Year[1],max(PB$Year)+10),ylim=c(0,1.1*max(PB$E)))
matplot(2020:2029,Eproj, type="l")

EMed <- apply(Eproj,1,median)
E975 <- apply(Eproj,1,quantile,probs=0.975)
E025 <- apply(Eproj,1,quantile,probs=0.025)

EMed.warm <- apply(Eproj.warm,1,median)
E975.warm <- apply(Eproj.warm,1,quantile,probs=0.975)
E025.warm <- apply(Eproj.warm,1,quantile,probs=0.025)

EMed.cold <- apply(Eproj.cold,1,median)
E975.cold <- apply(Eproj.cold,1,quantile,probs=0.975)
E025.cold <- apply(Eproj.cold,1,quantile,probs=0.025)

EMed.var <- apply(Eproj.var,1,median)
E975.var <- apply(Eproj.var,1,quantile,probs=0.975)
E025.var <- apply(Eproj.var,1,quantile,probs=0.025)

E_projyears <- cbind(E025,EMed,E975) %>%
  as.data.frame() %>%
  mutate(Year=max(PB$Year+1):max(PB$Year+10)) %>%
  select(Year,E025,EMed,E975)

E_projyears.warm <- cbind(E025.warm,EMed.warm,E975.warm) %>%
  as.data.frame() %>%
  mutate(Year=max(PB$Year+1):max(PB$Year+10)) %>%
  select(Year,E025.warm,EMed.warm,E975.warm)

E_projyears.cold <- cbind(E025.cold,EMed.cold,E975.cold) %>%
  as.data.frame() %>%
  mutate(Year=max(PB$Year+1):max(PB$Year+10)) %>%
  select(Year,E025.cold,EMed.cold,E975.cold)

E_projyears.var <- cbind(E025.var,EMed.var,E975.var) %>%
  as.data.frame() %>%
  mutate(Year=max(PB$Year+1):max(PB$Year+10)) %>%
  select(Year,E025.var,EMed.var,E975.var)

E_allyears <- PB %>%
  select(Year,E) %>%
  mutate(E025=E,EMed=E,E975=E) %>%
  select(-E) %>%
  rbind(E_projyears) %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=E025,ymax=E975),
              colour="blue", fill="blue",alpha=0.5)+
  theme_bw()+
  ylim(0,4.5)
E_allyears

#Weird looking  plot. Basically temp can be anything in the range.

E_allyears.warm <- PB %>%
  select(Year,E) %>%
  mutate(E025.warm=E,EMed.warm=E,E975.warm=E) %>%
  select(-E) %>%
  rbind(E_projyears.warm) %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=E025.warm,ymax=E975.warm),
              colour="orange", fill="orange",alpha=0.5)+
  theme_bw()+
  ylim(0,5.)
E_allyears.warm

E_allyears.cold <- PB %>%
  select(Year,E) %>%
  mutate(E025.cold=E,EMed.cold=E,E975.cold=E) %>%
  select(-E) %>%
  rbind(E_projyears.cold) %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=E025.cold,ymax=E975.cold),
              colour="green", fill="green",alpha=0.5)+
  theme_bw()+
  ylim(0,4.5)
E_allyears.cold

E_allyears.var <- PB %>%
  select(Year,E) %>%
  mutate(E025.var=E,EMed.var=E,E975.var=E) %>%
  select(-E) %>%
  rbind(E_projyears.var) %>%
  ggplot() +
  geom_ribbon(aes(x=Year, ymin=E025.var,ymax=E975.var),
              colour="purple", fill="purple",alpha=0.5)+
  theme_bw()+
  ylim(0,6)
E_allyears.var

