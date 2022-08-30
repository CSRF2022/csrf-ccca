# Some preliminary explorations of what the functions are actually doing.
# See READMEccca.Rmd for fully worked Greenland halibut example

# Robyn Forrest. August 29, 2022

# Load the library

# library(devtools)
# install_github("duplisea/ccca")
library(ccca)

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
# Pt = It+1 - It + Ct

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




