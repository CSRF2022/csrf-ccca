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
View(turbot)
View(ncod)
View(ShrimpGSL)

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

# Determine the annual P/B ratio of the population creating a
# dataframe with the index bumped by q

PB= PB.f(turbot, ref.years=ref.years, q=q)



