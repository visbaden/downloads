# ---------------------------------------------------------------------
# Program: MIDDLE-LGC-Analysis2-141023.R
#  Author: Steve Boker
#    Date: Fri Oct 24 08:10:52 EDT 2014
#
# This program plots and analyzes summaries of results from MIDDLE-LGC-Sim-141023.R
#
#   You must previously have run MIDDLE-LGC-Sim-141023.R and MIDDLE-LGC-Analysis-141023.R
#
# ---------------------------------------------------------------------
# Revision History
#   Steve Boker -- Mon Jun  9 21:57:55 CEST 2014
#      Created MIDDLE-LGC-Analysis-140610.R
#   Steve Boker -- Mon Jun  9 21:57:55 CEST 2014
#      Created MIDDLE-LGC-Analysis-141023.R from MIDDLE-LGC-Analysis-140610.R
#   Steve Boker -- Fri Oct 24 08:10:46 EDT 2014
#      Created MIDDLE-LGC-Analysis2-141023.R from MIDDLE-LGC-Analysis-141023.R
#      
#
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Variables 
# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

# ----------------------------------
# Read libraries and set options.

options(width=110)

library(OpenMx)
library(psych)
library(lattice)

# ----------------------------------
# Set constants.

#totalExperiments <- 100


#maxP <- 40  # maximum number of observations per person
#maxN <- 2000 # maximum number of persons in one experiment
#maxIterations <- maxP

#deltaT <- 1  # amount of elapsed time between recording intervals
#totalInterval <- (maxP-1) * deltaT  # Total amount of elapsed time
#theTimes  <- seq(0, totalInterval, length=maxP)  # the measurement occasions


# ----------------------------------
# Aggregate the results of the experiments.

leftLimit <- 0
rightLimit <- 1.25 * maxIterations
tX <- c(4:maxIterations)

meanMeanI <- apply(resultsArray20141023[,tX,6], 2, mean, na.rm=TRUE)
sdMeanI <- apply(resultsArray20141023[,tX,6], 2, sd, na.rm=TRUE)

meanMeanS <- apply(resultsArray20141023[,tX,7], 2, mean, na.rm=TRUE)
sdMeanS <- apply(resultsArray20141023[,tX,7], 2, sd, na.rm=TRUE)

meanVarI <- apply(resultsArray20141023[,tX,8], 2, mean, na.rm=TRUE)
sdVarI <- apply(resultsArray20141023[,tX,8], 2, sd, na.rm=TRUE)

meanVarS <- apply(resultsArray20141023[,tX,9], 2, mean, na.rm=TRUE)
sdVarS <- apply(resultsArray20141023[,tX,9], 2, sd, na.rm=TRUE)

meanCovIS <- apply(resultsArray20141023[,tX,10], 2, mean, na.rm=TRUE)
sdCovIS <- apply(resultsArray20141023[,tX,10], 2, sd, na.rm=TRUE)

meanN <- apply(resultsArray20141023[,tX,27], 2, mean, na.rm=TRUE)
sdN <- apply(resultsArray20141023[,tX,27], 2, sd, na.rm=TRUE)

scaledM2LL <- resultsArray20141023[,tX,24] / resultsArray20141023[,tX,28]

meanScaledM2LL <- apply(scaledM2LL, 2, mean, na.rm=TRUE)
sdScaledM2LL <- apply(scaledM2LL, 2, sd, na.rm=TRUE)


tFile <- paste("Plots-LGC-20141023/MeanIterationVsParameters.pdf", sep="")
pdf(tFile, height=5, width=6)
plot(c(0,rightLimit), c(0, 2.0),
     xlab="Iteration",
     ylab="Parameter Value",
     type='n')
lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,1],2), type='l', lty=1, lwd=2, col="black")
text(maxIterations, resultsArray20141023[tExperiment,maxIterations,1], "mean(I)", pos=4)
lines(tX, meanMeanI, type='l', lty=1, lwd=1, col="black")
lines(tX, meanMeanI+sdMeanI, type='l', lty=2, lwd=1, col="black")
lines(tX, meanMeanI-sdMeanI, type='l', lty=2, lwd=1, col="black")
lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,2],2), type='l', lty=1, lwd=2, col="blue")
text(maxIterations, resultsArray20141023[tExperiment,maxIterations,2], "mean(S)", pos=4)
lines(tX, meanMeanS, type='l', lty=1, lwd=1, col="blue")
lines(tX, meanMeanS+sdMeanS, type='l', lty=2, lwd=1, col="blue")
lines(tX, meanMeanS-sdMeanS, type='l', lty=2, lwd=1, col="blue")
lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,3],2), type='l', lty=1, lwd=2, col="red")
text(maxIterations, resultsArray20141023[tExperiment,maxIterations,3], "var(I)", pos=4)
lines(tX, meanVarI, type='l', lty=1, lwd=1, col="red")
lines(tX, meanVarI+sdVarI, type='l', lty=2, lwd=1, col="red")
lines(tX, meanVarI-sdVarI, type='l', lty=2, lwd=1, col="red")
lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,4],2), type='l', lty=1, lwd=2, col="green")
text(maxIterations, resultsArray20141023[tExperiment,maxIterations,4], "var(S)", pos=4)
lines(tX, meanVarS, type='l', lty=1, lwd=1, col="green")
lines(tX, meanVarS+sdVarS, type='l', lty=2, lwd=1, col="green")
lines(tX, meanVarS-sdVarS, type='l', lty=2, lwd=1, col="green")
lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,5],2), type='l', lty=1, lwd=2, col="darkgreen")
text(maxIterations, resultsArray20141023[tExperiment,maxIterations,5], "cov(I,S)", pos=4)
lines(tX, meanCovIS, type='l', lty=1, lwd=1, col="darkgreen")
lines(tX, meanCovIS+sdCovIS, type='l', lty=2, lwd=1, col="darkgreen")
lines(tX, meanCovIS-sdCovIS, type='l', lty=2, lwd=1, col="darkgreen")

dev.off()

tFile <- paste("Plots-LGC-20141023/MeanParticipants.pdf", sep="")
pdf(tFile, height=5, width=6)
plot(c(0,1.1*maxIterations), c(0,maxN),
     xlab="Iteration",
     ylab="Participants in Likelihood Estimation",
     type='n')
lines(tX, meanN, type='l', lty=1, lwd=1, col="black")
lines(tX, meanN+sdN, type='l', lty=2, lwd=1, col="black")
lines(tX, meanN-sdN, type='l', lty=2, lwd=1, col="black")
dev.off()

topLimit <- 6
bottomLimit <- 2
tFile <- paste("Plots-LGC-20141023/MeanMinus2LL.pdf", sep="")
pdf(tFile, height=5, width=6)
plot(c(0, 1.1*maxIterations), c(bottomLimit,topLimit),
     xlab="Iteration",
     ylab="Minus 2 Log Likelihood / Total Observations",
     type='n')
lines(tX, meanScaledM2LL, type='l', lty=1, lwd=1, col="black")
lines(tX, meanScaledM2LL+sdScaledM2LL, type='l', lty=2, lwd=1, col="black")
lines(tX, meanScaledM2LL-sdScaledM2LL, type='l', lty=2, lwd=1, col="black")
dev.off()

# ----------------------------------
# .

