# ---------------------------------------------------------------------
# Program: MIDDLE-LGC-Analysis-141023.R
#  Author: Steve Boker
#    Date: Sun Jun 15 13:05:57 CEST 2014
#
# This program plots and analyzes results from MIDDLE-LGC-Sim-141023.R
#
#   You must previously have run MIDDLE-LGC-Sim-141023.R
#
# ---------------------------------------------------------------------
# Revision History
#   Steve Boker -- Mon Jun  9 21:57:55 CEST 2014
#      Created MIDDLE-LGC-Analysis-140610.R
#   Steve Boker -- Mon Jun  9 21:57:55 CEST 2014
#      Created MIDDLE-LGC-Analysis-141023.R from MIDDLE-LGC-Analysis-140610.R
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

#totalExperiments <- 1


#maxP <- 40  # maximum number of observations per person
#maxN <- 2000 # maximum number of persons in one experiment
#maxIterations <- maxP

#deltaT <- 1  # amount of elapsed time between recording intervals
#totalInterval <- (maxP-1) * deltaT  # Total amount of elapsed time
#theTimes  <- seq(0, totalInterval, length=maxP)  # the measurement occasions


# ----------------------------------
# The Program.

tData <- data.frame(iteration=rep(4:maxIterations, each=totalExperiments),
                    cputime=c(resultsArray20141023[1:totalExperiments, 4:maxIterations, 26]),
                    iterationsqr=rep(4:maxIterations, each=totalExperiments)^2,
                    iterationcube=rep(4:maxIterations, each=totalExperiments)^3)

lmOut <- lm(cputime ~ iteration + iterationsqr -1, data=tData)
summary(lmOut)

tFile <- paste("Plots-LGC-20141023/CPUtime.pdf", sep="")
pdf(tFile, height=5, width=6)
plot(c(0, 1.5*maxIterations), c(0, max(tData$cputime, na.rm = TRUE)*2),
     xlab="Iteration",
     ylab="Time (seconds)",
     type='n')
lines(tData$iteration, tData$cputime, type='p', lty=1, lwd=2, col="black")
lines(c(1:100),(c(1:100)*lmOut$coef[1])+(c(1:100)^2*lmOut$coef[2]), type='l', lty=1, col="red")
dev.off()


for (tExperiment in 1:totalExperiments) {
    leftLimit <- 0
    rightLimit <- 1.25 * maxIterations
    tX <- c(4:maxIterations)

    tFile <- paste("Plots-LGC-20141023/IterationVsParameters", tExperiment, ".pdf", sep="")
    pdf(tFile, height=5, width=6)
    plot(c(0,rightLimit), c(0, 2.0),
         xlab="Iteration",
         ylab="Parameter Value",
         type='n')
    lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,1],2), type='l', lty=1, lwd=2, col="black")
    text(maxIterations, resultsArray20141023[tExperiment,maxIterations,1], "mean(I)", pos=4)
    lines(tX, resultsArray20141023[tExperiment,tX,6], type='l', lty=1, lwd=1, col="black")
    lines(tX, resultsArray20141023[tExperiment,tX,11], type='l', lty=2, lwd=1, col="black")
    lines(tX, resultsArray20141023[tExperiment,tX,16], type='l', lty=2, lwd=1, col="black")
    lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,2],2), type='l', lty=1, lwd=2, col="blue")
    text(maxIterations, resultsArray20141023[tExperiment,maxIterations,2], "mean(S)", pos=4)
    lines(tX, resultsArray20141023[tExperiment,tX,7], type='l', lty=1, lwd=1, col="blue")
    lines(tX, resultsArray20141023[tExperiment,tX,12], type='l', lty=2, lwd=1, col="blue")
    lines(tX, resultsArray20141023[tExperiment,tX,17], type='l', lty=2, lwd=1, col="blue")
    lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,3],2), type='l', lty=1, lwd=2, col="red")
    text(maxIterations, resultsArray20141023[tExperiment,maxIterations,3], "var(I)", pos=4)
    lines(tX, resultsArray20141023[tExperiment,tX,8], type='l', lty=1, lwd=1, col="red")
    lines(tX, resultsArray20141023[tExperiment,tX,13], type='l', lty=2, lwd=1, col="red")
    lines(tX, resultsArray20141023[tExperiment,tX,18], type='l', lty=2, lwd=1, col="red")
    lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,4],2), type='l', lty=1, lwd=2, col="green")
    text(maxIterations, resultsArray20141023[tExperiment,maxIterations,4], "var(S)", pos=4)
    lines(tX, resultsArray20141023[tExperiment,tX,9], type='l', lty=1, lwd=1, col="green")
    lines(tX, resultsArray20141023[tExperiment,tX,14], type='l', lty=2, lwd=1, col="green")
    lines(tX, resultsArray20141023[tExperiment,tX,19], type='l', lty=2, lwd=1, col="green")
    lines(c(0,maxIterations), rep(resultsArray20141023[tExperiment,maxIterations,5],2), type='l', lty=1, lwd=2, col="darkgreen")
    text(maxIterations, resultsArray20141023[tExperiment,maxIterations,5], "cov(I,S)", pos=4)
    lines(tX, resultsArray20141023[tExperiment,tX,10], type='l', lty=1, lwd=1, col="darkgreen")
    lines(tX, resultsArray20141023[tExperiment,tX,15], type='l', lty=2, lwd=1, col="darkgreen")
    lines(tX, resultsArray20141023[tExperiment,tX,20], type='l', lty=2, lwd=1, col="darkgreen")

    dev.off()
    
    tFile <- paste("Plots-LGC-20141023/Participants", tExperiment, ".pdf", sep="")
    pdf(tFile, height=5, width=6)
    plot(c(0,1.1*maxIterations), c(0,maxN),
         xlab="Iteration",
         ylab="Participants in Likelihood Estimation",
         type='n')
    lines(tX, resultsArray20141023[tExperiment,tX,27], type='l', lty=1, lwd=1, col="black")
    dev.off()

    topLimit <- 6
    bottomLimit <- 2
    tFile <- paste("Plots-LGC-20141023/Minus2LL-", tExperiment, ".pdf", sep="")
    pdf(tFile, height=5, width=6)
    plot(c(0, 1.1*maxIterations), c(bottomLimit,topLimit),
         xlab="Iteration",
         ylab="Minus 2 Log Likelihood / Total Observations",
         type='n')
    scaledM2LL <- resultsArray20141023[tExperiment,tX,24] / resultsArray20141023[tExperiment,tX,28]
    lines(tX, scaledM2LL, type='l', lty=1, lwd=1, col="black")
    codeRed <- resultsArray20141023[tExperiment,tX,25] == 6
    lines(tX[codeRed], scaledM2LL[codeRed], type='p', pch=8, col="red")
    codeGreen <- resultsArray20141023[tExperiment,tX,25] == 1
    lines(tX[codeGreen], scaledM2LL[codeGreen], type='p', pch=16, col="green")
    lines(5, bottomLimit + (topLimit-bottomLimit) * .9, type='p', pch=16, col="green")
    text(5, bottomLimit + (topLimit-bottomLimit) * .9, "Status Code Green", pos=4)
    lines(maxIterations/2+5, bottomLimit + (topLimit-bottomLimit) * .9, type='p', pch=16, col="red")
    text(maxIterations/2+5, bottomLimit + (topLimit-bottomLimit) * .9, "Status Code Red", pos=4)
    dev.off()
}
# ----------------------------------
# .

