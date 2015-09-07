# ---------------------------------------------------------------------
# Program: MIDDLE-AR1-Sim-140609.R
#  Author: Steve Boker
#    Date: Mon Jun  9 14:42:06 CEST 2014
#
# Simulates a population of latent growth curves and draws from them
#   according to MIDDLE methods.
#
# Tests accuracy of point estimates and CIs relative to full simulated population
#
# Credit goes to Timo von Oertzen and Mike Hunter for previous simulations
#   of the MIDDLE-style sampling and estimation paradigm.
#
# ---------------------------------------------------------------------
# Revision History
#   Steve Boker -- Mon Jun  9 14:42:06 CEST 2014
#      Created MIDDLE-LGA-Sim-140609.R
#
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Variables 
# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

date()


# ----------------------------------
# Read libraries and set options.

options(width=110)

library(psych)
library(OpenMx)
library(MASS)

# ---------------------------------------------
# Parameters for simulation.

mxOption(NULL, "Default optimizer", "NPSOL")
mxOption(NULL, "Calculate Hessian", "No")
mxOption(NULL, "Standard Errors"  , "No")
mxOption(NULL, "CI Max Iterations", "1")

#mxOption(NULL, "UsePPML", "Yes")


meanAR <- 0.5  # target AR parameter mean
varAR <- 0.01  # target AR parameter variance
meanEq <- 2    # target intercept mean
varEq <- 1.5   # target intercept variance
covAREq <- 0   # target covariance between slop and intercept
sigmaTarget <- matrix(c(varEq, covAREq, covAREq, varAR), 2, 2) # target latent covariance matrix
muTarget <- c(meanEq, meanAR) # target latent means.

varShock <- 1  # variance of the time-independent error
sdShock <- sqrt(varShock)


totalExperiments<-50  # total number of experiment replications to simulate

maxIterations<-150 # maximum number of iterations in an experiment
DLEPerIter<-1          # maximum number of Distributed Likelihood Estimations per iteration
ObsPerIter <- 1      # maximum number of observations per iteration (can be a fraction e.g., 1 obs per 10 iters)
minEstObs <- 5       # minimum number of opt-in observations before estimation is allowed.
maxSimP <- 200       # maximum number of simulated observations (from which the individual timeseries are drawn)
maxP<-20           # maximum number of observations per person used in estimation
maxN <- 1000         # maximum number of persons in one experiment


deltaT <- 1  # amount of elapsed time between recording intervals
totalInterval <- (maxSimP-1) * deltaT  # Total amount of elapsed time
theTimes  <- seq(0, totalInterval, length=maxSimP)  # the measurement occasions

fixedLoadings <- rbind(rep(1,maxSimP), theTimes)

pDeviceOn<-.3   # probability that any one person's device is available.
pOptIn <- .02    # probability that any one person opts into the experiment at each iteration.
pOptOut <- .01  # probability that any one person opts out of the experiment at each iteration.
pObserved<-.5  # probability that an observation is recorded at each iteration.
recordOnOptIn <- TRUE  # only start recording after first opt-in.

# ----------------------------------
# Create the array to store the simulated results

tcolnames <- c("trueMeanEq", "trueMeanAR", "trueVarEq", "trueVarAR", "trueVarShock",
               "estMeanEq", "estMeanAR", "estVarEq", "estVarAR", "estCovEqAR",
               "lowerCIMeanEq", "lowerCIMeanAR", "lowerCIVarEq", "lowerCIVarAR", "lowerCICovAREq",
               "upperCIMeanEq", "upperCIMeanAR", "upperCIVarEq", "upperCIVarAR", "upperCICovAREq",
               "estVarShock", "lowerVarShock", "upperVarShock",
               "minus2LL", "status", "cpuTime", "N", "totalObs")

resultsArray20140615 <- array(NA, dim=c(totalExperiments, maxIterations,28), dimnames=list(NULL,NULL,tcolnames))

# ----------------------------------
# Loop and fill the results array


# ----------------------------------
# This version assumes that 
#    1. there are no observations prior to first opt-in.
#    2. Recording continues after opt-out.
#    3. Only observations available are those recorded during an opt-in interval.


for (tExperiment in 1:totalExperiments) {

    # ----------------------------------
    # Simulate population of AR parameters

    tLatentMatrix <- mvrnorm(n=maxN, mu=muTarget, Sigma=sigmaTarget, tol=1e-6, empirical=TRUE, EISPACK=FALSE)
    tLatentMatrix[(tLatentMatrix[,2] <= 0.01), 2] <- 0.01
    tLatentMatrix[(tLatentMatrix[,2] >= 0.99), 2] <- 0.99
    
    # ----------------------------------
    # Simulate population of scores based on AR parameters
    
    tManifestMatrix <- matrix(NA, nrow=maxN, ncol=maxSimP)
    tManifestMatrix[,1] <- tLatentMatrix[,1] + rnorm(maxN, mean=0, sd=sdShock)
    for (i in 2:maxSimP) {
        tManifestMatrix[,i] <- tLatentMatrix[,1] + (tLatentMatrix[,2]*(tManifestMatrix[,i-1]-tLatentMatrix[,1])) + rnorm(maxN, mean=0, sd=sdShock)
    }
    tFile <- paste("Plots-AR-20140615/Trajectories-Exp", tExperiment, ".pdf", sep="")
    pdf(tFile, height=5, width=6)
    plot(c(0,maxSimP), c(-5,10),
         xlab="Observation",
         ylab="Score",
         type='n')
    for (tSubj in 1:20) {
        lines(1:maxSimP, tManifestMatrix[tSubj,], type='l', lty=1, lwd=1, col=topo.colors(20)[tSubj])
    }
    dev.off()
#}
#describe(tManifestMatrix)
#describe(t(tManifestMatrix)[,1:10])
#tLatentMatrix[1:10,]
#describe(tLatentMatrix)

#q()
#{
    optInVector <- rep(FALSE, maxN)  # at the current iteration is the participant opted-in
    optInMatrix <- matrix(FALSE, nrow=maxN, ncol=maxSimP)  # for each possible observation, is the pp opted-in
    firstOptInIndex <- rep(NA, maxN)  # what is the iteration of the first opt-in.
    useOptOutData <- rep(FALSE, maxN)  # does the pp allow the use of data from periods when they were opted-out
    
    recordedVector <- rep(FALSE, maxN)  # at the current iteration does the participant have recorded data
    recordedMatrix <- matrix(FALSE, nrow=maxN, ncol=maxSimP)  # for each possible observation, was it recorded.
    
    # ---------------------------
    # Starting values for model
    
    oldAR <- .1 + rnorm(1, mean=0, sd=.001)
    oldvShock <- 2 + rnorm(1, mean=0, sd=.5)
    oldvEq <- 2  + rnorm(1, mean=0, sd=.1)
    oldmEq <- 3 + rnorm(1, mean=0, sd=1)
        
    for (tIteration in 1:maxIterations) {
        cat("\n\n-------------------------------\n","Experiment=",tExperiment,"  Iteration=", tIteration,"\n")

        # ----------------------------------
        # Calculate opt-in selectors
        
        optInVector <- optInVector | (runif(maxN, 0, 1) < pOptIn)  # does the person opt-in at current iteration?
        optInVector[runif(maxN, 0, 1) < pOptOut] <- FALSE          # does the person opt-out at current iteration
        optInMatrix[optInVector, tIteration] <- TRUE               # select observations that can be used.
        optInMatrix[optInVector & useOptOutData, 1:tIteration] <- TRUE  # use previously recorded data if pp allows
        firstOptInIndex[is.na(firstOptInIndex) & optInVector & useOptOutData] <- 1
        firstOptInIndex[is.na(firstOptInIndex) & optInVector] <- tIteration

        # ----------------------------------
        # Calculate observation recorded selector
        
        recordedVector[optInVector] <- TRUE  # once recording starts, recording is possible even if participant opts-out
        if (recordOnOptIn) {
            recordedMatrix[recordedVector & (runif(maxN, 0, 1) < pObserved), tIteration] <- TRUE # an observation is recorded at this iteration.
        }
        else {
            recordedMatrix[(runif(maxN, 0, 1) < pObserved), tIteration] <- TRUE # an observation is recorded at this iteration.
        }
        
        # ----------------------------------
        # Do not estimate a model unless there are at least minEstObs possible occasions of measurement

        if (tIteration < minEstObs) next
        
        # ----------------------------------
        # Select available data
        
        deviceAvailable <- runif(maxN, 0, 1) < pDeviceOn  # TRUE if device available this iteration
        
        tMin <- tIteration - (maxP - 1)
        if (tMin < 1) tMin <- 1

        tData <- tManifestMatrix
        tData[!optInMatrix | !recordedMatrix] <- NA  # data are missing if not recorded or not opted in.
        availableData <- tData[deviceAvailable & optInVector, tMin:tIteration]  # select currently available data rows and columns.
        
        # ----------------------------------
        # Do not estimate a model unless there are at least minEstObs available occasions of measurement
        #   in minEstObs number of rows
        
        t1 <- apply(!is.na(availableData), 1, sum)
        if (sum(t1 > minEstObs) < minEstObs) next
        
        # ----------------------------------
        # Create OpenMx Model
        
        tDataCols <- dim(availableData)[2]
        theManifests <- paste("x", 1:tDataCols, sep="_")
        upStream <- paste("x", 1:(tDataCols-1), sep="_")
        downStream <- paste("x", 2:tDataCols, sep="_")
        downLatents <- paste("d", 2:tDataCols, sep="_")
        dimnames(availableData) <- list(NULL, theManifests)
        theLatents <- c("Eq", downLatents)
        
        cat("matrix dimensions", dim(availableData), "  obs=", sum(!is.na(availableData)), "\n")
        
        ARmodel <- mxModel(model="ARmodel", type="RAM", manifestVars=theManifests, latentVars=theLatents,
            mxPath(from=upStream, to=downLatents, connect='single', arrows=1, free=FALSE, values=-1),
            mxPath(from=downLatents, to=downStream, connect='single', arrows=1, free=TRUE, values=oldAR, labels="mAR"),
            mxPath(from=theManifests, to=theManifests, connect='single', arrows=2, free=TRUE, values=oldvShock, labels=c("vX",rep("vShock",tDataCols-1))),
            mxPath(from="Eq", to="Eq", connect='single', arrows=2, free=TRUE, values=oldvEq, labels="vEq"),
            mxPath(from="Eq", to=c(theManifests,downLatents), arrows=1, free=FALSE, values=1),
            mxPath(from="one", to="Eq", arrows=1, free=TRUE, values=oldmEq, labels=c("mEq")),
            #mxCI(c("mEq", "mAR", "vShock", "vEq")),
            mxData(availableData, type="raw")
            )
        mxOption(NULL, "Major iterations", DLEPerIter)
        # ARmodelFit <- mxRun(ARmodel, intervals=TRUE)
        ARmodelFit <- mxRun(ARmodel)
        # print(summary(ARmodelFit))

        # ---------------------------
        # Update starting values for model
    
        oldAR <- mxEval(mAR, ARmodelFit)
        oldvShock <- mxEval(vShock, ARmodelFit)
        oldvEq <- mxEval(vEq, ARmodelFit)
        oldmEq <- mxEval(mEq, ARmodelFit)

       resultsArray20140615[tExperiment, tIteration, 1] <- mean(tLatentMatrix[,1])
       resultsArray20140615[tExperiment, tIteration, 2] <- mean(tLatentMatrix[,2])
       resultsArray20140615[tExperiment, tIteration, 3] <- var(tLatentMatrix[,1])
       resultsArray20140615[tExperiment, tIteration, 4] <- var(tLatentMatrix[,2])
       resultsArray20140615[tExperiment, tIteration, 5] <- varShock
       resultsArray20140615[tExperiment, tIteration, 6] <- mxEval(mEq, ARmodelFit)
       resultsArray20140615[tExperiment, tIteration, 7] <- - mxEval(mAR, ARmodelFit)
       resultsArray20140615[tExperiment, tIteration, 8] <- mxEval(vEq, ARmodelFit)
       resultsArray20140615[tExperiment, tIteration, 9] <- NA # mxEval(vAR, ARmodelFit)
       resultsArray20140615[tExperiment, tIteration, 10] <- NA # mxEval(covEqAR, ARmodelFit)
       resultsArray20140615[tExperiment, tIteration, 11] <- NA # ARmodelFit$output$confidenceIntervals[1,1]
       resultsArray20140615[tExperiment, tIteration, 12] <- NA # -ARmodelFit$output$confidenceIntervals[2,1]
       resultsArray20140615[tExperiment, tIteration, 13] <- NA # ARmodelFit$output$confidenceIntervals[4,1]
       resultsArray20140615[tExperiment, tIteration, 14] <- NA # ARmodelFit$output$confidenceIntervals[5,1]
       resultsArray20140615[tExperiment, tIteration, 16] <- NA # ARmodelFit$output$confidenceIntervals[1,2]
       resultsArray20140615[tExperiment, tIteration, 17] <- NA # -ARmodelFit$output$confidenceIntervals[2,2]
       resultsArray20140615[tExperiment, tIteration, 18] <- NA # ARmodelFit$output$confidenceIntervals[4,2]
       resultsArray20140615[tExperiment, tIteration, 19] <- NA # ARmodelFit$output$confidenceIntervals[5,2]
       resultsArray20140615[tExperiment, tIteration, 21] <- mxEval(vShock, ARmodelFit)
       resultsArray20140615[tExperiment, tIteration, 22:23] <- NA # ARmodelFit$output$confidenceIntervals[3,1:2]
       resultsArray20140615[tExperiment, tIteration, 24] <- ARmodelFit$output$Minus2LogLikelihood
       resultsArray20140615[tExperiment, tIteration, 25] <- ARmodelFit$output$status$code
       resultsArray20140615[tExperiment, tIteration, 26] <- as.numeric(ARmodelFit$output$cpuTime)
       resultsArray20140615[tExperiment, tIteration, 27] <- dim(availableData)[1]
       resultsArray20140615[tExperiment, tIteration, 28] <- sum(!is.na(availableData))
    }
}

save(resultsArray20140615, file="resultsArray20140615.RData")

for (i in 1:totalExperiments) {
    print(describe(as.matrix(resultsArray20140615[i,,])))
}

date()



