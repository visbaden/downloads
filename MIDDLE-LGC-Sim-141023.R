# ---------------------------------------------------------------------
# Program: MIDDLE-LGC-Sim-141023.R
#  Author: Steve Boker
#    Date: Thu Oct 23 10:54:42 EDT 2014
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
#      Created MIDDLE-LGC-Sim-140609.R
#   Steve Boker -- Thu Oct 23 10:54:50 EDT 2014
#      Created MIDDLE-LGC-Sim-141023.R from MIDDLE-LGC-Sim-140609.R
#      Changed simulation parameters so that there are wider bounds on participant Ns
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


meanSlope <- 0.6  # target slope mean
varSlope <- 0.9  # target slope variance
meanInt <- 1.5  # target intercept mean
varInt <- 1.2  # target intercept variance
covSlopeInt <- .3  # target covariance between slop and intercept
sigmaTarget <- matrix(c(varInt, covSlopeInt, covSlopeInt, varSlope), 2, 2) # target latent covariance matrix
muTarget <- c(meanInt, meanSlope) # target latent means.

varResid <- .5  # variance of the time-independent error


totalExperiments<-100  # total number of experiment replications to simulate

maxIterations<-150 # maximum number of iterations in an experiment
DLEPerIter<-150          # maximum number of Distributed Likelihood Estimations per iteration
ObsPerIter <- 1      # maximum number of observations per iteration (can be a fraction e.g., 1 obs per 10 iters)
minEstObs <- 5       # minimum number of opt-in observations before estimation is allowed.
maxSimP <- 200       # maximum number of simulated observations (from which the individual timeseries are drawn)
maxP<-40           # maximum number of observations per person used in estimation
maxN <- 2000         # maximum number of persons in one experiment


deltaT <- 1  # amount of elapsed time between recording intervals
totalInterval <- (maxSimP-1) * deltaT  # Total amount of elapsed time
theTimes  <- seq(0, totalInterval, length=maxSimP)  # the measurement occasions

fixedLoadings <- rbind(rep(1,maxSimP), theTimes)

pDeviceOn<-.3   # probability that any one person's device is available.
pOptIn <- .03    # probability that any one person opts into the experiment at each iteration.
pOptOut <- .005  # probability that any one person opts out of the experiment at each iteration.
pObserved<-.5  # probability that an observation is recorded at each iteration.
recordOnOptIn <- TRUE  # only start recording after first opt-in.

# ----------------------------------
# Create the array to store the simulated results

tcolnames <- c("trueMeanInt", "trueMeanSlope", "trueVarInt", "trueVarSlope", "trueCovSlopeInt",
               "estMeanInt", "estMeanSlope", "estVarInt", "estVarSlope", "estCovSlopeInt",
               "lowerCIMeanInt", "lowerCIMeanSlope", "lowerCIVarInt", "lowerCIVarSlope", "lowerCICovSlopeInt",
               "upperCIMeanInt", "upperCIMeanSlope", "upperCIVarInt", "upperCIVarSlope", "upperCICovSlopeInt",
               "estVarResid", "lowerVarResid", "upperVarResid",
               "minus2LL", "status", "cpuTime", "N", "totalObs")

resultsArray20141023 <- array(NA, dim=c(totalExperiments, maxIterations,28), dimnames=list(NULL,NULL,tcolnames))


# ----------------------------------
# Loop and fill the results array


# ----------------------------------
# This version assumes that 
#    1. there are no observations prior to first opt-in.
#    2. Recording continues after opt-out.
#    3. Only observations available are those recorded during an opt-in interval.


for (tExperiment in 1:totalExperiments) {

    # ----------------------------------
    # Simulate population of LGC parameters

    tLatentMatrix <- mvrnorm(n=maxN, mu=muTarget, Sigma=sigmaTarget, tol=1e-6, empirical=TRUE, EISPACK=FALSE)
    tLatentMatrix[(tLatentMatrix[,2] <= 0.01), 2] <- 0.01
    tLatentMatrix[(tLatentMatrix[,2] >= 0.99), 2] <- 0.99
    
    # ----------------------------------
    # Simulate latent variable score matrix conforming to the target means and covariances

    tLatentMatrix <- mvrnorm(n=maxN, mu=muTarget, Sigma=sigmaTarget, tol=1e-6, empirical=TRUE, EISPACK=FALSE)

    # ----------------------------------
    # Simulate manifest variable score matrix latent scores
    
    tManifestMatrix <- (tLatentMatrix %*% fixedLoadings) + rnorm(maxN*maxP, mean=0, sd=sqrt(varResid))


    optInVector <- rep(FALSE, maxN)  # at the current iteration is the participant opted-in
    optInMatrix <- matrix(FALSE, nrow=maxN, ncol=maxSimP)  # for each possible observation, is the pp opted-in
    firstOptInIndex <- rep(NA, maxN)  # what is the iteration of the first opt-in.
    useOptOutData <- rep(FALSE, maxN)  # does the pp allow the use of data from periods when they were opted-out
    
    recordedVector <- rep(FALSE, maxN)  # at the current iteration does the participant have recorded data
    recordedMatrix <- matrix(FALSE, nrow=maxN, ncol=maxSimP)  # for each possible observation, was it recorded.
    
    # ---------------------------
    # Starting values for model
    
    oldmInt <- 1 + rnorm(1, mean=0, sd=.5)
    oldmSlope <- 1 + rnorm(1, mean=0, sd=.5)
    oldvInt <- .8  + rnorm(1, mean=0, sd=.1)
    oldvSlope <- .8 + rnorm(1, mean=0, sd=.1)
    oldcovIntSlope <- .2 + rnorm(1, mean=0, sd=.01)
    oldvU <- .1 + rnorm(1, mean=0, sd=.001)
        
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
        
        availableData <- availableData[t1 > 2,]
        
        # ----------------------------------
        # Create OpenMx Model
        
        tDataCols <- dim(availableData)[2]

        theManifests <- paste("x", 1:tDataCols, sep="_")
        dimnames(availableData) <- list(NULL, theManifests)
        theLatents <- c("Int", "Slope")
        tmpFixedLoadings <- fixedLoadings[, tMin:tIteration]
        
        cat("matrix dimensions", dim(availableData), "  obs=", sum(!is.na(availableData)), "\n")
        
        LGCmodel <- mxModel(model="LGCmodel", type="RAM", manifestVars=theManifests, latentVars=theLatents,
            mxPath(from="Int", to=theManifests, connect='single', arrows=1, free=FALSE, values=tmpFixedLoadings[1,]),
            mxPath(from="Slope", to=theManifests, connect='single', arrows=1, free=FALSE, values=tmpFixedLoadings[2,]),
            mxPath(from=theManifests, to=theManifests, connect='single', arrows=2, free=TRUE, values=oldvU, labels="vU"),
            mxPath(from=theLatents, to=theLatents, connect='single', arrows=2, free=TRUE, values=c(oldvInt,oldvSlope), labels=c("vInt", "vSlope")),
            mxPath(from="Int", to="Slope", connect='single', arrows=2, free=TRUE, values=oldcovIntSlope, labels=c("covIntSlope")),
            mxPath(from="one", to=theLatents, arrows=1, free=TRUE, values=c(oldmInt,oldmSlope), labels=c("mInt", "mSlope")),
            #mxCI(c("mInt", "mSlope", "vInt", "vSlope", "covIntSlope", "vU")),
            mxData(availableData, type="raw")
            )
        mxOption(NULL, "Major iterations", DLEPerIter)
        #LGCmodelFit <- mxRun(LGCmodel, intervals=TRUE)
        LGCmodelFit <- mxRun(LGCmodel, unsafe=TRUE)
        print(summary(LGCmodelFit))


        # ---------------------------
        # Update starting values for model
    

        resultsArray20141023[tExperiment, tIteration, 1] <- mean(tLatentMatrix[,1])
        resultsArray20141023[tExperiment, tIteration, 2] <- mean(tLatentMatrix[,2])
        resultsArray20141023[tExperiment, tIteration, 3] <- var(tLatentMatrix[,1])
        resultsArray20141023[tExperiment, tIteration, 4] <- var(tLatentMatrix[,2])
        resultsArray20141023[tExperiment, tIteration, 5] <- var(tLatentMatrix)[1,2]
        resultsArray20141023[tExperiment, tIteration, 6] <- mxEval(mInt, LGCmodelFit)
        resultsArray20141023[tExperiment, tIteration, 7] <-  mxEval(mSlope, LGCmodelFit)
        resultsArray20141023[tExperiment, tIteration, 8] <-  mxEval(vInt, LGCmodelFit)
        resultsArray20141023[tExperiment, tIteration, 9] <-  mxEval(vSlope, LGCmodelFit)
        resultsArray20141023[tExperiment, tIteration, 10] <-  mxEval(covIntSlope, LGCmodelFit)
        resultsArray20141023[tExperiment, tIteration, 11:15] <- NA # LGCmodelFit$output$confidenceIntervals[1:5,1]
        resultsArray20141023[tExperiment, tIteration, 16:20] <- NA # LGCmodelFit$output$confidenceIntervals[1:5,2]
        resultsArray20141023[tExperiment, tIteration, 21] <- mxEval(vU, LGCmodelFit)
        resultsArray20141023[tExperiment, tIteration, 22:23] <- NA # LGCmodelFit$output$confidenceIntervals[6,1:2]
        if (LGCmodelFit$output$status$code >= 0) {
            resultsArray20141023[tExperiment, tIteration, 24] <- LGCmodelFit$output$Minus2LogLikelihood
        }
        resultsArray20141023[tExperiment, tIteration, 25] <- LGCmodelFit$output$status$code
        resultsArray20141023[tExperiment, tIteration, 26] <- as.numeric(LGCmodelFit$output$cpuTime)
        resultsArray20141023[tExperiment, tIteration, 27] <- dim(availableData)[1]
        resultsArray20141023[tExperiment, tIteration, 28] <- sum(!is.na(availableData))

        if (LGCmodelFit$output$status$code >= 0) {
            oldmInt <-  mxEval(mInt, LGCmodelFit)
            oldmSlope <- mxEval(mSlope, LGCmodelFit)
            oldvInt <- mxEval(vInt, LGCmodelFit)
            oldvSlope <- mxEval(vSlope, LGCmodelFit)
            oldcovIntSlope <- mxEval(covIntSlope, LGCmodelFit)
            oldvU <- mxEval(vU, LGCmodelFit)
        }
    }
}

save(resultsArray20141023, file="resultsArray20141023.RData")

for (i in 1:totalExperiments) {
    print(describe(as.matrix(resultsArray20141023[i,,])))
}

date()



