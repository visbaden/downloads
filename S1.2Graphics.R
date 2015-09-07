


#******************************************************
#
#  				Session 1.2
#				Multivariate Graphics and
#				Principal Components
#
#******************************************************



#**************************************************
#
#  		Scatter Plot Matrices
#
#**************************************************
setwd("C:\\Users\\J\\Desktop\\Fall 15\\APMA 6430_STAT\\R")

# Scatter plots - Time series of extreme accidents

plot(2001:2014, tapply(xdmg$ACCDMG, as.factor(xdmg$YEAR), sum), type = "l", ylab = "Damage ($)", xlab = "Year", main = "Total Damage per Year")


# Source SPM_Panel.R

source("C:\\Users\\J\\Desktop\\Fall 15\\APMA 6430_STAT\\R\\Code\\SPM_Panel.R")


# without panel functions

pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = xdmg)

# with panel function

uva.pairs(xdmg[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]) 


# Print as png to avoid problems in the document

png("C:\\Users\\J\\Desktop\\Fall 15\\APMA 6430_STAT\\R\\metrics.png")
uva.pairs(xdmg[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")]) 
dev.off()

#uva.pairs(xdmg[,c("ACCDMG", "CASUALTY", "TRNSPD", "TONS","CARSUM", "TIMEHR")])
#dev.off()


# Look at possible predictors

# Create total number of cars

xdmg$CARSUM <- apply(xdmg[,c(52:57)], 1, sum)

names(xdmg[,c(52:57)])

# Create CASUALTY 

xdmg$CASUALTY <- xdmg$TOTKLD + xdmg$TOTINJ

totacts$CASUALTY <- totacts$TOTKLD + totacts$TOTINJ

uva.pairs(xdmg[,c("ACCDMG", "CASUALTY", "TRNSPD", "TONS","CARSUM", "TIMEHR")])
dev.off()

# Look at SPM for preditors and ACCDMG and 

uva.pairs(xdmg[,c("ACCDMG", "CASUALTY", "TRNSPD", "TONS","CARSUM", "TIMEHR" )])


#***********************************************************
#
#  	Principal Components with the Correlation Matrix	
#
#***********************************************************


# Principal Components with the Correlation Matrix for extreme data 2 (metrics)

xdmg.pca <- princomp(xdmg[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)



# Variance

summary(xdmg.pca)


# View data in the first 2 PC

biplot(xdmg.pca)
xdmg[4321, c(21,60,61)]

#totacts[4321, 123:137]

# Again use png to save it.

png("metrics.png")
uva.pairs(xdmg[ ,c("TRKDMG","EQPDMG","ACCDMG","TOTINJ","TOTKLD")]) 
dev.off()
# Remove outliers in component 2

xdmg.pca <- princomp(xdmg[-c(4231, 30016, 18969),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)

# View the first 2 PC without ouliers

biplot(xdmg.pca)

# Variance plot

screeplot(xdmg.pca, main = "Variance for PC of Metrics")



# Loadings

barplot(xdmg.pca$loadings[,1])
barplot(xdmg.pca$loadings[,2])
barplot(xdmg.pca$loadings[,6])

# Cumulative variance

source("C:\\Users\\J\\Desktop\\Fall 15\\APMA 6430_STAT\\R\\Code\\PCAplots.R")

cumplot(xdmg.pca, col = "blue")

#***********************************************************
#
#		PCA for Possible predictors of damage	
#
#***********************************************************


pred.pca <- princomp(xdmg[,c("ACCDMG", "TRNSPD", "TONS", "CARS", "TIMEHR", "TEMP")], cor = T )


biplot(pred.pca)


#**********************************************
# Interaction plots with quantitative variables
#**********************************************


#  Extreme Damage

Speed <- cut(xdmg$TRNSPD, c(min(xdmg$TRNSPD),median(xdmg$TRNSPD),max(xdmg$TRNSPD)), include.lowest = T, labels = c("low speed", "high speed"))

Weight <- cut(xdmg$TONS, c(min(xdmg$TONS),median(xdmg$TONS),max(xdmg$TONS)), include.lowest = T, labels = c("light", "heavy"))

interaction.plot(Speed, Weight, xdmg$ACCDMG, ylab = "Cost ($)")

# Do this for casualty ******************** getting errors for 156 ***********

interaction.plot(Speed, Weight, CASUALTY, ylab = "Cost ($)")

xcas <- totacts[totacts$CASUALTY > 0,]
dim(xcas)


#*************************************************
#
#   Creating new variables
#
#*************************************************


# Type of accident

summary(totacts$TYPE)

# Make it a categoical variable

totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))

totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative"))


summary(totacts$TYPE)

# Type of Train

summary(totacts$TYPEQ)

totacts$TYPEQ[1:50]

summary(as.factor(totacts$TYPEQ))

# Converting missing to NA

totacts$TYPEQ[which(totacts$TYPEQ == "")] <- NA


# New labels


totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B", "C", "D", "E"))


# Add type E

levels(totacts$TYPEQ) <- c(levels(totacts$TYPEQ), "E")

summary(totacts$TYPEQ)


# get this for all acciddents (totacts) 


totacts$TYPEQ[which(totacts$TYPEQ == "")] <- NA


# New labels


totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint", "Maint of Way", "B", "C", "D", "E"))

summary(totacts$TYPEQ)





# Cause

summary(totacts$CAUSE)

# Create a new variable called Cause
# that uses labels for cause.
# Add it to totacts.

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

totacts$Cause <- factor(totacts$Cause)

summary(totacts$Cause)

# Change for xdmg and xcas

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]
xcas <- totacts[totacts$CASUALTY > 0,]

# Do a check


#*************************************************
#
#   Graphics for Categorical Variables
#
#*************************************************


#*****************
# Type of accident
#*****************

# Basic bar plot
barplot(table(xdmg$TYPE)) #compare with the totacts plot


# Put all bar plots together

barplot(t(100*cbind(table(totacts$TYPE)/nrow(totacts), table(xdmg$TYPE)/nrow(xdmg), table(xcas$TYPE)/nrow(xcas))), horiz = T, cex.names = .7, las =1, beside = T, legend.text = c("All Accidents", "Extreme Damage", "Extreme Casualty"), col = c("steelblue", "salmon", "lightcyan"), args.legend = list(x = 40, y = 60), xlab = "% of Accidents")



#*****************
# Type of train
#*****************

barplot(table(xdmg$TYPEQ)) #compare with the totacts plot

# Put all bar plots together

barplot(t(100*cbind(table(totacts$TYPEQ)/nrow(totacts), table(xdmg$TYPEQ)/nrow(xdmg), table(xcas$TYPEQ)/nrow(xcas))), horiz = T, cex.names = .7, las =1, beside = T, legend.text = c("All Accidents", "Extreme Damage", "Extreme Casualty"), col = c("steelblue", "salmon", "lightcyan"), xlab = "% of Accidents", args.legend = (list = c(x= 80, y = 50)))


#*****************
# Cause
#*****************

barplot(table(xdmg$Cause)) #compare with the totacts plot

# Put all bar plots together


barplot(t(100*cbind(table(totacts$Cause)/nrow(totacts), table(xdmg$Cause)/nrow(xdmg), table(xcas$Cause)/nrow(xcas))), horiz = T, cex.names = .7, las =1, beside = T, legend.text = c("All Accidents", "Extreme Damage", "Extreme Casualty"), col = c("steelblue", "salmon", "lightcyan"), xlab = "% of Accidents", args.legend = list(x = 70, y =25))



#*********************************
# Distributions for categorial variables
#*********************************

library(lattice)

bwplot(ACCDMG~Cause, data = xdmg)

# repeat for TYPE and TYPEQ

# scatter plots

xyplot(ACCDMG~TRNSPD | Cause, data = xdmg, type = c("p", "r"))

# Repeat for TYPE and TYPEQ



#*********************************
# Interaction with Speed
#*********************************

#**********************
# Extreme Damage
#**********************


# Create the Derail variable & 
# then look at interactions with Cause

xdmg$Derail <- (xdmg$TYPE == "Derailment")

xyplot(ACCDMG~TRNSPD | Cause * Derail, data = xdmg, type = c("p", "r"))

# Create a Freight variable
# then look at interactions with Cause &
# TYPE

xdmg$Freight <- (xdmg$TYPEQ == "Freight")

xyplot(ACCDMG~TRNSPD | Cause * Freight, data = xdmg, type = c("p", "r"))

# Derail and Freight

xyplot(ACCDMG~TRNSPD | Derail * Freight, data = xdmg, type = c("p", "r"))

