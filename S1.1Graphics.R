

#********************************************************************************
#
#					Univariate Graphics
#
#********************************************************************************


#************************************
# Reading in data
#************************************

# Set working directory

# For example, an iOS user

setwd("/media/yang/Data/UVa Course/APMA6430/quiz1/")

# or a windows user

setwd("C:/me/data/TrainAccidents")


#***********************************************

# Read in the accident files one at at time


acts14 <- read.table("RailAccidents14.txt", sep = ",", header = T)

# Since the files are really csv you can use

acts14 <- read.csv("RailAccidents14.txt")


#**************************************************

# To get a summary of all of the variables use


# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
# you can use



# To get individual statistics (e.g. mean, var) you can use



# You can round your answer using the round() function


#**************************************************

# You will need to read in all 14 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:

source("AccidentInput.R")


# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

acts <- file.inputl(path) 

# E.G.

acts <- file.inputl("/Users/donaldbrown/Dropbox/department/Classes/SYS4021/sys 421 2012/data/TrainAccidents/")

# path is the specification of the path to your file.

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.

# Before we put all the data into one data frame
# we must clean the data


##################################################
#
#	Data Cleaning
#
##################################################

#************************************************
# Variable names

matrix(names(acts[[1]]))

matrix(names(acts[[8]]))

# Notice that the number of columns changes from year to year

ncol(acts[[1]])
ncol(acts[[8]])


# Get a common set the variables
	
	comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

	
	# Now combine the data frames for all 12 years
	# Use combine.data()
		
		
	totacts <- combine.data(acts, comvar)

# How many accidents?

	dim(totacts)

# View of accident damage

boxplot(totacts$ACCDMG)

hist(totacts$ACCDMG)

#*************************************************
# Accident Reports

# Look at the most costly accident in the data set

which(totacts$ACCDMG == max(totacts$ACCDMG))


# Check out the narratives for this extreme accident



# How do we find duplicates?

# Are there other duplicates?

duplicated(totacts[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# why not use latitude and longitude?

# why not use INCDTNO?

totacts[totacts$INCDTNO == "1", 1:10]


# Remove duplicates

totacts <- totacts[!duplicated(totacts[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]),]

#*******************************************
# What is the second most extreme accident?


# what should we do?


#********************************************
# Missing data

# Do a summary of totacts

names(summary(totacts$Latitude))


# Are we missing values for any variables?


# How many?

nafind <- function(x){sum(is.na(x))}

apply(totacts,2, "nafind")

# Do we need all the variables?

matrix(names(totacts))

# Remove unnecessary variables, then get a summary

nacount <- apply(totacts,2, "nafind")

varWna <- which(nacount > 0)

# Keep TYPEQ, we'll use it. The others we don't need.

which(colnames(totacts)[varWna] == "TYPEQ")

varWna <- varWna[-which(colnames(totacts)[varWna]== "TYPEQ")]


totacts <- totacts[, -varWna]

# Save your data frame

# check you working directory and change it if necessary
getwd()

write.csv(totacts, file = "totactsClean.csv")

#***********************************
#
# 	Summary of accidents
#
#***********************************

# Get a summary of the cleaned data set


# How many accidents?



# Total cost of all accidents


# Average annual cost of accidents
# first yearly costs (sums)
dmgyrsum <- tapply(totacts$ACCDMG, totacts$YEAR, sum)

#then average


# Total number killed


# Largest number killed in an accident 


# Total number injured


# Largest number injured in an accident 


# What is the average number of injuries per year?


# types of variables

str(totacts)

#**************************************************
#
#   Time series of Accidents
#
#**************************************************

# Yearly no. of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, length), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Accidents per Year", lwd =2)


# Yearly total cost of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, sum), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

# Yearly maximum cost of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

# Putting total and maximum together using symbols 

symbols(2001:2014, tapply(totacts$ACCDMG, totacts$YEAR, sum), circles=tapply(totacts$ACCDMG, totacts$YEAR, max), inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total Accident Damage")
lines(2001:2014, tapply(totacts$ACCDMG, totacts$YEAR, sum))


# Repeat this for total killed and total injured and the sum of them.



#***********************************
#
# 	histograms of ACCDMG and TEMP
#
#***********************************

# These examples are for 2011 

hist(acts[[11]]$ACCDMG) # for 2011

hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# Different bin widths

par(mfrow = c(2,2))

hist(totacts$TEMP, breaks = "scott", main = "Accident Temperatures (Scott)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = "fd", main = "Accident Temperatures (FD)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, main = "Accident Temperatures (Sturges)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = 100, main = "Accident Temperatures (100)", xlab = "Temp (F)", col = "steelblue")

par(mfrow = c(1,1))

# Different bin widths

hist(acts[[11]]$ACCDMG, breaks = "scott", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = "fd", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 20, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 100, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# other years


par(mfrow = c(2,2))
hist(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))



#*********************************************************************
#
# 				Box Plots of Metrics
#         and Extreme Accidents
#
#*********************************************************************

#*****************************
# ACCDMG

boxplot(totacts$ACCDMG)

# Plot only the extreme points
# (extreme defined by the box plot rule)

# Get the values in the box plot

dmgbox <- boxplot(totacts$ACCDMG)


# How many extreme damage accidents?

length(dmgbox$out)

# What proportion of accidents are extreme? (round to 2 digits)



# What is the proportion of costs for extreme damage accidents? (round to 2 digits)


# Create a data frame with just the extreme ACCDMG accidents

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

dim(xdmg)

# Look at the boxplots and histograms of these extreme accidents

boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Accident Damage per Year")

# also plot number of accidents per year.

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, length), type = "l", xlab = "Year", ylab = "No. of Accidents", main = "Number of Accidents per Year")

# Frequency of accident types

barplot(table(xdmg$TYPE)) #compare with the totacts plot

# Repeat for TOTKLD and TOTINJ
# Create a variable called Casualty = TOTKLD + TOTINJ

