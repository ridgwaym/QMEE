#this is where I will complete assignment 1
getwd()

#my chosen dataset
guppy <- read.csv(file = "guppy.csv", head= TRUE)
guppy

#create objects for each column
attach(guppy)

#check the sample size for each treatment
table(treatment)

#mean max parasite number out of the 10 days observed
tapply(max.parasite.count, fish.Id, mean)

#mean max parasite number for each treatment, omitting all missing data
tapply(max.parasite.count, treatment, mean, na.rm=TRUE)

#standard deviation max parasite number for each treatment, omitting all missing data
tapply(max.parasite.count, treatment, sd, na.rm=TRUE)

#test for statistical difference of max parasite number between treatments
guppy.aov <- aov(max.parasite.count ~ treatment)
summary(guppy.aov)
