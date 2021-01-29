# BMB: please try to avoid file names with spaces in them ...

# BMB: this is harmless, but sort of useless (it may be
# different for every person running the analysis)
#this is where I will complete assignment 1
getwd()

#my chosen dataset
guppy <- read.csv(file = "guppy.csv", head= TRUE)
# BMB: avoid printing large objects (summary(), head(), etc. are good)
guppy

#create objects for each column
# BMB: avoid attach(), even though it adds a bit more typing
# attach(guppy)

#check the sample size for each treatment
table(guppy$treatment)

#mean max parasite number out of the 10 days observed
with(guppy,tapply(max.parasite.count, fish.Id, mean))

#mean max parasite number for each treatment, omitting all missing data
with(guppy,tapply(max.parasite.count, treatment, mean, na.rm=TRUE))

#standard deviation max parasite number for each treatment, omitting all missing data
with(guppy,tapply(max.parasite.count, treatment, sd, na.rm=TRUE))

#test for statistical difference of max parasite number between treatments
guppy.aov <- aov(max.parasite.count ~ treatment, data=guppy)
summary(guppy.aov)

## This is fine. Score: 2
## BMB: Some of these computations can also be done via tidyverse, if you like,
## e.g.

library(tidyverse)
(guppy
    %>% group_by(treatment)
    %>% summarise_at("max.parasite.count",
                     list(mean=mean, sd=sd), na.rm=TRUE)
)

## it's absolutely up to you to decide what you prefer.

## score: 2
