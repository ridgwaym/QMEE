#this is where I will complete assignment 1
getwd()

#my chosen dataset
guppy <- read.csv(file = "guppy.csv", head= TRUE)
summary(guppy)

#create objects for each column

#check the sample size for each treatment
table(guppy$treatment)

#mean max parasite number out of the 10 days observed
with(guppy, tapply(max.parasite.count, fish.Id, mean))

#mean max parasite number for each treatment, omitting all missing data
with (guppy,tapply(max.parasite.count, treatment, mean, na.rm=TRUE))

#standard deviation max parasite number for each treatment, omitting all missing data
with(guppy,tapply(max.parasite.count, treatment, sd, na.rm=TRUE))

#test for statistical difference of max parasite number between treatments
guppy.aov <- aov(max.parasite.count ~ treatment, data=guppy)
summary(guppy.aov)

#New code for assignment 2
library(tidyverse)

(guppy
  %>% group_by(treatment)
  %>% summarise_at("max.parasite.count",
                  list(mean=mean, sd=sd), na.rm=TRUE))

#look for any inconsistencies in data
summary(guppy)

boxplot (max.parasite.count~treatment, data=guppy, 
         xlab = "Treatment", ylab = "Maximum Parasite Count")


plot.default(max.parasite.count, standard.length,
     ylab="Maximum Parasite Count", xlab="Standard Length")



##New code for assignment 3

library(tidyverse)

view(guppy)

library(dplyr)

#pairing down columns to only the ones I need
pdat <- select(guppy, treatment, day, parasite.count)


#re-arranging the data by treatment and day
timedat <- (pdat
                %>% group_by(treatment, day)
                %>% summarise_at("parasite.count",
                                 list(mean=mean, sd=sd), na.rm=TRUE))
print(timedat)


#looking at each treatment independently - chronic noise first
pdat_chr <- filter(pdat, treatment == "chronic noise")

#re-arranging data by day
timedat_chr <- (pdat_chr
            %>% group_by(day)
            %>% summarise_at("parasite.count",
                             list(mean=mean, sd=sd), na.rm=TRUE))
print(timedat_chr)


#looking at acute noise only
pdat_a <- filter(pdat, treatment == "acute noise")

#sorting by day of sampling
timedat_a <- (pdat_a
                %>% group_by(day)
                %>% summarise_at("parasite.count",
                                 list(mean=mean, sd=sd), na.rm=TRUE))
print(timedat_a)


#looking at control group only
pdat_no <- filter(pdat, treatment == "No Noise")

#sorting by say of sampling
timedat_no <- (pdat_no
              %>% group_by(day)
              %>% summarise_at("parasite.count",
                               list(mean=mean, sd=sd), na.rm=TRUE))
print(timedat_no)



library(ggplot2)
theme_set(theme_bw()) 

#plotting each treatment separately
#chronic
pchr <- ggplot(timedat_chr,aes(day, mean))+labs(y = "Parasite count", x="Sampling day")
print(pchr+geom_line())
print(pchr+geom_line() +geom_point())

#acute
pa <- ggplot(timedat_a,aes(day, mean))+labs(y = "Parasite count", x="Sampling day")
print(pa+geom_line())
print(pa+geom_line() +geom_point())

#control
pno <- ggplot(timedat_no,aes(day, mean))+labs(y = "Parasite count", x="Sampling day")
print(pno+geom_line())
print(pno+geom_line() +geom_point())

#plotting all 3 treatments together (line)
pt <- ggplot(timedat,aes(day, mean, colour=treatment))+labs(y = "Parasite count", x="Sampling day")
print(pt+geom_line())
print(pt+geom_line() +geom_point())


#taking the total parasite count over all days and comparing treatments 
totdat <- select(guppy, fish.Id, treatment, parasite.count)


#took the sum of the number of parasites over all sampling days for each individual
totdatgrp <- (totdat
            %>% group_by(fish.Id, treatment)
            %>% summarise_at("parasite.count",
                             list(sum=sum), na.rm=TRUE))

#took the mean total parasite for each treatment
totdatgrp2 <- (totdatgrp
               %>% group_by(treatment)
               %>% summarise_at("sum",
                                list(mean=mean, sd=sd), na.rm=TRUE))


#plotting the total parasite count over all sampling days across treatments (boxplot)
btot <- ggplot(totdatgrp2,aes(treatment, mean))+
  labs(y ="Mean total parasite count", x="Treatment")

print(btot+geom_bar(stat = "identity", width = 0.5, fill="steelblue"))+
  theme_minimal()




