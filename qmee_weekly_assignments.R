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

## JD: I needed to add selectors to run this code
plot.default(guppy$max.parasite.count, guppy$standard.length,
     ylab="Maximum Parasite Count", xlab="Standard Length")

## A2 grade 1.9/3. Not exactly sure what you checked here or what you learned

##New code for assignment 3

library(tidyverse)

## BMB: you should generally comment out these interactive-only function ...
view(guppy)

library(dplyr)

#paring down columns to only the ones I need
## BMB: good!
pdat <- select(guppy, treatment, day, parasite.count)


#re-arranging the data by treatment and day
timedat <- (pdat
                %>% group_by(treatment, day)
                %>% summarise_at("parasite.count",
                                 list(mean=mean, sd=sd), na.rm=TRUE))
print(timedat)


## BMB: usually _don't_ want to split things up and work one
## group at a time
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

## BMB: what about those SDs you worked so hard for?
print(pt+geom_line() +geom_point() +
      geom_ribbon(aes(ymin=mean-2*sd,ymax=mean+2*sd,
                      fill=treatment), colour=NA, alpha=0.2))
## Hmm, maybe we would like to use SE rather than SD ... or use
## a more count-based statistic of uncertainty ...

print(pt+geom_point() +
      geom_smooth(method="glm",
                  aes(fill=treatment),
                  method.args=list(family=quasipoisson),
                  formula=y~poly(x,2),
                  alpha=0.2)
      )


#taking the total parasite count over all days and comparing treatments 
totdat <- select(guppy, fish.Id, treatment, parasite.count)


#took the sum of the number of parasites over all sampling days for each individual
totdatgrp <- (totdat
            %>% group_by(fish.Id, treatment)
            %>% summarise_at("parasite.count",
                             list(sum=sum), na.rm=TRUE))
print(totdatgrp)

#took the mean total parasite for each treatment
totdatgrp2 <- (totdatgrp
               %>% group_by(treatment)
               %>% summarise_at("sum",
                                list(mean=mean, sd=sd), na.rm=TRUE))
print(totdatgrp2)

#plotting the total parasite count over all sampling days across treatments (boxplot)
btot <- ggplot(totdatgrp2,aes(treatment, mean))+
  labs(y ="Mean total parasite count", x="Treatment")

print(btot+geom_bar(stat = "identity", width = 0.5, fill="steelblue"))+
  theme_minimal()

## BMB: I'm not thrilled with this one since it displays so much less
## information, but OK

## grade: 2.1/3



##Assignment 5 (Permutations)

library("ggplot2"); theme_set(theme_bw())
library("lmPerm")
library("coin")
library("gtools")

#bringing in condensed guppy data for all 3 treatments
bguppy <- read.csv(file = "guppy3.csv", head= TRUE)
summary(bguppy)

ggplot(bguppy,aes(treatment,tot.parasite.count)) + geom_boxplot()
#permutation test 1
summary(lmp(tot.parasite.count~treatment, data=bguppy))

#bringing in condensed guppy data of only noise stressors
cguppy <- read.csv(file = "guppy2.csv", head= TRUE)
summary(cguppy)

cguppy$treatment <- factor(cguppy$treatment)
## BMB: do we need to see the whole thing? summary(), skimr::skim() ?
print(cguppy)


#visualizing mean total parasite count by treatment
print(ggplot(cguppy,aes(treatment,tot.parasite.count))
      + geom_boxplot(fill="lightgray")
      + stat_sum(alpha=0.7)
      + scale_size(breaks=1:2, range=c(3,6))
      + labs(y="Mean total parasite count", x="Treatment")
)

#permutation test 2
set.seed(101) 
nsim <- 9999
res <- numeric(nsim) 
for (i in 1:nsim) {
  perm <- sample(nrow(cguppy))
  bdat <- transform(cguppy,tot.parasite.count=tot.parasite.count[perm])
  res[i] <- mean(bdat$tot.parasite.count[bdat$treatment=="acute noise"])-
    mean(bdat$tot.parasite.count[bdat$treatment=="chronic noise"])
}

obs <- mean(cguppy$tot.parasite.count[cguppy$treatment=="acute noise"])-
  mean(cguppy$tot.parasite.count[cguppy$treatment=="chronic noise"])

res <- c(res,obs)
2*mean(res>=obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

## grade: 2

##Assignment 6 (linear models)

linguppy <- read.csv(file="guppylin1.csv", stringsAsFactors=TRUE)
linguppy$treatment <- factor(linguppy$treatment,levels=c("No Noise","acute noise","chronic noise"))
linguppy$treatment

#diagnostic plot

lmtpsl <- lm(tot_parasite~standard.length, data=linguppy)
lmtpsl
plot(lmtpsl, id.n=4)


lmint <- lm(tot_parasite~standard.length*treatment, data=linguppy)
summary(lmint) 
lmboth <- lm(tot_parasite~standard.length + treatment, data=linguppy)
summary(lmboth)
lmparasite <- lm(tot_parasite~standard.length, data=linguppy)
summary(lmparasite)
lmtreatment <- lm(tot_parasite~treatment, data=linguppy)
summary(lmtreatment)

anova(lmboth, lmparasite, test="F")
drop1(lmboth, test="F")
car::Anova(lmboth)


#inferential plot
## install.packages("emmeans")
## BMB: do NOT (please) put install.packages() in your code
## (if necessary, comment it out)

library(emmeans)
e1 <- emmeans(lmboth, "treatment")
pairs(e1)
plot(e1)

## JD: Very little discussion, even after a lot of back-and-forth
## Grade: 1.8/3


#Assignment 7 (generalized linear models)

#diagnostic plot
genlinguppy <- read.csv(file="gen_lin_guppy.csv", stringsAsFactors=TRUE)

## BMB: let's take a look
print(ggplot(genlinguppy, aes(day,parasite.count))
    + geom_violin(aes(group=day),scale="width",fill="red",alpha=0.3)
    + geom_point()
    + geom_smooth(method="glm", method.args=list(family=quasipoisson))
    + geom_smooth(colour="cyan")
    + geom_smooth(method="glm", method.args=list(family=quasipoisson),
                formula=y~poly(x,2), colour="purple")
    )
## the cyan curve is a nonparametric (loess) fit.

g1 <- glm(parasite.count~day,genlinguppy,family=quasipoisson(link="log"))
summary(g1)
plot(g1)
## the residuals have a broad range, which makes the curve/nonlinearity
## in the residuals vs fitted plot look less important than it really
## is.  The 'badness' of the Q-Q and scale-location plots are driven
## mostly by this nonlinearity

g2 <- update(g1, . ~ poly(day,2))
plot(g2)
## hmm, still bad heteroscedasticity ...

g3 <- MASS::glm.nb(parasite.count~poly(day,2), data=genlinguppy)
plot(g3) ## BMB: much better (although still skewed ...)

library(DHARMa)
plot(simulateResiduals(g3))

## looking for other information that would help us out
pairs(genlinguppy[,-1],gap=0)
car::scatterplotMatrix(genlinguppy[,-1])
GGally::ggpairs(genlinguppy,columns=2:4)

#inferential plot using emmeans

library(effects)

allEffects(g1)
plot(predictorEffects(g1))

## BMB: Looks OK.
## grade: 2






##Assignment 8

library("lattice") 
library("R2jags")
library("rstanarm")
library("arm")        
library("coda")
library("emdbook")    
library("dotwhisker")
library("broom.mixed")
library("ggplot2"); theme_set(theme_bw())
library("R2WinBUGS")
library("rjags")
namedList <- lme4:::namedList  
library(dplyr)
library(readr)
library(R2jags)
library(coda)
library(broom.mixed)


guppybayes <- (read_csv("guppylin1.csv")
            %>% mutate(treatment=factor(treatment,levels=c("chronic noise","No Noise","acute noise")))
            %>% mutate_if(is.character,factor)
)

guppydat1 <- with(guppybayes,
                namedList(N=nrow(guppybayes),            
                           ntreatment=length(levels(treatment)), 
                           treatment=as.numeric(treatment),     
                           tot_parasite))

treatmentmodel1 <- function() {
  for (i in 1:N) {
    logmean[i] <- b_treatment[treatment[i]]   
    pred[i] <- exp(logmean[i])      
    tot_parasite[i] ~ dpois(pred[i])
  }
  for (i in 1:ntreatment) {
    b_treatment[i] ~ dnorm(0,0.001)
  }
}

j1 <- jags(data=guppydat1,
           inits=NULL,
           parameters=c("b_treatment"),
           model.file=treatmentmodel1)
plot(j1)

broom.mixed::tidy(j1,conf.int=TRUE, conf.method="quantile")


#comparison to analogous frequentist fit

linguppy <- read.csv(file="guppylin1.csv", stringsAsFactors=TRUE)
linguppy$treatment <- factor(linguppy$treatment,levels=c("No Noise","acute noise","chronic noise"))

lmtreatment <- lm(tot_parasite~treatment, data=linguppy)
summary(lmtreatment)


















