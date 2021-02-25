install.packages("lmPerm")
install.packages("coin")
install.packages("gtools")

library("ggplot2"); theme_set(theme_bw())
library("lmPerm")
library("coin")
library("gtools")

ants <- read.csv(file="ants.csv", header=TRUE)
ants$place <- factor(ants$place)
print(ants)

print(ggplot(ants,aes(place,colonies))
      + geom_boxplot(fill="lightgray")
      + stat_sum(alpha=0.7)
      
      + scale_size(breaks=1:2, range=c(3,6))
)

set.seed(101) ## for reproducibility
nsim <- 9999
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(ants))
  bdat <- transform(ants,colonies=colonies[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$place=="field","colonies"])-
    mean(bdat[bdat$place=="forest","colonies"])
}
obs <- mean(ants[ants$place=="field","colonies"])-
  mean(ants[ants$place=="forest","colonies"])
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

