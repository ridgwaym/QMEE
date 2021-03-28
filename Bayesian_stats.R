##Bayesian example

library("lattice") ## built-in
## model-fitting
library("R2jags")
library("rstanarm")
library("arm")        ## for coefplot, bayesglm
## handling fitted models (diagnostics & inference)
library("coda")
library("emdbook")    ## for lump.mcmc.list() and as.mcmc.bugs()
library("dotwhisker")
library("broom.mixed")
library("ggplot2"); theme_set(theme_bw())
library("R2WinBUGS")
library("rjags")
namedList <- lme4:::namedList  ## utility

set.seed(411)
N <- 40
## predictor variables
a <- runif(N)
b <- runif(N)
c <- runif(N)
y <- rnorm(N,mean=2+1*a+4*b+1*c,sd=1)
dat <- data.frame(y,a,b,c)

print(summary(dat))

summary(lm(y~a+b+c,data=dat))

jags1 <- jags(model.file="bayes.bug",
              parameters=c("ma","mb","mc","int"),
              data = namedList(a, b, c, N, y),
              n.chains = 4,
              inits=NULL)

bb <- jags1$BUGSoutput  ## extract the "BUGS output" component
mm <- as.mcmc.bugs(bb)  ## convert it to an "mcmc" object that coda can handle
plot(jags1)             ## large-format graph
## plot(mm)                ## trace + density plots, same as above
xyplot(mm,layout=c(2,3))  ## prettier trace plot
densityplot(mm,layout=c(2,3)) ## prettier density plot
print(dwplot(jags1))              ## estimate + credible interval plot

summary(b1 <- bayesglm(y~a+b+c,data=dat)) ## LINEAR model

b2 <- bayesglm(y~a+b+c,data=dat, prior.scale=0.1)
dotwhisker::dwplot(list(flat=b1,shrunk=b2)) + geom_vline(xintercept=0,lty=2)


library(dplyr)
library(readr)
library(R2jags)
library(coda)
library(broom.mixed)

lizards <- (read_csv("lizards.csv")
            %>% mutate(time=factor(time,levels=c("early","midday","late")))
            %>% mutate_if(is.character,factor)
)






