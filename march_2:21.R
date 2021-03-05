lizards <- read.csv(file = "lizards.csv", stringsAsFactors=TRUE)
lizards$time <- factor(lizards$time,levels=c("early","midday","late"))
lmint <- lm(grahami~light*time, data=lizards) #interaction model
lmint
lmboth <- lm(grahami~light + time, data=lizards) #additive model, no interaction btw light and time
lmlight <- lm(grahami~light, data=lizards)
lmtime <- lm(grahami~time, data=lizards) #response variable~time

summary(lmboth)
#lightsunny is the difference btw shady (base value) and sunny

#test model with time to without time
anova(lmboth, lmlight, test="F")
#give you the effect  of time (combination of both time parameters)

#test light
anova(lmboth, lmtime, test="F")

#drop parameters one at a time and comparing to full model
drop1(lmboth, test="F")

install.packages("car")

#more convenient, make ANOVA table 
car::Anova(lmboth)

print(summary(lmint))
