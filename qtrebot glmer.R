### this script will be TESTS only, and needs to be preceeded by a general setup script.

library(nlme)
library(lme4)

glmer1 <- glmer(dec ~ avgbot + req + path + gender + (product|subj), data=qtsot, family=binomial)
summary(glmer1)  # Model failed to converge with max|grad| = 0.00974085 (tol = 0.001, component 1)  #too many vars?
library(effects)
plot(allEffects(glmer1))

glmer1.0 <- glmer(dec ~ avgbot + path + ( product | subj), data=qtsot, family=binomial)
summary(glmer1.0)  # this model does not fail to converge
plot(allEffects(glmer1.0))

glmer

glmer1.1 <- glmer(dec ~ avgbot + product + (product|subj), data=qtsot, family=binomial)
summary(glmer1.1)
plot(allEffects(glmer1.1)) # strong bot, trending by product

glmer1.1i <- glmer(dec ~ avgbot * product + (product|subj), data=qtsot, family=binomial)
summary(glmer1.1i) # Model failed to converge with max|grad| = 0.0967525 (tol = 0.001, component 1) # Model is nearly unidentifiable: very large eigenvalue # Rescale variables?
plot(allEffects(glmer1.1i))

glmer1.2 <- glmer(dec ~ avgbot * path + (product|subj), data=qtsot, family=binomial)
summary(glmer1.2)
plot(allEffects(glmer1.2))

glmer1.2a <- glmer(dec ~ path * re + (product|subj), data=qtsot, family=binomial)
summary(glmer1.2a)
plot(allEffects(glmer1.2a))

glmer1.2i <- glmer(dec ~ avgbot * path + (product|subj), data=qtsot, family=binomial)
summary(glmer1.2i)
plot(allEffects(glmer1.2i)) # no interaction 

glmer1.3 <- glmer(dec ~ avgbot + re + (product|subj), data=qtsot, family=binomial)
summary(glmer1.3)
plot(allEffects(glmer1.3)) # no interaction 

glmer3.1 <- glmer(dec ~ path + re + (product|subj), data=qtsot, family=binomial)
summary(glmer3.1)
plot(allEffects(glmer3.1)

glmer3.1i <- glmer(dec ~ path * req + (product|subj), data=qtsot, family=binomial)
summary(glmer3.1i)
plot(allEffects(glmer3.1i)     

glmer3.2i <- glmer(dec ~ avgbot * req + (product|subj), data=qtsot, family=binomial)
summary(glmer3.2i)
plot(allEffects(glmer3.2i)      
     
glmer1.3i <- glmer(dec ~ avgbot * re + (product|subj), data=qtsot, family=binomial)
summary(glmer1.3i)

glmer1.4 <- glmer(dec ~ avgbot + gender + (product|subj), data=qtsot, family=binomial)
summary(glmer1.4) # fail to converge

glmer1.5 <- glmer(dec ~ req + avgbot + (product|subj), data=qtsot, family=binomial)
summary(glmer1.5) #  fails to converge

glmer2 <- glmer(dec ~ gender * req + ( subj | product), data=qtsot, family=binomial)
summary(glmer2)
plot(allEffects(glmer2))

glmer1.4i <- glmer(dec ~ avgbot * gender + (1|subj), data=qtsot, family=binomial)
summary(glmer1.4i)

glmer2.1 <- glmer(dec ~ req + gender + (1|subj), data=qtsot, family=binomial)
summary(glmer2)
plot(allEffects(glmer2.1))

###### Linear Mixed Models  ####
# not working right now # Jan 31st

lme1  <- lmer(avgbot ~ aspect + (subj|product), data=qtsot)
summary(lme1)
lme2  <- lmer(avgbot ~ path   + (subj|product), data=qtsot)
summary(lme2)

#aggregate(qt, by=product, FUN=mean)
