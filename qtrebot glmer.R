### this script will be TESTS only, and needs to be preceeded by a general setup script.
library(nlme)
library(lme4)
library(effects)


# Model failed to converge with max|grad| = 0.00974085 (tol = 0.001, component 1)  
# too many vars?
glmer1 <- glmer(dec ~ avgbot + req + path + gender + (product|subj), data=qtsot, family=binomial)
summary(glmer1)  
plot(allEffects(glmer1))

# Focus (measured by avgbotA) predicts*** decision for either product.  Endow does not. 
glmer1.0 <- glmer(dec ~ avgbot + path + ( product | subj), data=qtsot, family=binomial)
summary(glmer1.0)  # both products are in this model, so using avgbot for both
plot(allEffects(glmer1.0))

# Adding ENDOW and REQ into the model, but neither of them are signif and Focus still is (***)
# lets try the same thing using avgbotA orthogonal to ice cream. 
glmer1.01 <- glmer(dec ~ avgbot + path + req + (product|subj), data=qtsot, family=binomial)
summary(glmer1.01)

# Orthogonal: What a person decides about IC is predicted* by Focus predom taken from Shampoo thoughts!
glm0 <- glm(dec ~ avgbotA, data=qtsubice, family=binomial)
summary(glm0)
plot(allEffects(glm0))

# trending for focus, nothing for endow
glm0.1 <- glm(dec ~ avgbotA + path, data=qtsubice, family=binomial)
summary(glm0.1)
plot(allEffects(glm0.1))

# REQ is signficant, nothing else is
glm0.2 <- glm(dec ~ avgbotA + path + req, data=qtsubice, family=binomial)
summary(glm0.2)
plot(allEffects(glm0.2))

# REQ signficant, Focus trend 
glm0.3 <- glm(dec ~ avgbotA + req, data=qtsubice, family=binomial)
summary(glm0.3)
plot(allEffects(glm0.3))

# REQ signficant on it's own.
glm0.4 <- glm(dec ~ req, data=qtsubice, family=binomial)
summary(glm0.4)
plot(allEffects(glm0.4))

# REQ does not predict the Shampoo decision - good.  It's working. 
glm0.41 <- glm(dec ~ req, data=qtsubshamp, family=binomial)
summary(glm0.41)
plot(allEffects(glm0.41))

# REQ signficant, endow is NOT
glm0.5 <- glm(dec ~ req + path, data=qtsubice, family=binomial)
summary(glm0.5)
plot(allEffects(glm0.5))

glm0 <- glm(dec ~ avgbotA, data=qtsubice, family=binomial)
summary(glm0)
plot(allEffects(glm0))
# No interaction between avgbot and endowment for all the products
glmer1.1i <- glmer(dec ~ avgbot * path + (product|subj), data=qtsot, family=binomial)
summary(glmer1.1i)
plot(allEffects(glmer1.1i)) 

# Trending interaction for the IC decision only, between endow and avgbot
glm1i <- glm(dec ~ avgbotA * path, data=qtsubice, family=binomial)
summary(glm1i)
plot(allEffects(glm1i))

# REQ alone predicts the decision to take IC
glm2 <- glm(dec ~ req, data=qtsubice, family=binomial)
summary(glm2)
plot(allEffects(glm2))

glm2.1  <- glm(dec ~ avgbotA, data=qtsubice, family=binomial)
summary(glm2.1)
plot(allEffects(glm2.1))

glmer1.1i <- glmer(dec ~ avgbot * path + (product|subj), data=qtsot, family=binomial)
summary(glmer1.1i)
plot(allEffects(glmer1.1i))

glmer1.1i <- glmer(dec ~ avgbotA * path + (product|subj), data=qtsubice, family=binomial)
summary(glmer1.1i)
plot(allEffects(glmer1.1i))

glmer1.2a <- glmer(dec ~ path * re + (product|subj), data=qtsot, family=binomial)
summary(glmer1.2a)
plot(allEffects(glmer1.2a))

glmer1.2i <- glmer(dec ~ avgbotA * path + (product|subj), data=qtsot, family=binomial)
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

#######
glm0 <- glm(dec ~ avgbotA + path, data=qtsubice, family=binomial)
summary(glm0)
plot(allEffects(glm0))

glm0.1 <- glm(dec ~ path, data = qtsubice, family=binomial)
summary(glm0.1)
plot(allEffects(glm0.1))

glm1 <- glm(dec ~ avgbotA * path, data=qtsubice, family=binomial)
summary(glm1)
plot(allEffects(glm1))

glm2 <- glm(dec ~ avgbotA + path, data=qtsubice, family=binomial)
summary(glm2)
plot(allEffects(glm2))

glm3 <- glm(dec ~ avgbotA + path, data=qtsubshamp, family=binomial)
summary(glm3)
plot(allEffects(glm3))

glm4 <- glm(dec ~ avgbot + req, data=qtsubice, family=binomial)
summary(glm4)
plot(allEffects(glm4))

glm4i <- glm(dec ~ avgbotA * req, data=qtsubice, family=binomial)
summary(glm4i)
plot(allEffects(glm4i))

# Endowment does not predict the taking of ice cream
glm5 <- glm(dec ~ path, data=qtsubice, family=binomial)
summary(glm5)
plot(allEffects(glm5))

# Endowment does not predict the taking of shampoo
glm5.1 <- glm(dec ~ path, data=qtsubshamp, family=binomial)
summary(glm5.1)
plot(allEffects(glm5.1))

# Endowment does not predict the taking of 1 or the other. (using whole dataframe)
glm6 <- glm(dec ~ path, data=qtsot, family=binomial)
summary(glm6)
plot(allEffects(glm6))


###### Linear Mixed Models  ####
# not working right now # Jan 31st

lme1  <- lmer(avgbot ~ aspect + (subj|product), data=qtsot)
summary(lme1)
lme2  <- lmer(avgbot ~ path   + (subj|product), data=qtsot)
summary(lme2)

#aggregate(qt, by=product, FUN=mean)
