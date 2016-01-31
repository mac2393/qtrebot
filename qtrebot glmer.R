#qt <- read.csv("RFqt_Cleaned.csv", header=TRUE)
#qtt  <-  qt
# remove(RFqt_Cleaned)
library(ggplot2)
names(qt)
str(qt$avgbot)
str(qt$dec)
attach(qt)
library(lme4)

glmer1 <- glmer(dec ~ avgbot + req + path + gender + (product|subj), data=qt, family=binomial)
summary(glmer1)  # this is not right, thinks there are 204 subj
library(effects)
plot(allEffects(glmer1))

glmer1.1 <- glmer(dec ~ avgbot + product + (product|subj), data=qtsot, family=binomial)
summary(glmer1.1)
plot(allEffects(glmer1.1)) # strong bot, trending by product

glmer1.1i <- glmer(dec ~ avgbot * product + (product|subj), data=qtsot, family=binomial)
summary(glmer1.1i)
plot(allEffects(glmer1.1i))

glmer1.2 <- glmer(dec ~ avgbot + path + (product|subj), data=qtsot, family=binomial)
summary(glmer1.2)
plot(allEffects(glmer1.2))

glmer1.2i <- glmer(dec ~ avgbot * path + (product|subj), data=qtsot, family=binomial)
summary(glmer1.2i)

glmer1.3 <- glmer(dec ~ avgbot + re + (product|subj), data=qtsot, family=binomial)
summary(glmer1.3)

glmer1.3i <- glmer(dec ~ avgbot * re + (product|subj), data=qtsot, family=binomial)
summary(glmer1.3i)

glmer1.4 <- glmer(dec ~ avgbot + gender + (product|subj), data=qtsot, family=binomial)
summary(glmer1.4)

glmer1.5 <- glmer(dec ~ req + avgbot + (product|subj), data=qtsot, family=binomial)
summary(glmer1.5)

lme1  <- lme(avgbot ~ aspect + (subj|product), data=qtsot)
lme1  <- lme(avgbot ~ aspect + (product|product), data=qtsot)
lme2  <- lme(avgbot ~ path   + (product|subj), data=qtsot)

which(qt$subj=="12")
which(qt$product=="NA")
which(qt$re=="NA")
which(qt$path=="NA")
which(qt$subj=="NA")
which(qt$product=="NA")

s  <- glmer(dec ~ avgbot, random + 1~ product|subj, data=qt, family=binomial)
summary(s)

glmer1.4i <- glmer(dec ~ avgbot * gender + (1|subj), data=qt, family=binomial)
summary(glmer1.4i)

glmer2 <- glmer(dec ~ re + gender + (1|subj), data=qt, family=binomial)
summary(glmer2)
plot(allEffects(glmer2))

bot1 <- 
ggplot(qt, aes(avgbot, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  # facet_wrap(~path)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance \n Affects Decision to Take Product")+
  theme_bw()
ggsave('bot1.png', w=5, h=4, dpi=255)

bot2 <- 
  ggplot(qt, aes(avgbot, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  facet_wrap(~path)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance Affects\n Decision to Take Product by Product")+
  theme_bw()
ggsave('bot2.png', w=5, h=4, dpi=255)

bot3 <- 
  ggplot(qt, aes(avgbot, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  facet_wrap(~re)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance Affects\n Decision to Take Product by Eater Type")+
  theme_bw()
ggsave('bot3.png', w=5, h=4, dpi=255)

bot4 <- 
  ggplot(qt, aes(avgbot, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  facet_wrap(~gender)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance Affects\n Decision to Take Product by Gender")+
  theme_bw()
ggsave('bot4.png', w=5, h=4, dpi=255)

summary(lme(aspect ~ gender + random = ~ +1|subj, data=qt))

summary(lme(aspect ~ gender, data=qt, random = ~ +1 | subj, na.action="na.omit"))

summary(lme(aspect ~ avgbot, data=qt, random = ~ +1 | subj, na.action="na.omit"))

summary(lme(dec ~ avgbot, data=qt, random = ~ +1 | subj, na.action="na.omit"))

#aggregate(qt, by=product, FUN=mean)

library(lme4)
lmer(dec ~ product + (product | subj), data=qt)


ggplot(qt, aes(as.factor(gender), aspect))+ geom_point(position=position_jitter(w=.01, h=.19))+
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', color="blue", width=.25, size=1)+
  stat_summary(fun.y=mean, geom='point', colour="red", size=2.5)+
  theme_bw()


ggplot(qt, aes(avgbot, aspect)) +  
  geom_smooth(method="loess", size=2)+
  geom_point(color=gender, position=position_jitter())+
  theme_bw()


grid.arrange(bot1, bot2, bot3, bot4, ncol=2, nrow=2)



ggplot(qt, aes(avgbot, dec))+ geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial")+
  facet_wrap(~re)+
  theme_bw()

ggplot(qt, aes(req, dec))+ geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial")+
  facet_wrap(~path)+
  theme_bw()


glmer2 <- glmer(dec ~ gender * req + ( subj | product), data=qt, family=binomial)
summary(glmer2)
plot(allEffects(glmer2))

str(macbot)
str(luisbot)
names(qt)
ggplot(qt, aes(x=avgbot))+
  geom_density(x=macbot)+
  geom_density(x=luisbot)+
  theme_bw()

histbot1 <- 
ggplot(qt, aes(avgbot))+
  geom_density(aes(x=macbot), color='red', lty=2)+
  geom_density(aes(x=luisbot),color='blue', alpha=.2)+
  xlab("Balance of Thoughts")+
  theme_bw()
#ggsave(histbot1, filename = "histbot1.png", w=5, h=4, dpi=255)

??glmer

