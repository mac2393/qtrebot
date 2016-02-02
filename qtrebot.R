qtsot <- read.csv("RF_Coded_11.10.csv", header=TRUE)
options(digits=3)
###
library(lme4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(agricolae)
library(plyr)
library(effects)
##
names(qtsot)
attach(qtsot)
##
qtsot$re  <- as.factor (qtsot$re)
qtsot$req <- as.numeric(qtsot$req)
qtsot$dec <- as.factor (qtsot$dec)
##

t.test(macbot,luisbot) # good no difference

qtsot$product <- as.factor (qtsot$product)
qtsot$endow   <- as.factor (qtsot$path)
qtsot$macbot  <- as.numeric(qtsot$macbot)
qtsot$luisbot  <- as.numeric(qtsot$luisbot) #SUDY - adding luis BOT 
qtsot$avgbot  <- as.numeric(qtsot$avgbot)  #SUDY - adding combined BOT
qtsot$bot <- as.numeric(qtsot$bot)         #SUDY - adding original BOT
qtsot$mac     <- as.factor (qtsot$mac)
qtsot$luis    <- as.factor (qtsot$luis)
qtsot$macpp  <- as.numeric(qtsot$macpp) #SUDY - adding macpp
qtsot$luispp  <- as.numeric(qtsot$luispp) #SUDY - adding luispp
qtsot$avgpp  <- as.numeric(qtsot$avgpp) #SUDY - adding avgpp which is the averaged score of macpp and luispp
qtsot$macsmrd <- as.numeric(qtsot$macsmrd)
qtsot$avgsmrd <- as.numeric(qtsot$avgsmrd) #SUDY - adding combined SMRD
qtsot$smrd <- as.numeric(qtsot$smrd)       #SUDY - adding original SMRD
qtsot$gender  <- as.factor (qtsot$gender)
qtsot$age     <- as.factor (qtsot$age)
qtsot$aspect  <- as.numeric(qtsot$aspect)
qtsot$rmsot <- as.numeric(qtsot$rmsot) #adding standardized and averaged avgbot & avgsmrd

attach(qt) 
str(re)         # good
str(req)        # good
str(mac)        # good
str(macsmrd)    # good
str(macbot)     # good
str(avgsmrd)    # SUDY added avgsmrd
str(avgbot)     # SUDY added avgbot
str(smrd)       # SUDY added original smrd
str(bot)        # SUDY added original bot
str(avgpp)      # SUDY added avgpp
str(luisbot)    # SUDY added luisbot
str(age)        # not sure why age is 1,2,3,4,5 -- SUDY each number corresponds to a range
str(gender)     # want to rename these levels Male and Female
qtsot$gender  <- as.numeric (qtsot$gender) #SUDY remnamed gender levels 
qtsot$gender[qtsot$gender=="1"] <- "Male"     #SUDY remnamed gender levels
qtsot$gender[qtsot$gender=="2"] <- "Female"   #SUDY remnamed gender levels

str(product)    # want to rename these levels Shampoo and IC
qtsot$product <- as.factor(qtsot$product) 

#library(car)
#qtsot$product <- recode(qtsot$product,"NA=1")
#qtsot$product <- recode(qtsot$product,"B=2")
#qtsot$product <- recode(qtsot$product,"1=Shampoo")
#qtsot$product[qtsot$product=="1"] <- "Shampoo"
#qtsot$product[qtsot$product=="NA"] <- "A"
#qtsot$product[qtsot$product=="NA"] <- "B"

qtsot$dec  <- as.numeric (qtsot$dec) #SUDY remnamed decision 
qtsot$dec[qtsot$dec==1] <- "Take" #SUDY remnamed decision
qtsot$dec[qtsot$dec==2] <- "Left" #SUDY remnamed decision
summary(qtsot$dec)
str(qtsot$dec)

str(endow) # want to rename these levels to endowed and unendowed
qtsot$endow  <- as.numeric(qtsot$endow)  
qtsot$endow[qtsot$endow==0] <- "Endowed" 
qtsot$endow[qtsot$endow==NA] <- "0"
qtsot$endow <- recode(qtsot$endow, "NA=0")
str(qtsot$endow)

#qtsot$dec[qtsot$dec==0] <- "Left" #SUDY remnamed decision

str(macpp)      # want to rename these levels to prom and prev and ?
str(luispp)
str(avgpp)
str(aspect)     # looks good
levels(gender)   <- c("Male", "Female")         # need to check that this is the order
levels(dec)      <- c("Took", "Left")           # need to check that this is the order
levels(product)  <- c("Shampoo", "Ice Cream")   # need to check that this is the order
levels(endow)    <- c("endowed", "unendowed")   # need to check that this is the order
?levels

#Comparing histogram distributions of SMRD/BOT/SOT RF vs. QT
# scale the continuous variables: bot and smrd to make original SOT
#qtsot$req.z    <- scale(qtsot$req)
#qtsot$macbot.z <- scale(qtsot$macbot)  
#qtsot$luisbot.z<-scale(qtsot$luisbot)
qtsot$bot.z <- scale(qtsot$bot)
qtsot$smrd.z <- scale(qtsot$smrd)
qtsot$qtsot <- ((qtsot$bot.z + qtsot$smrd.z)/2) #creating Query Theory SOT score
qtsot$qtsot <- as.numeric(qtsot$qtsot)
#qtsot$avgpp.z <- scale(qtsot$avgpp)

cor(qtsot$avgsmrd,qtsot$smrd) #0.273
cor(qtsot$avgbot,qtsot$bot) #0.572
cor(qtsot$rmsot,qtsot$qtsot) #0.448

###Creating RE, URE dataframe
RE<-qt %>% filter(re == "1")
summary(RE) #avgbot = .02
sd(RE$avgbot) #2.34
mean(RE$avgsmrd) #avgsmrd = .121
sd(RE$avgsmrd) #avgsmrd = .83
mean(RE$rmsot) #-.135
sd(RE$rmsot) #.842
URE<-qt %>% filter(re == "0")
sd(URE$avgbot) #2.21
summary(URE) #avgbot=.84
mean(URE$avgsmrd) #avgsmrd = .27
sd(URE$avgsmrd) #avgsmrd = .84
mean(URE$sot) #.132
sd(URE$sot) #.822
t.test(RE$avgbot, URE$avgbot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.01
t.test(RE$avgsmrd, URE$avgsmrd, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.2
t.test(RE$sot, URE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.02

###Creating E and UE dataframe
E<-qt %>% filter(endow == "0")
mean(E$sot) #-0.0425
sd(E$sot) #0.843
UE<-qt %>% filter(endow == "1")
mean(UE$sot) #0.0453
sd(UE$sot) #0.84
boxplot(E$sot,UE$sot)
t.test(E$sot, UE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.5

###Creating Ice Cream and Shampoo dataframe
S<-qtsot %>% filter(product == "A")
IC<-qtsot %>% filter(product == "B")


#Overall Endowed and Unendowed RE
eRE<-RE %>% filter(endow == "0") #endowed RE
summary(eRE) #avgbot =-.41
sd(eRE$avgbot) #1.95
mean(eRE$avgsmrd) #-.009
sd(eRE$avgsmrd) #.836
mean(eRE$sot) #-.306
sd(eRE$sot) #.764
ueRE<-RE %>% filter(endow == "1") #unendowed RE
summary(ueRE) #avgbot =.5
sd(ueRE$avgbot) #2.64
mean(ueRE$avgsmrd) #.26
sd(ueRE$avgsmrd) #.82
mean(ueRE$sot) #.05
sd(ueRE$sot) #.89
t.test(eRE$avgbot, ueRE$avgbot, conf=0.95,var.equal=FALSE, paired=FALSE) #NS p=.06
t.test(eRE$avgsmrd, ueRE$avgsmrd, conf=0.95,var.equal=FALSE, paired=FALSE) #NS p=.1 CI(-0.6014, 0.0534)
t.test(eRE$sot, ueRE$sot, conf=0.95,var.equal=FALSE, paired=FALSE) #NS p=.1 CI(-0.6014, 0.0534)
chisq.test(eRE$avgbot, ueRE$avgbot) #Doesn't work?!
summary(aov(eRE$avgbot, ueRE$avgbot)) #Doesn't work?!
boxplot(eRE$sot, ueRE$sot)

#Overall Endowed and Unendowed URE
eURE<-URE %>% filter(endow == "0") #endowed URE
summary(eURE) #avgbot =.94
sd(eURE$avgbot) #2.07
mean(eURE$avgsmrd) #.38
sd(eURE$avgsmrd) #.84
mean(eURE$sot) #.221
sd(eURE$sot) #.843
ueURE<-URE %>% filter(endow == "1") #unendowed URE
summary(ueURE) #avgbot =.5
sd(ueURE$avgbot) #2.36
mean(ueURE$avgsmrd) #.15
sd(ueURE$avgsmrd) #.85
mean(ueURE$sot) #.037
sd(ueURE$sot) #.79
t.test(eURE$avgbot, ueURE$avgbot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.7
t.test(eURE$avgsmrd, ueURE$avgsmrd, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.2
t.test(eURE$sot, ueURE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.2
chisq.test(ueURE$avgbot, eURE$avgbot) #Doesn't work
boxplot(eRE$sot, ueRE$sot, eURE$sot, ueURE$sot, main="SOTs by Endowment and Eater Type",ylab ="SOT",las=1, at =c(1,2,3,4),names=c("Endowed RE","Unendowed RE","Endowed URE","Unendowed URE"))
mtext("Endowment & Eater Type", side = 1, line = 3)
par(cex.axis=.8)
par(cex.lab=.8)

#Creating IC Endowed and Unendowed RE
icERE<-eRE %>% filter(product == "B") #ic endowed RE
mean(icERE$avgbot) #avgbot =-.83
sd(icERE$avgbot) #2.23
mean(icERE$avgsmrd) #-.07
sd(icERE$avgsmrd) #.855
mean(icERE$sot) #-.438
summary(icERE$sot)
sd(icERE$sot) #.842
hist(icERE$sot)
icUERE<-ueRE %>% filter(product == "B") #ic unendowed RE
mean(icUERE$avgbot) #avgbot =.33
sd(icUERE$avgbot) #3.28
mean(icUERE$avgsmrd) #.38
sd(icUERE$avgsmrd) #.78
mean(icUERE$sot) #.0846
sd(icUERE$sot) #1.01
t.test(icERE$avgbot, icUERE$avgbot, conf=0.95, var.equal=FALSE, paired=FALSE) #NS?!p=.2
t.test(icERE$avgsmrd, icUERE$avgsmrd, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.05
t.test(icERE$sot, icUERE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.05
summary(aov(RE$avgbot,URE$avgbot))
chisq.test(icERE$avgbot, icUERE$avgbot) #Doesn't work?!
boxplot(icERE$avgbot, icUERE$avgbot)

#Creating IC endowed and unendowed URE
icEURE<-URE %>% filter(endow == "0" & product=="B") #ice cream endowed URE
mean(icEURE$avgbot) #1.25
sd(icEURE$avgbot) #sd=1.96
mean(icEURE$avgsmrd) #.63
sd(icEURE$avgsmrd) #.7
mean(icEURE$sot) #.433
sd(icEURE$sot) #.688
icUEURE<-URE %>% filter(endow == "1" & product=="B") #ice cream unendowed URE
mean(icUEURE$avgbot) #.3
sd(icUEURE$avgbot) #sd=2.02
mean(icUEURE$avgsmrd) #.06
sd(icUEURE$avgsmrd) #sd=.82
mean(icUEURE$sot) #-.107
sd(icUEURE$sot) #.718
t.test(icEURE$avgbot, icUEURE$avgbot, conf=0.95, var.equal=FALSE, paired=FALSE) #NS?!p=.2
t.test(icEURE$avgsmrd, icUEURE$avgsmrd, conf=0.95, var.equal=FALSE, paired=FALSE) #p.01*
t.test(icEURE$sot, icUEURE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p.008
chisq.test(icUEURE$avgbot, icEURE$avgbot) #Doesn't work?!
boxplot(icUEURE$avgbot, icEURE$avgbot)

boxplot(icERE$avgbot, icUERE$avgbot)


#Creating Shampoo Endowed and Unendowed RE
sERE<-eRE %>% filter(product == "A") #shampoo endowed RE
mean(sERE$avgbot) #avgbot =.03
sd(sERE$avgbot) #1.52
mean(sERE$avgsmrd) #.06
sd(sERE$avgsmrd) #.83
mean(sERE$sot) #-.168
sd(sERE$sot) #.662
sUERE<-ueRE %>% filter(product == "A") #shampoo unendowed RE
mean(sUERE$avgsmrd) #.15
sd(sUERE$avgsmrd) #.86
mean(sUERE$sot) #.0225
sd(sUERE$sot) #.781
t.test(sERE$avgbot, sUERE$avgbot, conf=0.95, var.equal=FALSE, paired=FALSE) #NS?!p=.2
t.test(sERE$avgsmrd, sUERE$avgsmrd, conf=0.95, var.equal=FALSE, paired=FALSE) #NS?!p=.7
t.test(sERE$sot, sUERE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.4
boxplot(sERE$avgbot, sUERE$avgbot)

#Creating Shampoo endowed and unendowed URE
sEURE<-URE %>% filter(endow == "0" & product=="A") #shampoo endowed URE
mean(sEURE$avgbot) #.65
sd(sEURE$avgbot) #sd=2.17
mean(sEURE$avgsmrd) #.14
sd(sEURE$avgsmrd) #.91
mean(sEURE$sot) #.015
sd(sEURE$sot) #.93
sUEURE<-URE %>% filter(endow == "1" & product=="A") #shampoo unendowed URE
mean(sUEURE$avgbot) #1.2
sd(sUEURE$avgbot) #sd=2.62
mean(sUEURE$avgsmrd) #.23
sd(sUEURE$avgsmrd) #.88
mean(sUEURE$sot) #.182
sd(sUEURE$sot) #.858
t.test(sEURE$avgbot, sUEURE$avgbot, conf=0.95,var.equal=FALSE, paired=FALSE) #NS?!p=.4
t.test(sEURE$avgsmrd, sUEURE$avgsmrd, conf=0.95,var.equal=FALSE, paired=FALSE) #NS p=.7
t.test(sEURE$sot, sUEURE$sot, conf=0.95,var.equal=FALSE, paired=FALSE) #NS p=.7
chisq.test(icUEURE$avgbot, icEURE$avgbot) #Doesn't work?!
boxplot(icUEURE$avgbot, icEURE$avgbot)

#Need a bargraph of Ice Cream SOT where y-axis show average SOT (with std dev lines) and x-axis is endowed RE, unendowed RE, etc
#Before I can do a good bargraph for this data, I need you to create a new variable.

qtsot$newvariable <- 1 # this just creates the column and sets them all to 1.
qtsot$newvariable[qtsot$re=="1" & qtsot$endow=="0"]<- "RE endowed" 
qtsot$newvariable[qtsot$re=="1" & qtsot$endow=="1"] <- "RE unendowed"
qtsot$newvariable[qtsot$re=="0" & qtsot$endow=="0"]    <- "URE endowed"
qtsot$newvariable[qtsot$re=="0" & qtsot$endow=="1"]  <- "URE unendowed"
qtsot$newvariable <- as.factor(qtsot$newvariable)
str(qtsot$newvariable)  # this should return “Factor w/ 4 levels: “uRE endowed”…

melted <- melt(qt, id.vars=c("rmsot", "newvariable"))
icSOT <- table(qtsot$sot, icUEURE$sot,icERE$sot, icUERE$sot) #Error: all arguments must have the same length

means <- ddply(melted, c("sot", "variable"), summarise,mean=mean(value))

ggplot(data=qt, aes(newvariable,rmsot)) + 
  geom_point() + 
  stat_smooth(method='glm', family='binomial') + 
  labs(title='SOT Scores of Ice Cream Decision') + 
  xlab('Eater Type x Endowment') + 
  ylab('SOT Scores')

barplot(qtsot$newvariable)
then we can put that in the X of a ggplot.

attach(qt)
# Tests
# ANOVAs used to test general differences between two or more means
?aov
summary(aov(macbot~path))     # NS 
summary(aov(avgbot~path))     #SUDY NS p=.2
summary(aov(macsmrd~path))    # NS
summary(aov(avgsmrd~path))    #SUDY NS
summary(aov(avgpp~path))      #SUDY NS
summary(aov(macbot~product))  # NS
summary(aov(avgbot~product))  #SUDY NS p=.24
summary(aov(macsmrd~product)) # NS
summary(aov(avgsmrd~product)) #SUDY NS
summary(aov(macbot~re))       # ** p=.002
summary(aov(avgbot~re))       # SUDY * p=.01 sig difference between BOT and eater type
summary(aov(macsmrd~re))      # NS
summary(aov(avgsmrd~re))      # SUDY NS
summary(aov(avgsmrd~dec))     # SUDY *** p=.04 sig diff btwn SMRD and decision
summary(aov(macbot~dec))      # ***
summary(aov(avgbot~dec))      # SUDY *** p=.000 sig diff btwn BOT and decision
summary(aov(macsmrd~dec))     # Trending p=.08
summary(aov(aspect~mac))      # NS
summary(aov(aspect~avgsmrd))  # SUDY NS
summary(aov(aspect~dec))      # NS
summary(aov(aspect~re))       # NS
summary(aov(aspect~product))  # NS
summary(aov(aspect~age))      # NS
summary(aov(aspect~gender))   # one of the genders write more/less than the other
summary(aov(avgpp~dec))       # SUDY *** sig diff btwn avgPP and decision
summary(aov(avgpp~re))        # SUDY ** p=.003
?lm

# Linear Regression
# lm(y,x)
summary(lm(macbot~req))       # ** p=.005 low Rsq
summary(lm(req~macbot))       # SUDY ** p=.005 low Rsq
summary(lm(avgbot~req))       # SUDY * p=.02 low Rsq
summary(lm(req~avgbot))       # SUDY * p=.02 low Rsq
req.lm=lm(req~avgbot)         # creating and saving linear regression model
confint(req.lm, level=0.95)   # CI -0.595 -0.0536
summary(lm(req~avgsmrd))      # SUDY NS p=.44
summary(lm(req~avgpp))        # SUDY *.013, low Rsq
summary(lm(avgpp~dec))        # SUDY *** p=.000
summary(lm(avgbot~dec))        # SUDY *** p=.000
plot(dec, avgbot, main="Product Decision and PPBOT",ylab="BOT Score",xlab="Decision")
req.lm=lm(avgbot~dec)         # creating and saving linear regression model
confint(req.lm, level=0.95)   # CI -0.595 -0.0536

summary(lm(avgpp~dec))        # SUDY *** p=.000
summary(lm(macbot.z~req.z))   # ** p=.005 low Rsq
summary(lm(avgbot.z~req.z))   # SUDY* p=.03 low Rsq
summary(lm(avgsmrd.z~req.z))  # SUDY NS
summary(lm(aspect~req))       # NS
summary(lm(macbot~aspect))    # NS
summary(lm(avgbot~aspect))    # SUDY ***
summary(lm(macpp~ aspect))    # NS
summary(lm(avgsmrd~ aspect))  # SUDY ***
summary(lm(req~aspect))       # NS

#Linear regression of Overall SOT on Dec 
# pretty sure this should be a glm because dec is binary
# and i think it needs to be a glmer becuase of the dataframe. 
qtsot$dec <- as.numeric(qtsot$dec)
summary(lm(qtsot$dec~qtsot$sot))
fit1 <- glm(qtsot$dec ~ qtsot$sot, data=qtsot)
summary(fit1)
confint(fit1, level=0.95)
ggplot(data=qtsot, aes(sot, dec)) + 
  geom_point() + 
  stat_smooth(method='glm', family='binomial') + 
  labs(title='Regression of SOT Score on Product Decision') + 
  xlab('SOT Score') + 
  ylab('Product Decision')


#Linear Regression of Ice Cream SOT on Dec
ggplot(data=IC, aes(sot, dec)) + 
  geom_point() + 
  stat_smooth(method='glm', family='binomial') + 
  labs(title='Regression of SOT Score on Ice Cream Decision') + 
  xlab('SOT Score') + 
  ylab('Ice Cream Decision')
summary(lm(IC$dec~IC$sot)) #***


# Chi Sq goodness of fit
chisq.test(dec, re)     # * makes sense that whether or not you're an RE will affect your decision
plot(dec, re) 
chisq.test(dec, req)     # p=.002 REQ affects decision
plot(dec, req) 
chisq.test(dec, mac)    # *** looks like there is a "fit" between prom prev and the decision! :)  
plot(dec, mac)          # need to figure out what to do about Unknown
chisq.test(dec, avgpp) #SUDY *** overall fit btwn avgpp and dec 
plot(dec, avgpp)
chisq.test(dec, avgbot) #SUDY p=.006
plot(dec, avgbot)
chisq.test(re, avgpp) #SUDY p=.01 overall fit btwn avgpp and re! 
plot(re, avgpp)
chisq.test(re, avgbot) #SUDY NS 
plot(re, avgbot, main="REQ and PPBOT",ylab="BOT Score",xlab="URE vs RE")
chisq.test(dec, macsmrd) # SUDY NS no relation between decision and SMRD
plot(dec, macsmrd)
chisq.test(dec, avgsmrd) #SUDY NS
plot(dec, avgsmrd)
chisq.test(dec, smrd)   #SUDY *** 
plot(dec, smrd)
chisq.test(endow, smrd) #SUDY NS
plot(endow, smrd)
chisq.test(endow, avgbot) #SUDY NS
plot(endow, avgbot, 
     main="Endowment and PPBOT",ylab="BOT Score",xlab="Endowment")
chisq.test(dec, endow)  # NS it is weird that this is NS.  Endowment effect predicts that they would take it more? or at least value it? 
plot(dec, endow)        # looks like the effect is there, it is just too minor to be detected by this chisq test
chisq.test(dec, gender) # NS good no gender effects
chisq.test(dec, age)    # NS why is age a factor? Cant it be numeric?

#Inter-rater Reliability
library(lpSolve)
library(vcd)
BOT <- qt[,7,23]
kappa2(BOT[,c(7,23)], "unweighted")
?kappa
cor(macbot,luisbot)

hist(avgsmrd, breaks=seq(10,35,1))
hist(smrd)
hist(avgbot)
hist(bot)
hist(macsmrd)
hist(datause$REQ, breaks=seq(10,35,1),
     main="Histogram of Restrained Eater Questionnaire Scores",ylab="Frequency",xlab="Scores",axes=FALSE)

#UEprodA<-unendowment %>% filter(Product == "A")

###Creating RE, URE dataframe
RE<-qt %>% filter(re == "1")
summary(RE) #avgbot = .02
sd(RE$avgbot) #2.34
mean(RE$avgsmrd) #avgsmrd = .121
sd(RE$avgsmrd) #avgsmrd = .83
mean(RE$sot) #-.135
sd(RE$sot) #.842
URE<-qt %>% filter(re == "0")
sd(URE$avgbot) #2.21
summary(URE) #avgbot=.84
mean(URE$avgsmrd) #avgsmrd = .27
sd(URE$avgsmrd) #avgsmrd = .84
mean(URE$sot) #.132
sd(URE$sot) #.822
t.test(RE$avgbot, URE$avgbot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.01
t.test(RE$avgsmrd, URE$avgsmrd, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.2
t.test(RE$sot, URE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.02
hist(URE$avgbot, main="Histogram of BOT URE",ylab="Frequency",xlab="Distribution of BOT of URE",axes=FALSE)
axis(side=1, at=seq(-8,8))
axis(side=2, at=seq(0,40))
?hist

###Creating E and UE dataframe
E<-qt %>% filter(endow == "0")
mean(E$sot) #-0.0425
sd(E$sot) #0.843
UE<-qt %>% filter(endow == "1")
mean(UE$sot) #0.0453
sd(UE$sot) #0.84
boxplot(E$sot,UE$sot)
t.test(E$sot, UE$sot, conf=0.95, var.equal=FALSE, paired=FALSE) #p=.5

###Creating Ice Cream and Shampoo dataframe
S  <-qtsot %>% filter(product == "A")
IC <-qtsot %>% filter(product == "B")

#Overall Endowed and Unendowed RE
eRE<-RE %>% filter(endow == "0") #endowed RE
summary(eRE) #avgbot =-.41
sd(eRE$avgbot) #1.95
mean(eRE$avgsmrd) #-.009
sd(eRE$avgsmrd) #.836
mean(eRE$sot) #-.306
sd(eRE$sot) #.764
ueRE<-RE %>% filter(endow == "1") #unendowed RE
summary(ueRE) #avgbot =.5
sd(ueRE$avgbot) #2.64
mean(ueRE$avgsmrd) #.26
sd(ueRE$avgsmrd) #.82
mean(ueRE$sot) #.05
sd(ueRE$sot) #.89
chisq.test(eRE$avgbot, ueRE$avgbot) #Doesn't work?!
summary(aov(eRE$avgbot, ueRE$avgbot)) #Doesn't work?!
boxplot(eRE$sot, ueRE$sot)

#Overall Endowed and Unendowed URE
eURE<-URE %>% filter(endow == "0") #endowed URE
summary(eURE) #avgbot =.94
sd(eURE$avgbot) #2.07
mean(eURE$avgsmrd) #.38
sd(eURE$avgsmrd) #.84
mean(eURE$sot) #.221
sd(eURE$sot) #.843
ueURE<-URE %>% filter(endow == "1") #unendowed URE
summary(ueURE) #avgbot =.5
sd(ueURE$avgbot) #2.36
mean(ueURE$avgsmrd) #.15
sd(ueURE$avgsmrd) #.85
mean(ueURE$sot) #.037
sd(ueURE$sot) #.79

#Creating IC Endowed and Unendowed RE
icERE<-eRE %>% filter(product == "B") #ic endowed RE
mean(icERE$avgbot) #avgbot =-.83
sd(icERE$avgbot) #2.23
mean(icERE$avgsmrd) #-.07
sd(icERE$avgsmrd) #.855
mean(icERE$sot) #-.438
summary(icERE$sot)
sd(icERE$sot) #.842
hist(icERE$sot)
icUERE<-ueRE %>% filter(product == "B") #ic unendowed RE
mean(icUERE$avgbot) #avgbot =.33
sd(icUERE$avgbot) #3.28
mean(icUERE$avgsmrd) #.38
sd(icUERE$avgsmrd) #.78
mean(icUERE$sot) #.0846
sd(icUERE$sot) #1.01

#Creating IC endowed and unendowed URE
icEURE<-URE %>% filter(endow == "0" & product=="B") #ice cream endowed URE
mean(icEURE$avgbot) #1.25
sd(icEURE$avgbot) #sd=1.96
mean(icEURE$avgsmrd) #.63
sd(icEURE$avgsmrd) #.7
mean(icEURE$sot) #.433
sd(icEURE$sot) #.688
icUEURE<-URE %>% filter(endow == "1" & product=="B") #ice cream unendowed URE
mean(icUEURE$avgbot) #.3
sd(icUEURE$avgbot) #sd=2.02
mean(icUEURE$avgsmrd) #.06
sd(icUEURE$avgsmrd) #sd=.82
mean(icUEURE$sot) #-.107
sd(icUEURE$sot) #.718

icSOT <- table(icEURE$sot, icUEURE$sot,icERE$sot, icUERE$sot) #Error: all arguments must have the same length

#Creating Shampoo Endowed and Unendowed RE
sERE<-eRE %>% filter(product == "A") #shampoo endowed RE
mean(sERE$avgbot) #avgbot =.03
sd(sERE$avgbot) #1.52
mean(sERE$avgsmrd) #.06
sd(sERE$avgsmrd) #.83
mean(sERE$sot) #-.168
sd(sERE$sot) #.662
sUERE<-ueRE %>% filter(product == "A") #shampoo unendowed RE
mean(sUERE$avgsmrd) #.15
sd(sUERE$avgsmrd) #.86
mean(sUERE$sot) #.0225
sd(sUERE$sot) #.781
boxplot(sERE$avgbot, sUERE$avgbot)

#Creating Shampoo endowed and unendowed URE
sEURE<-URE %>% filter(endow == "0" & product=="A") #shampoo endowed URE
mean(sEURE$avgbot) #.65
sd(sEURE$avgbot) #sd=2.17
mean(sEURE$avgsmrd) #.14
sd(sEURE$avgsmrd) #.91
mean(sEURE$sot) #.015
sd(sEURE$sot) #.93
sUEURE<-URE %>% filter(endow == "1" & product=="A") #shampoo unendowed URE
mean(sUEURE$avgbot) #1.2
sd(sUEURE$avgbot) #sd=2.62
mean(sUEURE$avgsmrd) #.23
sd(sUEURE$avgsmrd) #.88
mean(sUEURE$sot) #.182
sd(sUEURE$sot) #.858
