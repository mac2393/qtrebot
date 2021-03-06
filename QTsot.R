qtsot <- read.csv("RF_Coded_11.10.csv", header=TRUE)
#qtsotraw <- read.csv("RF_Coded_11.10.csv", header=TRUE)

# need to make a df of advantages to take the product and another df of "disadvantes"

# then need to sum the Prom and Prev thoughts within adv and disadv

# also need to break apart the predominance score to see where the action is. 


options(digits=3)

library(reshape2)
library(dplyr)
library(ggplot2)
library(agricolae)
library(effects)

# changing some structure of variables
qtsot$re      <- as.factor (qtsot$re)
qtsot$req     <- as.numeric(qtsot$req)
qtsot$dec     <- as.factor (qtsot$dec)
qtsot$product <- as.factor (qtsot$product)
qtsot$endow   <- as.factor (qtsot$path)
qtsot$macbot  <- as.numeric(qtsot$macbot)
qtsot$luisbot <- as.numeric(qtsot$luisbot) #SUDY - adding luis BOT 
qtsot$avgbot  <- as.numeric(qtsot$avgbot)  #SUDY - adding combined BOT
qtsot$bot     <- as.numeric(qtsot$bot)     #SUDY - adding original BOT
qtsot$mac     <- as.numeric(qtsot$mac)
qtsot$luis    <- as.factor (qtsot$luis)
qtsot$macpp   <- as.numeric(qtsot$macpp)   #SUDY - adding macpp
qtsot$luispp  <- as.numeric(qtsot$luispp)  #SUDY - adding luispp
qtsot$avgpp   <- as.numeric(qtsot$avgpp)   #SUDY - adding avgpp which is the averaged score of macpp and luispp
qtsot$macsmrd <- as.numeric(qtsot$macsmrd)
qtsot$avgsmrd <- as.numeric(qtsot$avgsmrd) #SUDY - adding combined SMRD
qtsot$smrd    <- as.numeric(qtsot$smrd)       #SUDY - adding original SMRD
qtsot$gender  <- as.factor (qtsot$gender)
qtsot$age     <- as.factor (qtsot$age)
qtsot$aspect  <- as.numeric(qtsot$aspect)
qtsot$sot     <- as.numeric(qtsot$sot) #adding standardized and averaged avgbot & avgsmrd

attach(qtsot) 
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

#Need a bargraph of Ice Cream SOT where y-axis show average SOT (with std dev lines) and x-axis is endowed RE, unendowed RE, etc

qtsot$newvariable <- 1 # this just creates the column and sets them all to 1.
qtsot$newvariable[qtsot$re=="1" & qtsot$endow=="0"] <- "RE endowed" 
qtsot$newvariable[qtsot$re=="1" & qtsot$endow=="1"] <- "RE unendowed"
qtsot$newvariable[qtsot$re=="0" & qtsot$endow=="0"] <- "URE endowed"
qtsot$newvariable[qtsot$re=="0" & qtsot$endow=="1"] <- "URE unendowed"

qtsot$newvariable <- as.factor(qtsot$newvariable)
str(qtsot$newvariable)  # this should return “Factor w/ 4 levels: “uRE endowed”

str(macpp)      # want to rename these levels to prom and prev and ?
str(luispp)
str(avgpp)
str(aspect)     # looks good

###### naming levels of factor
str(qtsot$gender)
str(qtsot$path)
qtsot$path <- as.factor(qtsot$path)
str(qtsot$product)
str(qtsot$dec)

levels(qtsot$gender)   <- c("Male", "Female")         # need to check that this is the order
levels(qtsot$dec)      <- c("Left", "Took")           # need to check that this is the order
levels(qtsot$product)  <- c("Shampoo", "Ice Cream")   # need to check that this is the order
levels(qtsot$path)     <- c("endowed", "unendowed")   # need to check that this is the order
levels(qtsot$re)       <- c("Normal Eater", "Restrained Eater")

qtsot$dec1  <- qtsot$dec
levels(qtsot$dec1)  <- c(0,1)

# scale the continuous variables: macbot, req, avgbot, avgsmrd
qtsot$req.z     <- scale(qtsot$req)
qtsot$macbot.z  <- scale(qtsot$macbot)  
qtsot$luisbot.z <-scale(qtsot$luisbot)
qtsot$avgbot.z  <- scale(qtsot$avgbot)
qtsot$avgsmrd.z <- scale(qtsot$avgsmrd)
qtsot$avgpp.z   <- scale(qtsot$avgpp)

mean(qtsot$avgbot)
sd(qtsot$avgbot)
mean(qtsot$avgsmrd)
sd(qtsot$avgsmrd)
summary(qtsot$sot)

#### make subsets #####
qtsubice <- subset(qtsot, product=="Ice Cream")
qtsubshamp <- subset(qtsot, product=="Shampoo")

# I want for each participant, the avgbot for A only. 

# trying to replace that mediation analysis with the right stat test. 

plot(qtsubice$path, qtsubice$dec)
plot(qtsubice$re, qtsubice$dec)
plot(qtsubice$req, qtsubice$dec)
plot(qtsubice$req.z, qtsubice$dec)

attach(qtsubice)
re
qtsubicere1 <- subset(qtsubice, qtsubice$re=="Restrained Eater")
attach(qtsubicere1)
summary(dec)
summary(path)

matrix1 <- matrix(c(58,49,53,49),byrow=TRUE,nrow=2)
#dimnames(matrix1) <- list(path==c("endowed","unendowed"), dec==c("Left","Took"))
matrix1
chisq.test(matrix1)
chisq.test(matrix1,simulate.p.value=TRUE,B=10000)
# this might be telling us with great certainty that 
# there is no effect of endowment on the decision to take the IC or not
# lets check it for REs
matrix2 <- matrix(c(35, 16, 27,24), byrow=TRUE, nrow=2)
matrix2

chisq.test(matrix2)
chisq.test(matrix2, simulate.p.value=TRUE, B=10000)
plot(matrix1, ylim=c(0,100))
plot(matrix2, ylim=c(0,100))
