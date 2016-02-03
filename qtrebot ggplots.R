library(ggplot2)

# these first 4 plots need to be ggsaved

img1 <- 
ggplot(qtsot, aes(x=avgbot.z, y=as.numeric(dec1)-1))+
  geom_point(color="gray3", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  facet_grid(~product)+
  theme_bw()+
  ggtitle("Regulatory Focus Affects \n Choice About Ice Cream")+
  xlab("Focus Predominance")+
  ylab("Decision \n Did not Take     -    Took it")
ggsave(img1, filename = "qtrebot1.png", w=6, h=4.5, dpi=255)

img2 <- 
ggplot(qtsubice, aes(x=avgbot.z, y=as.numeric(dec1)-1))+
  geom_point(color="gray3", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  facet_wrap(~path)+
  theme_bw()+
  ggtitle("Ice Cream \n Regulatory Focus Affects Choice \n Endowment Does Not")+
  xlab("Focus Predominance")+
  ylab("Decision \n Did not Take     -    Took it")
ggsave(img2, filename = "qtrebot2.png", w=6, h=4.5, dpi=255)

ggplot(qtsubshamp, aes(x=avgbot.z, y=as.numeric(dec1)-1))+
  geom_point(color="gray3", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  theme_bw()

ggplot(qtsot, aes(x=avgbot.z, y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  theme_bw()

ggplot(qtsot, aes(x=avgbot, colour=path,  y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  theme_bw()

ggplot(qtsot, aes(x=avgbot, y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  facet_grid(product~path)+
  theme_bw()

ggplot(qtsot, aes(x=avgbot, colour=as.factor(re), y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=F)+
  facet_grid(~product)+
  theme_bw()

ggplot(qtsubice, aes(x=avgbot, colour=as.factor(re), y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  theme_bw()

ggplot(qtsubshamp, aes(x=avgbot, colour=as.factor(re), y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=T)+
  theme_bw()

ggplot(qtsot, aes(x=avgbot, colour=as.factor(re), y=as.numeric(dec1)-1))+
  geom_point(color="black", position=position_jitter(h=.01, w=.29)) +
  stat_smooth(size=1.2, method="glm", family="binomial", se=F)+
  facet_grid(~product)+
  theme_bw()

ggplot(qtsot, aes(x=avgbot, y=req))+
  geom_point(position=position_jitter())+
  geom_smooth(method="lm")+
  facet_wrap(~product)+
  theme_bw()

##########
hist(qtsot$aspect, breaks=20)
range(qtsot$aspect) # 1 to 19

ggplot(qtsot, aes(as.factor(gender), aspect))+ geom_point(position=position_jitter(w=.01, h=.19))+
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', color="blue", width=.25, size=1)+
  stat_summary(fun.y=mean, geom='point', colour="red", size=2.5)+
  theme_bw()

ggplot(qtsot, aes(avgbot, aspect)) +  
  geom_smooth(method="loess", size=2)+
  geom_point(color=gender, position=position_jitter())+
  theme_bw()

grid.arrange(bot1, bot2, bot3, bot4, ncol=2, nrow=2)

## need to use dummy (0,1) coded variables in order to do these glm

ggplot(qtsot, aes(dec, avgbot))+ geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial")+
  facet_wrap(~re)+
  theme_bw()

ggplot(qtsot, aes(req, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial")+
  facet_wrap(~path)+
  theme_bw()

ggplot(data=qtsot, aes(newvariable,avgbot)) + 
  geom_point(position=position_jitter(w=.08, h=.08)) + 
  stat_smooth(method='glm', family='binomial') + 
  labs(title='Regression of SOT Score on Product Decision') + 
  xlab('Eater Type x Endowment') + 
  ylab('Mean SOT')

histbot1 <- 
  ggplot(qtsot, aes(avgbot))+
  geom_density(aes(x=macbot), color='red', lty=2)+
  geom_density(aes(x=luisbot),color='blue', alpha=.2)+
  xlab("Balance of Thoughts")+
  theme_bw()
#ggsave(histbot1, filename = "histbot1.png", w=5, h=4, dpi=255)

bot1 <- 
  ggplot(qtsot, aes(avgbot, dec1))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  # facet_wrap(~path)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance \n Affects Decision to Take Product")+
  theme_bw()
ggsave('bot1.png', w=5, h=4, dpi=255)

bot2 <- 
  ggplot(qtsot, aes(dec, avgbot))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  facet_wrap(~path)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance Affects\n Decision to Take Product by Product")+
  theme_bw()
ggsave('bot2.png', w=5, h=4, dpi=255)

bot3 <- 
  ggplot(qtsot, aes(avgbot, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  facet_wrap(~re)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance Affects\n Decision to Take Product by Eater Type")+
  theme_bw()
ggsave('bot3.png', w=5, h=4, dpi=255)

bot4 <- 
  ggplot(qtsot, aes(avgbot, dec))+ 
  geom_point(position=position_jitter(w=.19, h=.001))+
  geom_smooth(method="glm", family="binomial", size=2)+
  facet_wrap(~gender)+
  xlab("Prevention predominance -------- Promotion predominance \n Balance of Thoughts")+
  ylab("Decision")+
  ggtitle("Promotion Predominance Affects\n Decision to Take Product by Gender")+
  theme_bw()
ggsave('bot4.png', w=5, h=4, dpi=255)


hist(avgsmrd, breaks=seq(10,35,1))
hist(smrd)
hist(avgbot)
hist(bot)
hist(macsmrd)
hist(datause$REQ, breaks=seq(10,35,1),
     main="Histogram of Restrained Eater Questionnaire Scores",ylab="Frequency",xlab="Scores",axes=FALSE)

hist(qtsotsot$avgsmrd, main="Histogram of Regulatory Focus SMRD",xlab="SMRD")
hist(qtsotsot$smrd, main="Histogram of Query Theory SMRD",xlab="SMRD")
hist(qtsotsot$avgbot, main="Histogram of Regulatory Focus BOT",xlab="BOT", ylim=c(0,80))
axis(side=1, at=seq(-8,8))
axis(side=2, at=seq(0,0))
hist(qtsotsot$bot, main="Histogram of Query Theory BOT",xlab="BOT", ylim=c(0,80))
axis(side=1, at=seq(-8,8))
axis(side=2, at=seq(0,0))
hist(qtsotsot$rmsot, main="Histogram of Regulatory Focus SOT",xlab="SOT", xlim=c(-3,3), ylim=c(0,60))
hist(qtsotsot$qtsotsot, main="Histogram of Query Theory SOT",xlab="SOT", xlim=c(-3,3), ylim=c(0,60))

hist(URE$avgbot, main="Histogram of BOT URE",ylab="Frequency",xlab="Distribution of BOT of URE",axes=FALSE)
axis(side=1, at=seq(-8,8))
axis(side=2, at=seq(0,40))

boxplot(eRE$sot, ueRE$sot, eURE$sot, ueURE$sot, main="SOTs by Endowment and Eater Type",ylab ="SOT",las=1, at =c(1,2,3,4),names=c("Endowed RE","Unendowed RE","Endowed URE","Unendowed URE"))

mtext("Endowment & Eater Type", side = 1, line = 3)
par(cex.axis=.8)
par(cex.lab=.8)

ggplot(data=qtsotsot, aes(sot, dec)) + 
  geom_point() + 
  stat_smooth(method='glm', family='binomial') + 
  labs(title='Regression of SOT Score on Product Decision') + 
  xlab('SOT Score') + 
  ylab('Product Decision')


#Logistic Regression of Ice Cream SOT on Dec
ggplot(data=IC, aes(sot, dec)) + 
  geom_point() + 
  stat_smooth(method='glm', family='binomial') + 
  labs(title='Regression of SOT Score on Ice Cream Decision') + 
  xlab('SOT Score') + 
  ylab('Ice Cream Decision')
summary(lm(IC$dec~IC$sot)) #***