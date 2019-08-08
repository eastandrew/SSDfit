library(tidyverse)
library(Hmisc)
library(MASS)

ssddata <- data.frame(stringsAsFactors=FALSE,
                      Scientific.name = c("Aedes aegypti", "Brachionus calyciflorus",
                                          "Chironomus tentans", "Danio rerio", "Daphnia magna",
                                          "Enallagma cyathigerum", "Lampsilis siliquoidea",
                                          "Lymnaea stagnalis", "Moina macrocopa", "Oryzias latipes",
                                          "Pimephales promelas", "Rana pipiens", "Xenopus laevis",
                                          "Xiphophorus helleri", "Daphnia magna", "Daphnia magna",
                                          "Daphnia magna", "Danio rerio", "Danio rerio", "Danio rerio"),
                      Duration = c("44-d", "28-d", "20-d", "70-180-d", "21-25-d", ">300-d",
                                   "7-d", "21-d", "7-d (chronic)", ">100-d, multi-gen",
                                   "21-d", "35-d", "4-mos", "42-d", "21-d", "21-d", "25-d",
                                   "70-d", "150-d", "180-d"),
                      Endpoint = c("Larval survival", "Intrinsic rate of increase",
                                   "Emergence", "Female size", "Reproduction",
                                   "Metamorphosis Success", "Glochidia metamorphosis viability",
                                   "Adult Survival", "Reproductive", "GSI, Number of eggs, Growth",
                                   "Cumulative Reproduction", "Time to metamorphosis, length",
                                   "Time to, size at -metamorphosis", "GSI", "Survival",
                                   "Reproduction", "Reproduction", "Female GSI and mass",
                                   "Male growth", "Female size"),
                      NOEC = c(50, 250, 2.3, 3.1, 404, 10, 4.5, 3000, 31.25, 10, 30,
                               1000, 1000, 100, 5300, 1250, 10, 10, 5, 0.6),
                      units = c("ug_l", "ug_l", "ug_l", "ug_l", "ug_l", "ug_l", "ug_l",
                                "ug_l", "ug_l", "ug_l", "ug_l", "ug_l", "ug_l", "ug_l",
                                "ug_l", "ug_l", "ug_l", "ug_l", "ug_l", "ug_l"),
                      idnum = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
                                17, 18, 19, 20)
)

ecdfobj <- Ecdf(ssddata$NOEC,what="F",pl=F)
ssddata <- arrange(ssddata,desc(NOEC))
ssddata$percorder2 <- rank(ssddata$NOEC)/max(ssddata$idnum)
ssddata
ssddata <- arrange(ssddata,desc(NOEC))
ssddata

plot(percorder2~NOEC, data=ssddata, log="x")

plot(density(ssddata$NOEC))
Ecdf(ssddata$NOEC,what="F",log="x")



fitdistr

fitdistr(ssddata$NOEC,densfun="lognormal")

fitobj <- MASS::fitdistr(ssddata$NOEC,densfun="lognormal")
fitobj
fitobj$estimate
fitobj$estimate[1]
fitobj$estimate[2]
fitobj$sd
fitobj$sd[1]
fitobj$sd[2]
meandist <- abs(rnorm(10,mean=fitobj$estimate[[1]],sd=fitobj$estimate[[2]]))
sddist <- abs(rnorm(10,mean=fitobj$sd[[1]],sd=fitobj$sd[[2]]))
plot(density(meandist))
plot(density(sddist))



vect <- c()
vect[1] <- min(ssddata$NOEC)
multiplier <- exp((log(max(ssddata$NOEC))-log(min(ssddata$NOEC)))/(10-1))
for (i in 2:(10)){
  vect[i] <- vect[i-1]*multiplier
}


qvec <- vect#ssddata$NOEC#seq(from=0.1, to=10000,length.out=100000)
lprob <- plnorm(qvec,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T)
#sprob <- plnorm(ssddata$NOEC,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T)

plot(lprob~qvec, type="l",log="x")
points(ssddata$percorder2~ssddata$NOEC,pch=16,col="red")
points(ssddata$percorder2~ssddata$NOEC,type="S",col="red")



qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T)

dftest <- data.frame(meandf=meandist,sddf=sddist)
dftest$estimate <- qlnorm(0.05,meanlog=dftest$meandf,sdlog=dftest$sddf)
dftest
plot(density(dftest$estimate,na.rm=T))
quantile(dftest$estimate,probs=c(0.01,0.025,0.05,0.25,0.5,0.75,0.95,0.975,0.99), na.rm=T)

plot(lprob~qvec, type="p",log="x", xlim=c(min(dftest$estimate),20000),pch=NA)
boxplot(dftest$estimate, at=0.05, horizontal=T, boxwex=0.1, add=T)
lines(lprob~qvec)
points(ssddata$percorder2~ssddata$NOEC,pch=16,col="red")



meandist2 <- abs(rnorm(10,mean=fitobj$estimate[1],sd=fitobj$estimate[[2]]))
sddist2 <- abs(rnorm(10,mean=fitobj$sd[[1]],sd=fitobj$sd[[2]]))
probvec <- c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99)#seq(from=0.01,to=0.99,length.out=100)#c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99)#seq(from=0.05,to=0.95,length.out=10)

dfbigtest <- expand.grid(mean=meandist2,sd=sddist2,prob=probvec)
head(dfbigtest,20)
str(dfbigtest)
dfbigtest$estimate <- qlnorm(dfbigtest$prob,meanlog=dfbigtest$mean,sdlog=dfbigtest$sd, lower.tail=T)



dfbig95 <- dfbigtest %>%
  group_by(prob) %>%
  summarise(lowCI = quantile(estimate, probs=0.025, na.rm=T, names=F),
            highCI = quantile(estimate, probs=0.975, na.rm=T, names=F))

plot(dfbigtest$prob~dfbigtest$estimate, log="x", pch="-",cex=0.5, ylim=c(0,1))
lines(prob~lowCI, data=dfbig95,lty=2, col="blue")
lines(prob~highCI, data=dfbig95,lty=2, col="blue")
lines(lprob~qvec, col="blue")
points(ssddata$percorder2~ssddata$NOEC,pch=16,col="red")
segments(0.001,0.05,qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0.05)
segments(qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0,qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T),0.05)
#Hmisc::Ecdf(ssddata$NOEC,what="F",add=T)


dfbig95$lowCI[dfbig95$prob==0.05]
qlnorm(0.05,meanlog=fitobj$estimate[[1]],sdlog=fitobj$estimate[[2]],lower.tail=T)
median(dfbigtest$estimate[dfbigtest$prob==0.05])
