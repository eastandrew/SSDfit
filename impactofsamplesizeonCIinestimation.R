n <- 10
ave <- 10
disp <- 1

data <- data.frame(stringsAsFactors=FALSE,
                   NOEC=rlnorm(n,meanlog=ave,sdlog=disp),
                   idnum=seq(1:n)
)

data <- data[order(data$NOEC,decreasing=T), ]

data$percorder <- 1+(1/n)-order(data$NOEC, decreasing=T)/max(data$idnum)
fitobj <- MASS::fitdistr(data$NOEC,densfun="lognormal")
meandist2 <- rnorm(100,mean=fitobj$estimate[1],sd=fitobj$sd[[1]])
sddist2 <- rnorm(100,mean=fitobj$estimate[[2]],sd=fitobj$sd[[2]])
probvec <- c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99)
dfbigtest <- expand.grid(mean=meandist2,sd=sddist2,prob=probvec)
dfbigtest$estimate <- qlnorm(dfbigtest$prob,meanlog=dfbigtest$mean,sdlog=dfbigtest$sd, lower.tail=T)

library(drc)
simulate <- function(x) {
  n <- x
  ave <- 3.9094433
  disp <- 2.5785167
  
  data <- data.frame(stringsAsFactors=FALSE,
                     NOEC=rlnorm(n,meanlog=ave,sdlog=disp),
                     idnum=seq(1:n)
  )
  
  data <- data[order(data$NOEC,decreasing=T), ]
  
  data$percorder <- 1+(1/n)-order(data$NOEC, decreasing=T)/max(data$idnum)
  fitobj <- MASS::fitdistr(data$NOEC,densfun="lognormal")
  meandist2 <- rnorm(100,mean=fitobj$estimate[1],sd=fitobj$sd[[1]])
  sddist2 <- rnorm(100,mean=fitobj$estimate[[2]],sd=fitobj$sd[[2]])
  probvec <- c(0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.975,0.99)
  dfbigtest <- expand.grid(mean=meandist2,sd=sddist2,prob=probvec)
  dfbigtest$estimate <- qlnorm(dfbigtest$prob,meanlog=dfbigtest$mean,sdlog=dfbigtest$sd, lower.tail=T)
  quantile(dfbigtest$estimate[dfbigtest$prob==0.05], probs=0.05, na.rm=T, names=F)/median(dfbigtest$estimate[dfbigtest$prob==0.05])
  #quantile(dfbigtest$estimate[dfbigtest$prob==0.05], probs=0.05, na.rm=T, names=F)
  #median(dfbigtest$estimate[dfbigtest$prob==0.05])
  #sd(dfbigtest$estimate[dfbigtest$prob==0.05], na.rm=T)/median(dfbigtest$estimate[dfbigtest$prob==0.05],na.rm=T)
}

simulate(15)
simulate(17)

testdf <- data.frame(nsize=c(2:1000))
testdf$CIratio <- sapply(testdf$nsize,simulate)

plot(CIratio~nsize, data=testdf, log="x", xlim=c(2,1000), ylim=c(min(testdf$CIratio, na.rm=T),max(testdf$CIratio, na.rm=T)))

drm1 <- drm(CIratio~nsize, data=testdf, fct=LL.2())
plot(drm1)
plot(drm1, type="bars")
mselect(drm1, list(LL.4(),W1.4(),W2.4()))
ED(drm1, 0.5, type="absolute",interval="delta")
