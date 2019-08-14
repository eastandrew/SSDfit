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
  #quantile(dfbigtest$estimate[dfbigtest$prob==0.05], probs=0.05, na.rm=T, names=F)/median(dfbigtest$estimate[dfbigtest$prob==0.05])
  #quantile(dfbigtest$estimate[dfbigtest$prob==0.05], probs=0.05, na.rm=T, names=F)
  #median(dfbigtest$estimate[dfbigtest$prob==0.05])
  #sd(dfbigtest$estimate[dfbigtest$prob==0.05], na.rm=T)/median(dfbigtest$estimate[dfbigtest$prob==0.05],na.rm=T)
  IQR(dfbigtest$estimate[dfbigtest$prob==0.05],  na.rm=T)
}

simulate(15)
simulate(17)
simulate(15:17)

sim2 <- Vectorize(simulate)
sim2(15)
sim2(15:17)

testdf <- data.frame(nsize=c(2:1000))
testdf$CIratio <- sim2(testdf$nsize)#sapply(testdf$nsize,simulate)

plot(CIratio~nsize, data=testdf, log="xy", xlim=c(2,100), ylim=c(min(testdf$CIratio, na.rm=T),max(testdf$CIratio, na.rm=T)))

m <- matrix(data=NA, nrow=100, ncol=10)
for (i in 2:100) {
  for (j in 1:10) {
    m[i,j] <- Vectorize(simulate(i))
  }
}
m
rownames(m) <- 1:100
m
m <- data.frame(m)
m
m$sampsize <- as.numeric(rownames(m))
m
m <- gather(m, key=runnumber, value=value, -sampsize)
m
plot(value~sampsize, data=m, log="xy")
m2 <- m %>%
  group_by(sampsize) %>%
  summarise(medianv = median(value, na.rm=T),
            meanv = mean(value, na.rm=T)) %>%
  arrange(sampsize)
m2
plot(meanv~sampsize, data=m2, type="b", log="")
plot(density(m$value[m$sampsize==10]))

mfit <- nls(value~b+max(value,na.rm=T)*exp(-a*sampsize), data=m, start=list(a=1,b=1))
mfit
mobj <- summary(mfit)
mobj$coefficients[1,1]
mobj$coefficients[2,1]
m$predict <- mobj$coefficients[1,1]+max(m$value, na.rm=T)*exp(-mobj$coefficients[2,1]*m$sampsize)
head(m, 20)


mfit2 <- nls(meanv~b+max(meanv,na.rm=T)*exp(-a*sampsize), data=m2, start=list(a=1, b=1))
mfit2
mobj2 <- summary(mfit2)
mobj2$coefficients[1,1]
mobj2$coefficients[2,1]
m2$predict <- mobj2$coefficients[1,1]+max(m2$meanv, na.rm=T)*exp(-0.284*m2$sampsize)
#m2$predict2 <- predict(mfit2, sampesize=m2$sampsize)
head(m2, 20)





plot(value~sampsize, data=m, log="", xlim=c(1,100), ylim=c(0.1,20))
curve(mobj$coefficients[1,1]+max(m$value, na.rm=T)*exp(-mobj$coefficients[2,1]*x), from=2, to=100, n=99, add=T)
points(predict~sampsize, data=m, type="l")

plot(value~sampsize, data=m, log="y")
curve(mobj$coefficients[1,1]+max(m$value, na.rm=T)*exp(-mobj$coefficients[2,1]*x), from=2, to=100, n=99, add=T)
points(predict~sampsize, data=m, type="l")


plot(meanv~sampsize, data=m2,  log="")
points(predict~sampsize, data=m2, type="l")
curve(mobj2$coefficients[1,1]+max(m$value, na.rm=T)*exp(-mobj2$coefficients[2,1]*x), from=2, to=100, n=99, add=T)

plot(meanv~sampsize, data=m2,  log="", xlim=c(0,30),ylim=c(0,15))
points(predict~sampsize, data=m2, type="l")
curve(mobj2$coefficients[1,1]+max(m2$meanv, na.rm=T)*exp(-mobj2$coefficients[2,1]*x), from=2, to=100, n=99, add=T)

