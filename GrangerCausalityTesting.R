
library(fUnitRoots) 
library(urca)
library(vars)
library(aod)
library(zoo)
library(tseries)
library(data.table)


Data = read.csv("gretl for granger in R.csv")

data = (as.data.table(Data))
data[,2:3]
data[,obs:=(as.POSIXct(data$obs))]

format.POSIXct(Data$date, "%d/%m/%Y")

plot(Data$dateF[700:1011],Data$Mid_CNH[700:1011],type="l",col="black",lwd=2)
lines(Data$dateF[700:1011],Data$Mid_CNY[700:1011],col="blue",lty=2,lwd=1)
lines(Data$dateF[700:1011],Data$CPR[700:1011],col="red",lty=2,lwd=1)
legend("topleft",c("CNY","CNH", "CPR"),col=c("black","blue","red"),lty=c(1,2),lwd=c(2,1),bty="n")
#end of 2013 break?
#Test for unit roots
adf.test(data$Mid_CNY)
adf.test(data$Mid_CNH)
kpss.test(data$Mid_CNY)
kpss.test(data$Mid_CNY)

adf.test(diff(data$Mid_CNY,1))
adf.test(diff(data$Mid_CNH,1))
kpss.test(diff(data$Mid_CNY,1))
kpss.test(diff(data$Mid_CNH,1))

#Set up VAR-Model
#select lag order // either 2 or 6
VARselect(cnhcny,lag=5,type="const")

V.1<-VAR(cnhcny[0:157],p=6,type="const")
V.2<-VAR(cnhcny[158:292],p=6,type="const")
V.3<-VAR(cnhcny[293:621],p=6,type="const")
V.4<-VAR(cnhcny[622:751],p=6,type="const")
V.5<-VAR(cnhcny[752:1011],p=6,type="const")
V.6<-VAR(cnhcny,p=6,type="const")

#VAR
V.5<-VAR(data[,14:15],p=5,type="both")
serial.test(V.2)

# 0-157
#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.1$varresult[[1]]), Sigma=vcov(V.1$varresult[[1]]), Terms=c(2,4,6,8,10))
#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.1$varresult[[2]]), Sigma=vcov(V.1$varresult[[2]]), Terms= c(1,3,5,7,9))

# 158-292
#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.2$varresult[[1]]), Sigma=vcov(V.2$varresult[[1]]), Terms=c(2,4,6,8,10))
#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.2$varresult[[2]]), Sigma=vcov(V.2$varresult[[2]]), Terms= c(1,3,5,7,9))

# 293-621
#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.3$varresult[[1]]), Sigma=vcov(V.3$varresult[[1]]), Terms=c(2,4,6,8,10))
#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.3$varresult[[2]]), Sigma=vcov(V.3$varresult[[2]]), Terms= c(1,3,5,7,9))

# 622-751
#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.4$varresult[[1]]), Sigma=vcov(V.4$varresult[[1]]), Terms=c(2,4,6,8,10))
#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.4$varresult[[2]]), Sigma=vcov(V.4$varresult[[2]]), Terms= c(1,3,5,7,9))

# 752-1011
#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.5$varresult[[1]]), Sigma=vcov(V.5$varresult[[1]]), Terms=c(2,4,6,8,10))
#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.5$varresult[[2]]), Sigma=vcov(V.5$varresult[[2]]), Terms= c(1,3,5,7,9))

# all
#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.6$varresult[[1]]), Sigma=vcov(V.6$varresult[[1]]), Terms=c(2,4,6,8,10))
#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.6$varresult[[2]]), Sigma=vcov(V.6$varresult[[2]]), Terms= c(1,3,5,7,9))




#Wald-test (H0: CNY does not Granger-cause CNH)
wald.test(b=coef(V.6$varresult[[1]]), Sigma=vcov(V.6$varresult[[1]]), Terms=c(2,4,6,8,10))
#X2 = 61.5, df = 6, P(> X2) = 2.3e-11

#Wald.test (H0: CNH does not Granger-cause CNY)
wald.test(b=coef(V.2$varresult[[2]]), Sigma=vcov(V.2$varresult[[2]]), Terms= c(1,3,5,7,9))
#X2 = 222.8, df = 6, P(> X2) = 0.0


   
