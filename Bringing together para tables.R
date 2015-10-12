library(data.table)
library(urca)

fulldataset = data.table(read.csv("mastertable.csv"))
set1 = fulldataset[750001:999911]
setkey(set1, X)
dataset0_460 = data.table(read.csv("mastertable_0_460.csv"))
set0_460 = dataset0_460[1:458911]
setkey(set0_460, X)

dataset501_422 = data.table(read.csv("mastertable_501_422.csv"))
set501_422 = dataset501_422[459001:500000]
setkey(set501_422, X)

dataset501_650 = data.table(read.csv("mastertable_501_650.csv"))
set501_650 = dataset501_650[500001:649911]
setkey(set501_650, X)


dataset651_750 = data.table(read.csv("mastertable_651_750.csv"))
set651_750 = dataset651_750[650001:749911]
setkey(set651_750, X)

dontable = rbind(set1, set0_460)
dontable = rbind(dontable, set501_422)
dontable = rbind(dontable, set501_650)
dontable = rbind(dontable, set651_750)
                 
dontable = dontable[order(X)]

write.csv(dontable, "finalbosstable.csv")

dontable[ DateCode == 500]
plot(dontable[ DateCode == 500]$CNHCoefm)

for (i in 1:911) {
  a <- CNHCoefmtable$mean[i]
  s <- CNHCoefmtable$sd[i]
  n <- 1000
  error <- qnorm(0.975)*s/sqrt(n)
  CNHCoefmtable$cilower[i] <- a-error
  CNHCoefmtable$ciupper[i] <- a+error
}
for (i in 1:911) {
  a <- CNYCoefmtable$mean[i]
  s <- CNYCoefmtable$sd[i]
  n <- 1000
  error <- qnorm(0.975)*s/sqrt(n)
  CNYCoefmtable$cilower[i] <- a-error
  CNYCoefmtable$ciupper[i] <- a+error
  }

for (i in 1:911) {
  a <- CnyTstatTable$mean[i]
  s <- CnyTstatTable$sd[i]
  n <- 1000
  error <- qnorm(0.975)*s/sqrt(n)
  CnyTstatTable$cilower[i] <- a-error
  CnyTstatTable$ciupper[i] <- a+error
  }

for (i in 1:911) {
  a <- CnhTstatTable$mean[i]
  s <- CnhTstatTable$sd[i]
  n <- 1000
  error <- qnorm(0.975)*s/sqrt(n)
  CnhTstatTable$cilower[i] <- a-error
  CnhTstatTable$ciupper[i] <- a+error
}

sd(dontable[ DateCode == 500]$CNHCoefm)

CNHCoefmtable = data.table(sd = 1, mean = 2, DateCode= 1:911, top = 0, bottom = 0, ciupper = 0, cilower = 0)
CNYCoefmtable = data.table(sd = 1, mean = 2, DateCode= 1:911, top = 0, bottom = 0, ciupper = 0, cilower = 0)
CnyTstatTable = data.table(sd = 1, mean = 2, DateCode= 1:911, top = 0, bottom = 0, ciupper = 0, cilower = 0)
CnhTstatTable = data.table(sd = 1, mean = 2, DateCode= 1:911, top = 0, bottom = 0, ciupper = 0, cilower = 0)


for (i in 1:911) {
  
  daydata = dontable[DateCode == i]
  
  CNHCoefmtable$sd[i] =  sd(daydata$CNHCoefm) 
  CNHCoefmtable$mean[i] =  mean(daydata$CNHCoefm)
  CNHCoefmtable$top[i] =  range(daydata$CNHCoefm)[2]
  CNHCoefmtable$bottom[i] =  range(daydata$CNHCoefm)[1]
  
  CNYCoefmtable$sd[i] =  sd(daydata$CNYCoefm) 
  CNYCoefmtable$mean[i] =  mean(daydata$CNYCoefm)
  CNYCoefmtable$top[i] =  range(daydata$CNYCoefm)[2]
  CNYCoefmtable$bottom[i] =  range(daydata$CNYCoefm)[1]
  
  CnhTstatTable$sd[i] =  sd(daydata$CnhTstat) 
  CnhTstatTable$mean[i] =  mean(daydata$CnhTstat)
  CnhTstatTable$top[i] =  range(daydata$CnhTstat)[2]
  CnhTstatTable$bottom[i] =  range(daydata$CnhTstat)[1]
  
  CnyTstatTable$sd[i] =  sd(daydata$CnyTstat) 
  CnyTstatTable$mean[i] =  mean(daydata$CnyTstat)
  CnyTstatTable$top[i] =  range(daydata$CnyTstat)[2]
  CnyTstatTable$bottom[i] =  range(daydata$CnyTstat)[1]
  }
for (i in 1:911)

  mastertable2$CNYCoefm[(p + g)] =  x$rlm$coefficients[1,2] #CNY coef
  mastertable2$CnhTstat[(p + g)] = x1$`Response CNH.d`$coefficients[1,3] #ecterm t stat for CNH
  mastertable2$CnyTstat[(p + g)] = x1$`Response CNY.d`$coefficients[1,3] #ec term t stat for CNY
  mastertable2$Date[(p + g)] = name
  mastertable2$RanNum[(p + g)] = i
  mastertable2$DateCode[(p + g)] = g
  mastertable2$EntryCode[(p + g)] = (p + g)
  print((p + g))}
CNYCoefmtable = data.table(sd = 1, mean = 2, DateCode= 1:911, high = 1, low = 1)
CnhTstatTable = data.table(sd = 1, mean = 2, DateCode= 1:911, high = 1, low = 1)
CnyTstatTable = data.table(sd = 1, mean = 2, DateCode= 1:911, high = 1, low = 1)
