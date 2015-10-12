library(tsDyn)
library(dplyr)
library(data.table)
library(urca)

setwd("/Users/christophersheehan/Desktop/Working Directory R")
CNH = read.csv("CNH.csv")
CNY = read.csv("CNY.csv")
CNY = as.data.table(CNY)
CNH = as.data.table(CNH)
CNH[,midPrice:=(Bid.Price + Ask.Price)/2]
CNY[,midPrice:=(Bid.Price + Ask.Price)/2]
levels(CNY[,Date.G.])
CNHts = select(CNH, Date, Time, midPrice)
CNYts = select(CNY, Date.G., Time.G., midPrice)


setkey(CNHts, Date)
setkey(CNYts, Date.G.)
x = cbind(as.vector(select(CNHts["01/06/2012"], midPrice)[1:200]), as.vector(select(CNYts["01/06/2012"], midPrice)[1:200]))

setkey(CNHts, Date)
setkey(CNYts, Date)

[1] 1009
for (i in datelist) {
  tablea = CNHts[i]$midPrice
  CNHfinaltabe$ = mean(tablea)
  
}
data.table(CNHFrq = 1:1009, CNYFrq = 1:1009, Date = datelist)


#first needs something to find out number in each day

CNHFrq = as.list(NULL)
for (i in datelist) {
 CNHFrq[i] = length(CNHts[i]$Time)
}
CNYFrq = as.list(NULL)
for (i in datelist) {
  CNYFrq[i] = length(CNYts[i]$Time)
}
PerDay = data.table(CNYFrq, CNHFrq, datelist)
set.seed(1)
CNHts = as.data.table(CNHts)
x = cbind(as.vector(select(CNHts["01/06/2012"], midPrice)[1:200]), as.vector(select(CNYts["01/06/2012"], midPrice)[1:200])) #DINGDINGDING
ranCny1 = NULL
CnyRanList = data.table(1:1009)
t = 0
for (g in 1:1000) {
for (i in PerDay$CNYFrq)  { 
  t = t + 1
  ranCny1[t]= ceiling(runif(1, 0, i))}
CnyRanList[,pick[g]:=ranCny1]}

lapply(1:1000)

county <- function(arg1, arg2){
  t = 0
  ranCny1 = NULL
  for (i in arg1)  { 
    t = t + 1
    ranCny1[t]= ceiling(runif(1, 0, i))}
  object = 1:arg1
  return(object)
} 
deta = data.table(1:1009)
county <- function(arg1, arg2){
  object = ceiling(runif(1, 0, arg1))
  return(object)
} 
CnyRanList = list()
for (i in 1:1000) {
  CnyRanList[[i]] = lapply(PerDay$CNYFrq, county)
}#DINGDINGDING
CnhRanList = list()
for (i in 1:1000) {
  CnhRanList[[i]] = lapply(PerDay$CNHFrq, county)
}#DINGDINGDING
CnyRanList = list()
for (i in 1:1000) {
  CnyRanList[[i]] = lapply(PerDay$CNYFrq, county)
}#DINGDINGDING




x = cbind(as.vector(select(CNHts["01/06/2012"], midPrice)[1:200]), as.vector(select(CNYts["01/06/2012"], CNYMidPrice)[1:200])) #DINGDINGDING
list[k][t] = select(CNHts[g], midPrice)[i] 
#where t is 1:1009, k is 1:1000, g is list of dates and i is the random number
select(CNHts["01/06/2012"], midPrice)[50]
list[k][t] = select(CNHts[g], midPrice)[i] 
length(CNHts["01/06/2012"]$Time)
#Here is the loop building for actual test
listA = rep( list(list(1009)), 1009) 
setkey(CNHts, Date)
randtry <- function(k, t) {
  i = CnhRanList[k][t]
  for (g in datelist) {
  for (i in CnhRanList) {
  print(select(CNHts[g], midPrice)[i])}}
  listA[[k]][[t]] = select(CNHts[g], midPrice)[4]}
}

randtry <- function(k, t) {
  i = CnhRanList[k][t]
  for (g in datelist) {
    for (i in CnhRanList) {
      print(select(CNHts[g], midPrice)[i])}}
  listA[[k]][[t]] = select(CNHts[g], midPrice)[4]}
}

randtry <- function(k, t) {
  i = CnhRanList[k][t]
  for (g in datelist) {
    for (i in CnhRanList and g in datelist) {
      print(select(CNHts[g], midPrice)[i])}}
  listA[[k]][[t]] = select(CNHts[g], midPrice)[4]}
}
Data = read.csv("gretl for granger in R.csv")
l = matrix(nrow = 1009, ncol = 1000)
for (i in datelist) {
  g == 
  for (g in CnhRanList[i]) print(g)}
    l[i,g] = CnhRanList
    l = matrix(nrow = 1009, ncol = 1000)
}

for (i in 1:1000) {
  for (g in 1:1009) {
    dat = datelist[g]
    rn = CnhRanList[[i]][[g]]
    listA[[i]][[g]] = CNHts[dat]$midPrice[rn]
    } #GOTIT
}
listCNY = rep( list(vector(mode = "list", length = 1009)), 1000)
for (i in 1:1000) {
  for (g in 1:1009) {
    dat = datelist[g]
    rn = CnyRanList[[i]][[g]]
    listCNY[[i]][[g]] = CNYts[dat]$midPrice[rn]
  }} #GOTIT
CnyRanList
RanLevels1 = data.table(CNH = 1:1009, CNY = 1:1009, Date = 1:1009)
RanLevels1[,CNH:=listA[[1]]]
RanLevels1[,CNY:=listCNY[[1]]]
RanLevels1[,Date:=datelist]

RanLevels2 = data.table(CNH = 1:1009, CNY = 1:1009, Date = 1:1009)
RanLevels2[,CNH:=listA[[2]]]
RanLevels2[,CNY:=listCNY[[2]]]
RanLevels2[,Date:=datelist]

cbind(CNH = listA[[1]], CNY = listCNY[[1]])
for (i in 1:1000) {
  nam = paste('RanSeries', i, sep = "_")
  assign(nam, cbind(CNH = listA[[i]], CNY = listCNY[[i]]))
}

for (i in 1:1000) {
  nam1 = paste('CNH', i, sep = "_")
  RanLevels1[,nam1 := listA[[i]]]
  nam1 = paste('CNY', i, sep = "_")
  RanLevels1[, nam2 := listCNY[[i]]]
}

for (i in 1:10) {
  nam = paste('RanSeries', i, sep = "_")
  nam1 = paste('V', i)
  test[,t:=unlist(nam)]
}

for (i in 1:10) {
  nam = paste('RanSeries', i, sep = "_")
  nam1= paste(nam, '[,1]', sep = "")
  print(nam1)
  print(unlist(nam1))
  nam1 = paste('V', i)
  }

data.frame(RanSeries_1, date = datelist)
for (i in 1:1000) {
  nam = paste('RanSeries', i, sep = "_")
  data_frame(nam, date = datelist)
}
listA = rep( list(vector(mode = "list", length = 1009)), 1000)
vector(mode = "list", length = 1009)
for (i in 1:1000) {
  for (g in 1:1009) {
    dat = datelist[g]
    rn = CnhRanList[[i]][[g]]
    print(CNHts[dat]$midPrice[rn])
    
  }
}
CNHts[dat]$midPrice[[rn]]
unlist()
na.omit(data.table(unlist(RanSeries_1[,1]), unlist(RanSeries_1[,2]), datelist))
RanLevels1[,.(CNH,CNY)]

for (i in 1:1000) {
  
  RanLevels1[,]
}
data = (as.data.table(Data))
data[,2:3]
data[,obs:=(as.POSIXct(data$obs))]

data[,obs:=(format.POSIXct(data$obs, "%d/%m/%Y"))]

datelist = data$obs

CNHts[,Date.G.:=strptime(as.list(levels(CNYts$Date.G.)), "%d/%m/%Y"))]





x = cbind(as.vector(select(CNHts["01/06/2012"], CNHMidPrice)[1:200]), as.vector(select(CNYts["01/06/2012"], CNYMidPrice)[1:200]))

alphaols(ca.jo(x, ecdet = "const"))
spec="longrun"
spec="transitory"
summary(ca.jo(x, ecdet = "const", spec="longrun"))
summary(alphaols(ca.jo(x, ecdet = "const", spec="transitory")))


myvecmNC1 <- ca.jo(RanSeries_1, type="eigen", K=5, spec="longrun")

cajorls(myvecmNC) #replciatedd
y = cajorls(myvecm)
x
alphaols(myvecmNC)

as.table(CNY[,Date.G.])
levels(CNY[,Date.G.])

as.vector((strptime(as.list(levels(CNY$Date.G.)), "%d/%m/%Y")))
as.vector((strptime(as.list(levels(CNH$Date)), "%d/%m/%Y")))
cbind(as.vector((strptime(as.list(levels(CNY$Date.G.)), "%d/%m/%Y"))), as.vector((strptime(as.list(levels(CNH$Date)), "%d/%m/%Y"))))
dC[,CNYdate:=as.vector((strptime(as.list(levels(CNY$Date.G.)), "%d/%m/%Y")))]
#length of cny 1527895 levels 1127
length(levels(CNH$Date))
length(levels(CNH$Date))
#levels of cnh date
# 1107

sort((strptime(as.list(levels(CNY$Date.G.)), "%d/%m/%Y")))
write.csv(sort((strptime(as.list(levels(CNY$Date.G.)), "%d/%m/%Y"))), "CNYdates.csv")
write.csv(sort((strptime(as.list(levels(CNH$Date)), "%d/%m/%Y"))), "CNHdates.csv")