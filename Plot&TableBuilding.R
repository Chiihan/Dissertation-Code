library(plotrix)
library(data.table)
library("reshape2")
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(dplyr)

IRates = data.table(read.csv("IRP.csv"))
IRates[,DATE:=as.factor(as.IDate(strptime(IRates$Date, format = "%d/%m/%y")))]
setkey(IRates, DATE)
merge(TruePlot, IRates, all.x = TRUE)
TruePlot[,Date:=NULL]
ggplot(data=TruePlot) +
  +     geom_line(aes(x = DATE, y =Mid_CNH), color="red") +
  +     theme(title="Twitter", axis.title.x = theme_grey())

x = data.table(read.csv("outo.csv"))
TruePlot = x
TruePlot[,DATE:=TruePlot$dateF]
setkey(JP, DATE)
setkey(TruePlot, DATE)
JP = data.table(read.csv("JP.csv"))
TruePlot = merge(TruePlot, JP, all.x = TRUE)
CPRs = data.table(read.csv("CPRs.csv"))
976
DATE = double(length = 1033)Tru
DATE[1:975] = (strptime(CPRs$Date[1:975], format = "%d/%m/%y"))
DATE[976:1033] = (strptime(CPRs$Date[976:1033], format = "%d %b %Y"))
as.vector(str(strptime(CPRs$Date[1:975], format = "%d/%m/%y")))
CPRs[,DATE2:=as.factor(as.POSIXlt("2011-02-01"))]
CPRs$datep[1:975] = as.character(strptime(CPRs$Date[1:975], format = "%d/%m/%y"))
CPRs$datep[976:1033] = as.character(strptime(CPRs$Date[976:1033], format = "%d %b %Y"))
CPRs$datep = unlist(CPRs$datep)
SimpleCPR = data.table(CPR = CPRs[, USD], DATE = CPRs[, datep])
setkey(SimpleCPR, DATE)
TruePlot = merge(TruePlot, SimpleCPR, all.x = TRUE)
> TruePlot1 = TruePlot
SimpleCPR$DATE1 = as.factor(SimpleCPR$DATE)
SimpleCPR$DATE = SimpleCPR$DATE2
SimpleCPR$DATE = SimpleCPR$DATE1
setkey(SimpleCPR, DATE)
CNHperyear = x[,.(Mid_CNH.Sum = sum(VOL_CNH)),by=YEAR]
CNYperyear = x[,.(Mid_CNY.Sum = sum(VOL_CNY)),by=YEAR]
TruePlot[,CNHspread:=(TruePlot$ASK_CNH-TruePlot$BID_CNH)]
TruePlot[,CNYspread:=(TruePlot$ASK_CNY-TruePlot$BID_CNY)]
CNpy = merge(CNHperyear, CNYperyear, by = 'YEAR')
spreadmelt <- melt(TruePlot[,.(CNYspread, CNHspread, dateF)], id="dateF")
CPRmelt = melt(TruePlot[,.(Mid_CNH, Mid_CNY, CPR, dateF)], id="dateF")
ggplot(data=spreadmelt, aes(x=dateF, y=value, group=variable, colour=variable)) +
  xlab("Date") + ylab("Bid to Ask Spread") +
  geom_line(aes(linetype=variable), size=0.5) +
  theme_bw() +
  scale_x_date(breaks = "1 year", minor_breaks = "6 month") +
  theme(legend.position=c(0.9, .7))#CNH to CNY
CPRmelt$dateF = as.Date(CPRmelt$dateF)
ggplot(data=CPRmelt, aes(x=dateF, y=value, group=variable, colour=variable)) +
  xlab("Date") + ylab("Bid to Ask Spread") +
  geom_line(aes(linetype=variable), size=0.5) +
  theme_bw() +
  scale_x_date(breaks = "1 year", minor_breaks = "6 month") +
  theme(legend.position=c(0.9, .7))#CNH to CNY
SSE = data.table(read.csv("ShanghaiSE.csv"))
NYS = data.table(read.csv("NY stock index.csv"))
NYS[,DATE:=as.IDate(strptime(Date, "%e/%m/%y"))]
SSE[,DATE:=as.IDate(strptime(Date, "%e/%m/%y"))]
SSE[,SS:=Close]
NYS[,NY:=Close]
setkey(SSE, DATE)
setkey(NYS, DATE)
StoInds = merge(SSE[,.(DATE, SS)], NYS[,.(DATE, NY)])
test_data_long <- melt(x[,.(Mid_CNH, Mid_CNY, dateF)], id="dateF")
NYS[,DATE:=as.factor(DATE)]
TruePlot = merge(TruePlot, NYS[,.(DATE, NY)], all.x = TRUE)
SSE[,DATE:=as.factor(DATE)]
TruePlot = merge(TruePlot, SSE[,.(DATE, SS)], all.x = TRUE)
levels(test_data_long$variable) = c("CNH", "CNY")
test_data_long$dateF = as.Date(test_data_long$dateF)
test_data_long$Currency = test_data_long$variable
Allmelt = melt(TruePlot[,.(Mid_CNH, JPMOR, SS, dateF)], id="dateF")
Allmelt$dateF = as.Date(Allmelt$dateF)
ggplot(data=Allmelt, aes(x=dateF, y=value, group=variable, colour=variable)) +
  xlab("Date") + ylab("Bid to Ask Spread") +
  geom_line(aes(linetype=variable), size=0.5) +
  theme_bw() +
  scale_x_date(breaks = "1 year", minor_breaks = "6 month") +
  theme(legend.position=c(0.9, .7))#CNH to CNY

ggplot(data=CPRmelt, aes(x=dateF, y=value, group=variable, colour=variable)) +
  xlab("Date") + ylab("Bid to Ask Spread") +
  geom_line(aes(linetype=variable), size=0.5) +
  theme_bw() +
  scale_x_date(breaks = "1 year", minor_breaks = "6 month") +
  theme(legend.position=c(0.9, .7))#CNH to CNY
ggplot(data=test_data_long, aes(x=dateF, y=value, group=Currency, colour=variable)) +
  xlab("Date") + ylab("USD Exchange Rate") +
  geom_line(aes(linetype=variable), size=0.5) +
  scale_x_date(breaks = "1 year", minor_breaks = "6 month") +
  theme_bw() +
  theme(legend.position=c(0.925, .7))#CNH to CNY

ggplot(data=TruePlot, aes(x=dateF, y=value, group=Currency, colour=variable)) +
  xlab("Date") + ylab("USD Exchange Rate") +
  geom_line(aes(linetype=variable), size=0.5) +
  scale_x_date(breaks = "1 year", minor_breaks = "6 month") +
  theme_bw() +
  theme(legend.position=c(0.925, .7))#CNH to CNY

barplot(t(as.matrix(CNpy[,.(Mid_CNH.Sum, Mid_CNY.Sum)])), main="Trades Per Year*",
        xlab="Year", col=c("darkblue","red"), beside=TRUE, 
        names.arg = CNHperyear$YEAR, legend.text = c("CNH", "CNY"),
        ylab = "Trades Per Year")

write.csv(TruePlot, "NewData.CSV")

data.table(date = TruePlot[,DATE], TruePlot[,Mid_CNH])

p = (cajorls(ca.jo(data.table(TruePlot[,Mid_CNH], TruePlot[,Mid_CNY]) , type="eigen", K=5, spec="longrun")))
summary(p$rlm)
setnames(TruePlot,"DATE","obs")

#Make sure to check ordering of dates

levels(as.factor(mastertable$RanNum))