load("workspaceImage.RData")

install.packages("tsDyn")
install.packages("dplyr")
install.packages("data.table")
install.packages("urca")
library(tsDyn)
library(dplyr)
library(data.table)
library(urca)

mastertable1 = data.table(CNHCoefm = 1:2000000, CNYCoefm = 1:2000000, CnhTstat = 1:2000000,
                          RanNum = 0, DateCode = 0, EntryCode = 0, CnyTstat = 1:2000000,
                          Date = "none")
for (i in 754:500) {
  p = (i * 1000 - 1000)
  for (g in 1:911) {
    filecnh = unlist(listCNH[[i]])
    filecny = unlist(listCNY[[i]])
    VECMtable = na.omit(data.table(CNH = filecnh, CNY = filecny, date = datelist))[g:(g+99)]
    name1 = VECMtable$date[1]
    name2 = VECMtable$date[length(VECMtable$date)]
    name = paste(name1, name2, sep = ' till ')
    VECMtable[,date:=NULL]
    x = cajorls(ca.jo(VECMtable, type="eigen", K=5, spec="longrun"))
    x1 = summary(x$rlm)
    mastertable1$CNHCoefm[(p + g)] =  x$rlm$coefficients[1,1] #CNH coef
    mastertable1$CNYCoefm[(p + g)] =  x$rlm$coefficients[1,2] #CNY coef
    mastertable1$CnhTstat[(p + g)] = x1$`Response CNH.d`$coefficients[1,3] #ecterm t stat for CNH
    mastertable1$CnyTstat[(p + g)] = x1$`Response CNY.d`$coefficients[1,3] #ec term t stat for CNY
    mastertable1$Date[(p + g)] = name
    mastertable1$RanNum[(p + g)] = i
    mastertable1$DateCode[(p + g)] = g
    mastertable1$EntryCode[(p + g)] = (p + g)
    print((p + g))
  }}

write.csv(mastertable1, "mastertable.csv")



