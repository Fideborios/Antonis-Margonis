#Algorithm to create a raw dataset from DigizeIt readings from a Kaplan-Meier curve
library("MASS")
library("splines")
library("survival")
library(purrr)
###FUNCTION INPUTS

tot.events<-"272" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-mt_Left" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_mt_Left
Kras_mt_Left<- read.csv("second_meta-analysis/AD KM-plots/Wang/MT-Type _LS_.csv")
# path = "second_meta-analysis/AD KM-plots/Yamashita/"

t.S<-Kras_mt_Left[,1]
S<-Kras_mt_Left[,2]

t.risk<-seq(0,1826,by=365)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(317,282,205,135,84,56)



#Read in published numbers at risk, n.risk, at time, t.risk, lower and upper 
# indexes for time interval
source("second_meta-analysis/Analysis/Simulator.R")



#Output IPD

IPD<-as.data.frame(matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F))
IPD[,1:2] =  apply(IPD[,1:2],2,as.numeric)


colnames(IPD)= c("Time", "Dead", "KRAS")

IPD$rightleft = lapply(X = strsplit( as.character(IPD$KRAS),"_"), "[", 2)
IPD$KRAS = lapply(X = strsplit( as.character(IPD$KRAS),"_"), "[", 1)


############ Left side Mutated


tot.events<-"NA" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-mt_Left" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_wt_Left
Kras_mt_Left<- read.csv("second_meta-analysis/AD KM-plots/Yamashita/Left_Side_RAS_MT.csv")

t.S<-Kras_mt_Left[,1]
S<-Kras_mt_Left[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(170,151,88,49,29,20)



source("second_meta-analysis/Analysis/Simulator.R")


#Output IPD

temp<-as.data.frame(matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F))
temp[,1:2] =  apply(temp[,1:2],2,as.numeric)


colnames(temp)= c("Time", "Dead", "KRAS")

temp$rightleft = lapply(X = strsplit( as.character(temp$KRAS),"_"), "[", 2)
temp$KRAS = lapply(X = strsplit( as.character(temp$KRAS),"_"), "[", 1)


test.surv = survfit(Surv(Time, Dead)~1, data=temp)
plot(test.surv)

IPD= rbind(IPD, temp)


############ Right side Wild type

rm(list=ls()[! ls() %in% c("IPD")])
tot.events<-"NA" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-wt_Right" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_wt_Left
Kras_wt_Right<- read.csv("second_meta-analysis/AD KM-plots/Yamashita/Right_Side_RAS_WT.csv")

t.S<-Kras_wt_Right[,1]
S<-Kras_wt_Right[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(146,123,84,46,27,17)

source("second_meta-analysis/Analysis/Simulator.R")


#Output IPD

temp<-as.data.frame(matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F))
temp[,1:2] =  apply(temp[,1:2],2,as.numeric)


colnames(temp)= c("Time", "Dead", "KRAS")

temp$rightleft = lapply(X = strsplit( as.character(temp$KRAS),"_"), "[", 2)
temp$KRAS = lapply(X = strsplit( as.character(temp$KRAS),"_"), "[", 1)

test.surv = survfit(Surv(Time, Dead)~1, data=temp)
plot(test.surv)


IPD= rbind(IPD, temp)

############ Right side mutated


rm(list=ls()[! ls() %in% c("IPD")])
tot.events<-"NA" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-mt_Right" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_wt_Left
Kras_mt_Right<- read.csv("second_meta-analysis/AD KM-plots/Yamashita/Right_Side_RAS_MT.csv")

t.S<-Kras_mt_Right[,1]
S<-Kras_mt_Right[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(92,72,33,15,7,4)

source("second_meta-analysis/Analysis/Simulator.R")


#Output IPD

temp<-as.data.frame(matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F))
temp[,1:2] =  apply(temp[,1:2],2,as.numeric)


colnames(temp)= c("Time", "Dead", "KRAS")

temp$rightleft = lapply(X = strsplit( as.character(temp$KRAS),"_"), "[", 2)
temp$KRAS = lapply(X = strsplit( as.character(temp$KRAS),"_"), "[", 1)

test.surv = survfit(Surv(Time, Dead)~1, data=temp)
plot(test.surv)


IPD= rbind(IPD, temp)
IPD[,3:4] =  apply(IPD[,3:4],2,as.character) 

test.surv = survfit(Surv(Time, Dead)~ KRAS+rightleft, data=IPD)
plot(test.surv)

#### Save file


write.csv(x = IPD,"second_meta-analysis/AD KM-plots/Yamashita/Simulated_data.csv")




###### Simulate Wang data-set









