#Algorithm to create a raw dataset from DigizeIt readings from a Kaplan-Meier curve
library("MASS")
library("splines")
library("survival")
library(purrr)

### Wild Type Right Side (34)
tot.events<-"31" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-wt_Right" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_mt_Left
Kras_WT_Right<- read.csv("First meta-analysis/Papers/Simulated IPD/Becker/Right side Wild type.csv")


t.S<-Kras_WT_Right[,1]/30
S<-Kras_WT_Right[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))


### Total number of patients 34


n.risk<-c(34,24,14,3,2,1)



#Read in published numbers at risk, n.risk, at time, t.risk, lower and upper 
# indexes for time interval
source("second_meta-analysis/Analysis/Simulator.R")



#Output IPD

IPD<-as.data.frame(matrix(c(t.IPD,event.IPD,arm),ncol=3,byrow=F))
IPD[,1:2] =  apply(IPD[,1:2],2,as.numeric)


colnames(IPD)= c("Time", "Dead", "KRAS")

IPD$rightleft = lapply(X = strsplit( as.character(IPD$KRAS),"_"), "[", 2)
IPD$KRAS = lapply(X = strsplit( as.character(IPD$KRAS),"_"), "[", 1)

IPD[,1:2] =  apply(IPD[,1:2], 2, as.numeric)
test.surv = survfit(Surv(Time, Dead)~1, data=IPD)
plot(test.surv)

### Mutated Right Side (85)


tot.events<-"76" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-mt_Right" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_wt_Left
Kras_mt_Right<- read.csv("First meta-analysis/Papers/Simulated IPD/Becker/Right side KRAS.csv")

t.S<-Kras_mt_Right[,1]/30
S<-Kras_mt_Right[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(85,74,43,20,13,8)



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


############ Wild type Left side (175)

rm(list=ls()[! ls() %in% c("IPD")])
tot.events<-"124" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-wt_Left" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_wt_Left
Kras_wt_Left<- read.csv("First meta-analysis/Papers/Simulated IPD/Becker/Left side Wild Type.csv")

t.S<-Kras_wt_Left[,1]/30
S<-Kras_wt_Left[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(175,146,116,89,52,24)

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

############ Mutated Left side (195)


rm(list=ls()[! ls() %in% c("IPD")])
tot.events<-"158" #tot.events = total no. of events reported. If not reported, then tot.events="NA"
arm.id<-"Kras-mt_Left" #arm indicator
###END FUNCTION INPUTS
#Read in survival times read by Kras_wt_Left
Kras_mt_Left<- read.csv("First meta-analysis/Papers/Simulated IPD/Becker/Left side KRAS mutated.csv")

t.S<-Kras_mt_Left[,1]/30
S<-Kras_mt_Left[,2]

t.risk<-seq(0,60,by=12)
lower<-map_dbl(t.risk, function(x) min(which(t.S >=x)))
upper<-map_dbl(c(t.risk[-1],Inf), function(x) max(which(t.S<x)))
n.risk<-c(191,187,164,139,100,86)

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


write.csv(x = IPD,"second_meta-analysis/AD KM-plots/Fiala/Simulated_data.csv")











