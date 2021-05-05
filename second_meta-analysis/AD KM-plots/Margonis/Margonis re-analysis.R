######  Margonis re-analysis

rm(list=ls())
Margonis =  read_xlsx("second_meta-analysis/IPD/Margonis.xlsx",sheet = 1 )

Margonis$Dead.5year =  ifelse(Margonis$Time>1825, 0, Margonis$Dead)

names(Margonis)


names(Margonis)[names(Margonis)=="rvsl"] =  "Right left (with Rectum)"
names(Margonis)[names(Margonis)=="rvslexcluderectum"] =  "rightleft"
names(Margonis)[names(Margonis)=="vitalstatus"] =  "Dead" 
names(Margonis)[names(Margonis)=="codonspecifickras3"] =  "KRAS" 

Margonis$Study=  "Margonis"
Margonis$Dead.5year =  ifelse(Margonis$Time>1825, 0, Margonis$Dead)


Simulated.Margonis =  read.csv("second_meta-analysis/AD KM-plots/Margonis/Simulated_data.csv")


Margonis = Margonis%>%
  mutate(KRAS = recode(KRAS, `0` = "Wild type",
                       `1` = "Mutated"))




Margonis.fit =  survfit(Surv(time = `Time`,event = `d`==1) ~ rightleft+KRAS, data = Margonis)
names(Margonis)



ggsurvplot(Margonis.fit, 
           risk.table = TRUE,  
           data = Margonis, 
           xlim = c(0,1825), break.x.by =365)


test = Margonis[Margonis$Time<1825,]%>%
  arrange(desc(KRAS))%>%
  group_by(as.factor(KRAS))%>%
  do(model = coxph(formula = Surv(`Time`,`Dead.5year`==1)~`Right left (with Rectum)`, ### For each study we fit RCS 
                   data = .))



test$model
