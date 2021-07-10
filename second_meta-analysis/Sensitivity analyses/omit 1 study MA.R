#### We perform a survival analysis for each data-set 

library(readxl)
library(dplyr)
library("survival")
library("survminer")
library(ggplot2)
library(broom)
library(mvmeta)
library(meta)

rm(list=ls())

#### Analysis with Rectum

IPD= read_xlsx("second_meta-analysis/IPD/Full IPD (light).xlsx",sheet = 1)

IPD$Dead.5year =  ifelse(IPD$Time>1825, 0, IPD$Dead)

Margonis =  read_xlsx("second_meta-analysis/IPD/Margonis.xlsx",sheet = 1)

names(Margonis); names(IPD)


names(Margonis)[names(Margonis)=="rvsl"] =  "Right left (with Rectum)"
names(Margonis)[names(Margonis)=="vitalstatus"] =  "Dead" 
names(Margonis)[names(Margonis)=="codonspecifickras3"] =  "KRAS" 

Margonis$Study=  "Margonis"
Margonis$Dead.5year =  ifelse(Margonis$Time>1825, 0, Margonis$Dead)

Chen =  read.csv("second_meta-analysis/AD KM-plots/Chen/Chen_Simulated_data.csv", check.names=FALSE )

names(Chen)

Chen$Dead.5year =  ifelse(Chen$Time>1825, 0, Chen$Dead)


Goffredo =  read.csv("second_meta-analysis/AD KM-plots/Goffredo/Goffredo_Simulated_data.csv", check.names=FALSE )

Goffredo$Dead.5year =  ifelse(Goffredo$Time>1825, 0, Goffredo$Dead)


Data_set =  IPD%>%
  select(Study,Time, Dead.5year,KRAS,  `Right left (with Rectum)` )

Margonis =  Margonis%>%
  select(Study,Time, Dead.5year,KRAS,  `Right left (with Rectum)` )

Chen =  Chen%>%
  select(Study,Time, Dead.5year,KRAS,  `Right left (with Rectum)` )

Goffredo =  Goffredo%>%
  select(Study,Time, Dead.5year,KRAS,  `Right left (with Rectum)` )


Wang =      read.csv("second_meta-analysis/AD KM-plots/Wang/Simulated_data.csv"); 

names(Wang)[names(Wang)=="rightleft"] =  "Right left (with Rectum)"
names(Wang)[names(Wang)=="Dead"] =  "Dead.5year"


names(Wang); names(Data_set)


IPD=  rbind(Data_set, Margonis,Chen, Goffredo, Wang)
rm(list=ls()[! ls() %in% c("IPD")])


## Fit a RCS model per study (with Rectum)
Surv.model.with.Rectum.stratified.by.KRAS = IPD%>%
  arrange(desc(Study))%>%
  group_by(Study, KRAS) %>%
  do(model = coxph(formula = Surv(`Time`,`Dead.5year`==1)~`Right left (with Rectum)`, ### For each study we fit RCS 
                   data = .))





Surv.model.with.Rectum.stratified.by.KRAS= Surv.model.with.Rectum.stratified.by.KRAS %>%
  mutate(KRAS = recode(KRAS, `0` = "Wild type",
                       `1` = "Mutated")
  )




### Extract the coefficients from each study
Surv.model.with.Rectum.stratified.by.KRAS$Coefs.with.Rectum = sapply(Surv.model.with.Rectum.stratified.by.KRAS$model, function(x) coef(x)) ; 


Surv.model.with.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum= sapply(Surv.model.with.Rectum.stratified.by.KRAS$model, function(x) diag(vcov(x)))


meta = metagen(TE = Coefs.with.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
               title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
               tau.common =F,prediction = F,hakn = T, 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.with.Rectum.stratified.by.KRAS, 
               print.byvar = T,adhoc.hakn = "ci")



meta.WT = metagen(TE = Coefs.with.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
               title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
               tau.common =F,prediction = F,hakn = T, 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.with.Rectum.stratified.by.KRAS[Surv.model.with.Rectum.stratified.by.KRAS$KRAS=="Wild type",], 
               print.byvar = T,adhoc.hakn = "ci")

meta.MT = metagen(TE = Coefs.with.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                  title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                  tau.common =F,
                  prediction = F,
                  hakn = T, 
                  method.tau = "EB", 
                  studlab = Study, 
                  data = Surv.model.with.Rectum.stratified.by.KRAS[Surv.model.with.Rectum.stratified.by.KRAS$KRAS== "Mutated",], 
                  print.byvar = T,adhoc.hakn = "ci")



forest(meta,
       print.subgroup.labels = T, print.byvar = T)
library(grid)
grid.text("Forest plot for right vs left (with rectum) stratified by KRAS mutation", .5, 0.95, gp=gpar(cex=2))






##### Interaction terms meta-analysis


## Fit a RCS model per study (with Rectum)
Surv.model.with.Rectum.interaction = IPD%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = coxph(formula = Surv(`Time`,`Dead.5year`==1)~`Right left (with Rectum)`*KRAS, ### For each study we fit RCS 
                   data = .))





### Extract the coefficients from each study
Surv.model.with.Rectum.interaction$Interaction.terms.with.Rectum = 
  sapply(Surv.model.with.Rectum.interaction$model, function(x) coef(x)[3]) ; 


Surv.model.with.Rectum.interaction$Interaction.Vcov.Coefs.without.Rectum= 
  sapply(Surv.model.with.Rectum.interaction$model, function(x) diag(vcov(x))[3])


meta = metagen(TE = Interaction.terms.with.Rectum, seTE = Interaction.Vcov.Coefs.without.Rectum, 
               sm="HR",
               title = "Interaction terms (KRAS*Left)",
               overall = T, overall.hetstat = T,
               tau.common =F,prediction = F,hakn = T, 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.with.Rectum.interaction, 
               print.byvar = T,adhoc.hakn = "ci")


forest(meta,  prediction = T)
grid.text("Forest plot for KRAS*sideness interaction terms (Left includes Rectum tumours)", .5, 0.95, gp=gpar(cex=2))




#### Omit 1 study sensitivity analysis 

#### Sensitivity analysis 

Mat =  matrix(NA, nrow = 9,ncol = 4+3)
Mat =  as.data.frame(Mat)
names(Mat) =  c( "Study omited", "Wild Type (Random)", "Mutated (Random)", "Interaction terms (Random)",
                 "Wild Type (Fixed)", "Mutated (Fixed)", "Interaction terms (Fixed)")
count= 1

Mat[9,1] = "Overall pooled estimate"
Mat[9,2] = round(exp(meta.WT$TE.random),2)
Mat[9,3] = round(exp(meta.MT$TE.random),2)
Mat[9,4] = round(exp(meta$TE.random),2)
Mat[9,5] = round(exp(meta.WT$TE.fixed),2)
Mat[9,6] = round(exp(meta.MT$TE.fixed),2)
Mat[9,7] = round(exp(meta$TE.fixed),2)


Mat



for(i in unique(Surv.model.with.Rectum.stratified.by.KRAS$Study)){
  
  print(i)
  
  minidf = Surv.model.with.Rectum.stratified.by.KRAS[!(Surv.model.with.Rectum.stratified.by.KRAS$Study==i),]
  minidf.inter = Surv.model.with.Rectum.interaction[!(Surv.model.with.Rectum.interaction$Study==i),]
  
  
  meta.WT = metagen(TE = Coefs.with.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                    title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                    tau.common =F,prediction = F,hakn = T, 
                    method.tau = "EB", studlab = Study, 
                    data = minidf[minidf$KRAS == "Wild type",], 
                    print.byvar = T,adhoc.hakn = "ci")
  
  meta.MT = metagen(TE = Coefs.with.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                    title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                    tau.common =F,
                    prediction = F,
                    hakn = T, 
                    method.tau = "EB", 
                    studlab = Study, 
                    data = minidf[minidf$KRAS == "Mutated",], 
                    print.byvar = T,adhoc.hakn = "ci")
  
  meta = metagen(TE = Interaction.terms.with.Rectum, seTE = Interaction.Vcov.Coefs.without.Rectum, 
                 sm="HR",
                 title = "Interaction terms (KRAS*Left)",
                 overall = T, overall.hetstat = T,
                 tau.common =F,prediction = F,hakn = T, 
                 method.tau = "EB", studlab = Study, 
                 data = minidf.inter, 
                 print.byvar = T,adhoc.hakn = "ci")
  
  
  
  Mat[count,1] = i
  Mat[count,2] = round(exp(meta.WT$TE.random),2)
  Mat[count,3] = round(exp(meta.MT$TE.random),2)
  Mat[count,4] = round(exp(meta$TE.random),2)
  Mat[count,5] = round(exp(meta.WT$TE.fixed),2)
  Mat[count,6] = round(exp(meta.MT$TE.fixed),2)
  Mat[count,7] = round(exp(meta$TE.fixed),2)
  
  
  count= count+1
  
  
  
  
}


sens = Mat[,-c(2,3,4)]


write.csv(sens,"second_meta-analysis/Sensitivity analyses/omit 1 study MA (with rectum).csv")









