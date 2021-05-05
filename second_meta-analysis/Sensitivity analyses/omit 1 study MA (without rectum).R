#### We perform a survival analysis for each data-set 

library(readxl)
library(dplyr)
library("survival")
library("survminer")
library(ggplot2)
library(broom)
library(mvmeta)
library(meta)



### Analysis without rectum


rm(list=ls()[! ls() %in% c("IPD")])

#### Analysis with Rectum

IPD= read_xlsx("second_meta-analysis/IPD/Full IPD (light).xlsx",sheet = 1)

Yamashita=  read.csv("second_meta-analysis/AD KM-plots/Yamashita/Simulated_data.csv", check.names=FALSE)

Margonis =  read_xlsx("second_meta-analysis/IPD/Margonis.xlsx",sheet = 1)

Chen =  read.csv("second_meta-analysis/AD KM-plots/Chen/Chen_Simulated_data.csv", check.names=FALSE )




names(Margonis)



names(IPD)


names(Margonis)[names(Margonis)=="rvslexcluderectum"] =  "rightleft"
names(Margonis)[names(Margonis)=="vitalstatus"] =  "Dead" 
names(Margonis)[names(Margonis)=="codonspecifickras3"] =  "KRAS" 

Margonis$Study=  "Margonis"
Margonis$Dead.5year =  ifelse(Margonis$Time>1825, 0, IPD$Dead)


Data_set =  IPD%>%
  select(Study,Time, Dead,KRAS,  `rightleft` )

Margonis =  Margonis%>%
  select(Study,Time, Dead,KRAS,  `rightleft` )

Chen =  Chen%>%
  select(Study,Time, Dead,KRAS,  `rightleft` )

Yamashita =  Yamashita%>%
  select(Study,Time, Dead,KRAS,  `rightleft` )



IPD= rbind(Data_set, Margonis, Chen, Yamashita)


rm(list=ls()[! ls() %in% c("IPD")])


## Fit a RCS model per study (with Rectum)
Surv.model.without.Rectum.stratified.by.KRAS = IPD%>%
  arrange(desc(Study))%>%
  group_by(Study, KRAS) %>%
  do(model = coxph(formula = Surv(`Time`,`Dead`==1)~rightleft, ### For each study we fit RCS 
                   data = .))


Surv.model.without.Rectum.stratified.by.KRAS= Surv.model.without.Rectum.stratified.by.KRAS %>%
  mutate(KRAS = recode(KRAS, `0` = "Wild type",
                       `1` = "Mutated")
  )




### Extract the coefficients from each study
Surv.model.without.Rectum.stratified.by.KRAS$Coefs.without.Rectum = sapply(Surv.model.without.Rectum.stratified.by.KRAS$model, function(x) coef(x)) ; 


Surv.model.without.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum= sapply(Surv.model.without.Rectum.stratified.by.KRAS$model, function(x) diag(vcov(x)))



meta = metagen(TE = Coefs.without.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
               title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
               tau.common =F,prediction = F,hakn = T, 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.without.Rectum.stratified.by.KRAS, 
               print.byvar = T,adhoc.hakn = "ci")




meta.WT = metagen(TE = Coefs.without.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                  title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                  tau.common =F,prediction = F,hakn = T, 
                  method.tau = "EB", studlab = Study, 
                  data = Surv.model.without.Rectum.stratified.by.KRAS[Surv.model.without.Rectum.stratified.by.KRAS$KRAS=="Wild type",], 
                  print.byvar = T,adhoc.hakn = "ci")

meta.MT = metagen(TE = Coefs.without.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                  title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                  tau.common =F,
                  prediction = F,
                  hakn = T, 
                  method.tau = "EB", 
                  studlab = Study, 
                  data = Surv.model.without.Rectum.stratified.by.KRAS[Surv.model.without.Rectum.stratified.by.KRAS$KRAS== "Mutated",], 
                  print.byvar = T,adhoc.hakn = "ci")



##### Interaction terms meta-analysis


## Fit a RCS model per study (without Rectum)
Surv.model.without.Rectum.interaction = IPD%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = coxph(formula = Surv(`Time`,`Dead`==1)~rightleft*KRAS, ### For each study we fit RCS 
                   data = .))





### Extract the coefficients from each study
Surv.model.without.Rectum.interaction$Interaction.terms.without.Rectum = 
  sapply(Surv.model.without.Rectum.interaction$model, function(x) coef(x)[3]) ; 


Surv.model.without.Rectum.interaction$Interaction.Vcov.Coefs.without.Rectum= 
  sapply(Surv.model.without.Rectum.interaction$model, function(x) diag(vcov(x))[3])


meta = metagen(TE = Interaction.terms.without.Rectum, seTE = Interaction.Vcov.Coefs.without.Rectum, 
               sm="HR",
               title = "Interaction terms (KRAS*Left)",
               overall = T, overall.hetstat = T,
               tau.common =F,prediction = F,hakn = T, 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.without.Rectum.interaction, 
               print.byvar = T,adhoc.hakn = "ci")





#### Omit 1 study sensitivity analysis 

#### Sensitivity analysis 

Mat =  matrix(NA, nrow = 7,ncol = 4+3)
Mat =  as.data.frame(Mat)
names(Mat) =  c( "Study omited", "Wild Type (Random)", "Mutated (Random)", "Interaction terms (Random)",
                 "Wild Type (Fixed)", "Mutated (Fixed)", "Interaction terms (Fixed)")
count= 1

Mat[7,1] = "Overall pooled estimate"
Mat[7,2] = round(exp(meta.WT$TE.random),2)
Mat[7,3] = round(exp(meta.MT$TE.random),2)
Mat[7,4] = round(exp(meta$TE.random),2)
Mat[7,5] = round(exp(meta.WT$TE.fixed),2)
Mat[7,6] = round(exp(meta.MT$TE.fixed),2)
Mat[7,7] = round(exp(meta$TE.fixed),2)


Mat


for(i in unique(Surv.model.without.Rectum.stratified.by.KRAS$Study)){
  
  print(i)
  
  minidf = Surv.model.without.Rectum.stratified.by.KRAS[!(Surv.model.without.Rectum.stratified.by.KRAS$Study==i),]
  minidf.inter = Surv.model.without.Rectum.interaction[!(Surv.model.without.Rectum.interaction$Study==i),]
  
  
  meta.WT = metagen(TE = Coefs.without.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                    title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                    tau.common =F,prediction = F,hakn = T, 
                    method.tau = "EB", studlab = Study, 
                    data = minidf[minidf$KRAS == "Wild type",], 
                    print.byvar = T,adhoc.hakn = "ci")
  
  meta.MT = metagen(TE = Coefs.without.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
                    title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
                    tau.common =F,
                    prediction = F,
                    hakn = T, 
                    method.tau = "EB", 
                    studlab = Study, 
                    data = minidf[minidf$KRAS == "Mutated",], 
                    print.byvar = T,adhoc.hakn = "ci")
  
  meta = metagen(TE = Interaction.terms.without.Rectum, seTE = Interaction.Vcov.Coefs.without.Rectum, 
                 sm="HR",
                 title = "Interaction terms (KRAS*Left)",
                 overall = T, overall.hetstat = T,
                 tau.common =F,prediction = F,hakn = T, 
                 method.tau = "EB", studlab = Study, 
                 data = minidf.inter, 
                 print.byvar = T,adhoc.hakn = "ci")
  
  
  
  
  Mat[count,2] = round(exp(meta.WT$TE.random),2)
  Mat[count,3] = round(exp(meta.MT$TE.random),2)
  Mat[count,4] = round(exp(meta$TE.random),2)
  Mat[count,5] = round(exp(meta.WT$TE.fixed),2)
  Mat[count,6] = round(exp(meta.MT$TE.fixed),2)
  Mat[count,7] = round(exp(meta$TE.fixed),2)
  
  Mat[count,1] = i
  
  count= count+1
  
  
  
  
}


Mat


write.csv(Mat,"second_meta-analysis/Sensitivity analyses/omit 1 study MA (without rectum).csv")









