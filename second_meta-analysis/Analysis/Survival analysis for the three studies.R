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

####### Analysis with Rectum ######

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

Kim =      read.csv("second_meta-analysis/AD KM-plots/Kim/Simulated_data.csv"); 

names(Kim)[names(Kim)=="rightleft"] =  "Right left (with Rectum)"
names(Kim)[names(Kim)=="Dead"] =  "Dead.5year"


names(Kim); names(Data_set)





IPD=  rbind(Data_set, Margonis,Chen, Goffredo, Wang,Kim)
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



forest(meta,
       print.subgroup.labels = T, print.byvar = T)
library(grid)
grid.text("Forest plot for right vs left (with rectum) stratified by KRAS mutation", .5, 0.95, gp=gpar(cex=2))



### Crossvalidation code


Right_Left.terms.meta.KRAS.WT = metagen(TE = Surv.model.with.Rectum.stratified.by.KRAS$Coefs.with.Rectum[c(1,3,5,7,9,11,13,15)],
                                        seTE = Surv.model.with.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum[c(1,3,5,7,9,11,13,15)], sm="HR", 
                                        method.tau = "EB",adhoc.hakn = "ci", hakn = T, 
                                        studlab = unique(Surv.model.with.Rectum.stratified.by.KRAS$Study))

forest(Right_Left.terms.meta.KRAS.WT, prediction = T)
grid.text("Forest plot for right vs left (with rectum) KRAS Wild type", .5, 0.95, gp=gpar(cex=2))

Influence.Right_Left.terms.meta.KRAS.WT= metainf(Right_Left.terms.meta.KRAS.WT)
forest(Influence.Right_Left.terms.meta.KRAS.WT )
grid.text("Influence analysis (KRAS Wild type)", .5, 0.95, gp=gpar(cex=2))




funnel(Right_Left.terms.meta.KRAS.WT,comb.fixed = T,studlab = T, comb.random = T, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkgreen", "green", "lightgreen"),
       lwd = 1, cex = 1, pch = 1,  cex.studlab = 1)

metabias(Right_Left.terms.meta.KRAS.WT)



Right_Left.terms.meta.KRAS.MT = metagen(TE = Surv.model.with.Rectum.stratified.by.KRAS$Coefs.with.Rectum[c(2,4,6,8,10,12,14,16)], 
                                        seTE = Surv.model.with.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum[c(2,4,6,8,10,12,14,16)], sm="HR", 
                                        method.tau = "EB", adhoc.hakn = "ci", hakn = T, 
                                        studlab = unique(Surv.model.with.Rectum.stratified.by.KRAS$Study))

forest(Right_Left.terms.meta.KRAS.MT, prediction = T)
grid.text("Forest plot for right vs left (with rectum) KRAS mutated", .5, 0.95, gp=gpar(cex=2))

Influence.Right_Left.terms.meta.KRAS.MT= metainf(Right_Left.terms.meta.KRAS.MT)
forest(Influence.Right_Left.terms.meta.KRAS.MT )
grid.text("Influence analysis (KRAS mutated)", .5, 0.95, gp=gpar(cex=2))


rm(list=ls()[! ls() %in% c("IPD")])

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
               tau.common =F,prediction = F,hakn = T,adhoc.hakn = "ci", 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.with.Rectum.interaction, 
               print.byvar = T)


forest(meta,  prediction = T)
grid.text("Forest plot for KRAS*sideness interaction terms (Left includes Rectum tumours)", .5, 0.95, gp=gpar(cex=2))


Influence.Interaction.terms= metainf(meta)
forest(Influence.Interaction.terms )
grid.text("Influence analysis (Interaction terms)", .5, 0.95, gp=gpar(cex=2))




rm(list=ls()[! ls() %in% c("IPD")])





####### Analysis without rectum ######



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
Surv.model.without.Rectum.stratified.by.KRAS$Coefs.with.Rectum = sapply(Surv.model.without.Rectum.stratified.by.KRAS$model, function(x) coef(x)) ; 


Surv.model.without.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum= sapply(Surv.model.without.Rectum.stratified.by.KRAS$model, function(x) diag(vcov(x)))


meta = metagen(TE = Coefs.with.Rectum, seTE = Vcov.Coefs.without.Rectum, sm="HR",byvar =KRAS,
               title = "Right vs Left (stratified by KRAS)",overall = F, overall.hetstat = F,
               tau.common =F,prediction = F,hakn = T, 
               method.tau = "EB", studlab = Study, 
               data = Surv.model.without.Rectum.stratified.by.KRAS, 
               print.byvar = T,adhoc.hakn = "ci")



forest(meta,
       print.subgroup.labels = T, print.byvar = T)
library(grid)
grid.text("Forest plot for right vs left (without rectum) stratified by KRAS mutation", .5, 0.95, gp=gpar(cex=2))


### Crossvalidation code


Right_Left.terms.meta.KRAS.WT = metagen(TE = Surv.model.without.Rectum.stratified.by.KRAS$Coefs.with.Rectum[c(1,3,5,7,9,11)],
                                        seTE = Surv.model.without.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum[c(1,3,5,7,9,11)], sm="HR", 
                                        adhoc.hakn = "ci", hakn = T, method.tau = "EB")

forest(Right_Left.terms.meta.KRAS.WT, studlab = unique(IPD$Study), prediction = T)
grid.text("Forest plot for right vs left (without rectum) KRAS wild type patients", .5, 0.95, gp=gpar(cex=2))


Right_Left.terms.meta.KRAS.MT = metagen(TE = Surv.model.without.Rectum.stratified.by.KRAS$Coefs.with.Rectum[c(2,4,6,8,10,12)], 
                                        seTE = Surv.model.without.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum[c(2,4,6,8,10,12)], sm="HR", 
                                        adhoc.hakn = "ci", hakn = T, method.tau = "EB")

forest(Right_Left.terms.meta.KRAS.MT, studlab = unique(IPD$Study), prediction = T)
grid.text("Forest plot for right vs left (without rectum) KRAS mutated patients", .5, 0.95, gp=gpar(cex=2))


### Meta-analysis without Yamashita and De Santibanes
Right_Left.terms.meta.KRAS.MT = metagen(TE = Surv.model.without.Rectum.stratified.by.KRAS$Coefs.with.Rectum[c(4,6,8,10)], 
                                        seTE = Surv.model.without.Rectum.stratified.by.KRAS$Vcov.Coefs.without.Rectum[c(4,6,8,10)], 
                                        sm="HR", adhoc.hakn = "ci", hakn = T,
                                        method.tau = "EB")

forest(Right_Left.terms.meta.KRAS.MT, studlab = unique(IPD$Study)[2:5], prediction = T)
grid.text("Forest plot for right vs left (without rectum) KRAS mutated patients", .5, 0.95, gp=gpar(cex=2))





rm(list=ls()[! ls() %in% c("IPD")])

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


forest(meta,  prediction = T)
grid.text("Forest plot for KRAS*sideness interaction terms (Left doesn't include Rectum tumours)", .5, 0.95, gp=gpar(cex=2))


rm(list=ls()[! ls() %in% c("IPD")])





