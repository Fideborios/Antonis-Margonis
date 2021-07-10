### Demographics for the IPD

library(readxl)
library(dplyr)
library(ggplot2)
library(broom)


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

Kim =      read.csv("second_meta-analysis/AD KM-plots/Kim/Simulated_data.csv"); 

names(Kim)[names(Kim)=="rightleft"] =  "Right left (with Rectum)"
names(Kim)[names(Kim)=="Dead"] =  "Dead.5year"


names(Kim); names(Data_set)



IPD=  rbind(Data_set, Margonis,Chen, Goffredo, Wang,Kim)
rm(list=ls()[! ls() %in% c("IPD")])
table(IPD$Study)



Gagniere = read_xlsx("second_meta-analysis/IPD/Gagniere.xlsx",1)


median(Margonis[!is.na(Margonis$`Right left (with Rectum)`),]$age,na.rm = T)

Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(`Right left (with Rectum)`)%>%
  summarise(median(age,na.rm = T))


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(`Right left (with Rectum)`)%>%
  summarise("1st quantile" = quantile(x = age, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = age, na.rm = T,probs = 3/4))


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(gender)%>%
  summarise(n())

Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(`Right left (with Rectum)`,gender)%>%
  summarise(n())

Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(synchronous)%>%
  summarise(n())


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  summarise(Median = median(tumornumber,na.rm = T),
            "1st quantile" = quantile(x = tumornumber, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = tumornumber, na.rm = T,probs = 3/4))


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(`Right left (with Rectum)`)%>%
  summarise(Median = median(tumornumber,na.rm = T),
            "1st quantile" = quantile(x = tumornumber, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = tumornumber, na.rm = T,probs = 3/4))


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(extrahepaticdisease)%>%
  summarise(n())

Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(`Right left (with Rectum)`,extrahepaticdisease)%>%
  summarise(n())


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(r0resection)%>%
  summarise(n())


Margonis[!is.na(Margonis$`Right left (with Rectum)`),]%>%
  group_by(posthepaticresectionchemotherapy)%>%
  summarise(n())


Santibanes =  read_xlsx("second_meta-analysis/IPD/De Santibanes cohort.xlsx",1)

Santibanes%>%
  group_by(rightleft, gender)%>%
  summarise(n() )

Santibanes%>%
  group_by(`synchro 0us (less than 12 months apart)`)%>%
  summarise(n() )


Santibanes%>%
  group_by(`extrahepatic disease resected`)%>%
  summarise(n() )

Santibanes%>%
  group_by(rightleft,`extrahepatic disease resected`)%>%
  summarise(n() )

Santibanes%>%
  group_by(R)%>%
  summarise(n() )



Santibanes%>%
  summarise(Median = median(`number of liver metastases`,na.rm = T),
            "1st quantile" = quantile(x = `number of liver metastases`, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = `number of liver metastases`, na.rm = T,probs = 3/4))






IPD$`Right left (with Rectum)` <- recode(IPD$`Right left (with Rectum)`,`0` = "Right", `1` = "Left")
IPD$KRAS <- recode(IPD$KRAS,`0` = "Wild type", `1` = "Mutated")


IPD%>%
  group_by(Study,KRAS)%>%
  summarise(n() )





a= IPD%>%
  group_by(Study,`Right left (with Rectum)`,KRAS)%>%
  summarise(n() )



IPD= read_xlsx("second_meta-analysis/IPD/Full IPD (light).xlsx",sheet = 1)

names(IPD)


IPD%>%
  group_by(Study)%>%
  summarise(median(Age,na.rm = T) )

IPD%>%
  group_by(Study)%>%
  summarise("1st quantile" = quantile(x = Age, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = Age, na.rm = T,probs = 3/4))


IPD%>%
  group_by(Study,`Right left (with Rectum)`)%>%
  summarise(median(Age,na.rm = T) )


IPD%>%
  group_by(Study,`Right left (with Rectum)`)%>%
  summarise("1st quantile" = quantile(x = Age, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = Age, na.rm = T,probs = 3/4))




IPD%>%
  group_by(Study,Gender)%>%
  summarise(n() )


IPD%>%
  group_by(Study,`Right left (with Rectum)`,Gender)%>%
  summarise(n() )



French = read_xlsx("second_meta-analysis/IPD/french db.xlsx",1)

French%>%
  group_by(synchronous)%>%
  summarise(n() )


French%>%
  group_by(rightleft)%>%
  summarise("Median" =  median(number,na.rm = T), 
            "1st quantile" = quantile(x = number, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = number, na.rm = T,probs = 3/4)
  )


  
French%>%
  summarise( "Median" =  median(number,na.rm = T), 
             "1st quantile" = quantile(x = number, na.rm = T,probs = 1/4),
             "2nd quantile" = quantile(x = number, na.rm = T,probs = 3/4)
             )



French%>%
  group_by(extrahep)%>%
  summarise(n() )

French%>%
  group_by(rightleft,extrahep)%>%
  summarise(n() )


French%>%
  group_by(R0)%>%
  summarise(n() )


French%>%
  group_by(adjuvantchemo)%>%
  summarise(n() )



IPD%>%
  group_by(Study)%>%
  summarise("1st quantile" = quantile(x = Age, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = Age, na.rm = T,probs = 3/4))


IPD%>%
  group_by(Study,`Right left (with Rectum)`)%>%
  summarise(median(Age,na.rm = T) )


IPD%>%
  group_by(Study,`Right left (with Rectum)`)%>%
  summarise("1st quantile" = quantile(x = Age, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = Age, na.rm = T,probs = 3/4))



Gagniere%>%
  group_by(rightleft)%>%
  summarise(median(number_liver_tumors_at_path__non,na.rm = T) )

Gagniere%>%
  group_by(rightleft)%>%
  summarise("1st quantile" = quantile(x = number_liver_tumors_at_path__non, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = number_liver_tumors_at_path__non, na.rm = T,probs = 3/4))



table(Gagniere$`EHD at time of resection`)

table(Gagniere$rightleft,
      Gagniere$`EHD at time of resection` 
      )


table(Gagniere$margin_1pos_0neg)/sum(table(Gagniere$margin_1pos_0neg))

table(Gagniere$adj_chemo_1yes_0no)/
  sum(table(Gagniere$adj_chemo_1yes_0no))





Gagniere%>%
  group_by(rightleft)%>%
  summarise(median(number_liver_tumors_at_path__non,na.rm = T) )

Gagniere%>%
  group_by(rightleft)%>%
  summarise("1st quantile" = quantile(x = number_liver_tumors_at_path__non, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = number_liver_tumors_at_path__non, na.rm = T,probs = 3/4))


Santibanes =  read_xlsx("second_meta-analysis/IPD/De Santibanes cohort.xlsx",1)

Santibanes%>%
  group_by(rightleft,`number of liver metastases`)%>%
  summarise(perc = n())





quantile(Santibanes$`number of liver metastases`,probs = 3/4)




Santibanes%>%
  group_by(rightleft)%>%
  summarise("1st quantile" = quantile(x = `number of liver metastases`, na.rm = T,probs = 1/4),
            "2nd quantile" = quantile(x = `number of liver metastases`, na.rm = T,probs = 3/4))




IPD[IPD$Study=="De Santibanes" | IPD$Study=="French",]$Study = "Marques"


table(IPD[IPD$Study== "Gagniere",]$Gender)




table(Santibanes$`number of liver metastases`)
