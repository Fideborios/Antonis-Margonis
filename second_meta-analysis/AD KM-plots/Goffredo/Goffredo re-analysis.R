######  Goffredo re-analysis


Goffredo =  read.csv("second_meta-analysis/AD KM-plots/Goffredo/Goffredo_Simulated_data.csv", check.names=FALSE )

Goffredo$Dead.5year =  ifelse(Goffredo$Time>1825, 0, Goffredo$Dead)



Goffredo.fit =  survfit(Surv(time = `Time`,event = `Dead.5year`==1) ~ `rightleft`+ as.factor(KRAS), data = Goffredo)
names(Goffredo)



ggsurvplot(Goffredo.fit, risk.table = TRUE,  data = Goffredo )


test = Goffredo%>%
  arrange(desc(KRAS))%>%
  group_by(as.factor(KRAS))%>%
  do(model = coxph(formula = Surv(`Time`,`Dead.5year`==1)~`Right left (with Rectum)`, ### For each study we fit RCS 
                   data = .))



test$model

