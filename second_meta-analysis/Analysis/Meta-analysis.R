#### Meta-analysis of KRAS-mt location interaction term 


## Fit a RCS model per study (without Rectum)
Surv.model.without.Rectum.interaction = IPD%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = coxph(formula = Surv(`Time`,`Dead`==1)~rightleft*KRAS, ### For each study we fit RCS 
                   data = .))



## Fit a RCS model per study (with Rectum)
Surv.model.with.Rectum.interaction = IPD%>%
  arrange(desc(Study))%>%
  group_by(Study) %>%
  do(model = coxph(formula = Surv(`Time`,`Dead`==1)~`Right left (with Rectum)`*KRAS, ### For each study we fit RCS 
                   data = .))


Surv.model.without.Rectum.interaction$model
Surv.model.with.Rectum.interaction$model


Coefs.model.without.Rectum.Interaction.terms = data.frame(estimate =  c(0.3332,0.2035 ,0.2320), std.error = c(0.4664,0.2797,0.2561), 
                                                          Study=c("De Santibanes" ,"French","Gagniere"))

Coefs.with.Rectum.Interaction.terms = data.frame(estimate =  c(0.3474,0.06286,0.2231), std.error = c(0.4437,0.24101,0.2297), 
                                                 Study=c("De Santibanes" ,"French","Gagniere"))

Surv.model.without.Rectum.Interaction.terms =  Surv.model.with.Rectum%>%
  mutate(tidy(model))


Surv.model.without.Rectum.interaction%>%
  mutate(tidy(model))




library(meta)

meta_IPD.without.Rectum = Coefs.without.Rectum%>%
  metagen(TE = estimate,seTE =  std.error,data = .,byvar = KRAS,studlab = Study,sm = "HR")

meta_IPD.with.Rectum = Coefs.with.Rectum%>%
  metagen(TE = estimate,seTE =  std.error,data = .,byvar = KRAS,studlab = Study,sm = "HR")


meta_IPD.without.Rectum.interaction=Coefs.model.without.Rectum.Interaction.terms%>%
  metagen(TE = estimate,seTE =  std.error,data = .,studlab = Study,sm = "HR")
meta_IPD.with.Rectum.interaction=Coefs.with.Rectum.Interaction.terms%>%
  metagen(TE = estimate,seTE =  std.error,data = .,studlab = Study,sm = "HR")


forest(meta_IPD.without.Rectum, overall = F)
forest(meta_IPD.with.Rectum, overall = F)

forest(meta_IPD.without.Rectum.interaction)
forest(meta_IPD.with.Rectum.interaction)
