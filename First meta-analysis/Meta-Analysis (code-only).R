# right vs. left colon

Wild.Type.DF = data.frame(
  
  Authors       = c("20050181",   #1
                    "20050181",   #2
                    "CRYSTAL",    #3
                    "CRYSTAL",    #4
                    "FIRE-3",     #5
                    "FIRE-3",     #6
                    "CALGB 80405",#7
                    "CALGB 80405",#8
                    "PRIME",      #9
                    "PRIME",      #10
                    "PEAK",       #11
                    "PEAK",       #12
                    "AIO KRK0104",#13
                    "TRIBE",      #14
                    "AIO KRK0207",#15
                    "JACCRO CC-05/06", #16
                    "MACRO-2+PLANET",#17
                    "OPTIMOX3 DREAM",#18
                    "Demurtas",   #19
                    "Kamran",     #20
                    "Fiala",      #21
                    "Fiala",      #22
                    "Uhlyarik",   #23
                    "CAIRO-3"     #24
  ), 
  `Study design` =c("RCT",           #1
                    "RCT",           #2
                    "RCT",           #3
                    "RCT",           #4
                    "RCT",           #5
                    "RCT",           #6
                    "RCT",           #7
                    "RCT",           #8
                    "RCT",           #9
                    "RCT",           #10
                    "RCT",           #11
                    "RCT",           #12
                    "RCT",           #13
                    "RCT",           #14
                    "RCT",           #15
                    "RCT",           #16
                    "RCT",           #17
                    "RCT",           #18
                    "Observational", #19                  
                    "Observational", #20
                    "Observational", #21
                    "Observational", #22
                    "Observational", #23
                    "RCT"            #24
  ),
  `Treatment`     =c("Panitumumab + FOLFIRI",                  #1
                    "FOLFIRI",                                #2
                    "FOLFIRI",                                #3
                    "FOLFIRI + cetuximab",                    #4
                    "FOLFIRI + bevacizumab",                  #5
                    "FOLFIRI + cetuximab",                    #6
                    "FOLFIRI + bevacizumab",                  #7
                    "FOLFIRI + cetuximab",                    #8
                    "FOLFOX4",                                #9
                    "FOLFOX4 + panitumumab",                  #10
                    "FOLFOX6 + bevacizumab",                  #11
                    "FOLFOX6 + panitumumab",                  #12
                    "Cetuximab +  CAPIRI or CAPOX",           #13
                    "FOLFIRI + bevacizumab",                  #14
                    "FOLFIRI + bevacizumab",                  #15
                    "FOLFIRI + cetuximab",                    #16
                    "FOLFIRI + panitumumab/cetuximab",        #17
                    "Chemotherapy + anti-EGFR/anti-VEGF",     #18
                    "Chemotherapy + anti-EGFR/anti-VEGF",     #19
                    "Chemotherapy + anti-EGFR/anti-VEGF",     #20
                    "FOLFIRI + cetuximab/panitumumab",        #21
                    "FOLFIRI + bevacizumab",                  #22
                    "FOLFIRI + cetuximab",                    #23
                    "CAPOX-B + cetuximab"                     #24
                    
  ),
  `Subgroup Treatment`=  c("Chemotherapy + anti-EGFR",        #1
                           "Chemotherapy alone",                     #2
                           "Chemotherapy alone",                     #3
                           "Chemotherapy + anti-EGFR",               #4
                           "Chemotherapy + anti-VEGF",               #5
                           "Chemotherapy + anti-EGFR",               #6
                           "Chemotherapy + anti-VEGF",               #7
                           "Chemotherapy + anti-EGFR",               #8
                           "Chemotherapy alone",                     #9
                           "Chemotherapy + anti-EGFR",               #10
                           "Chemotherapy + anti-VEGF",               #11
                           "Chemotherapy + anti-EGFR",               #12
                           "Chemotherapy + anti-EGFR",               #13
                           "Chemotherapy + anti-VEGF",               #14
                           "Chemotherapy + anti-VEGF",               #15
                           "Chemotherapy + anti-EGFR",               #16
                           "Chemotherapy + anti-EGFR",               #17
                           "Mixed therapies",                        #18
                           "Mixed therapies",                        #19
                           "Mixed therapies",                        #20
                           "Chemotherapy + anti-EGFR",               #21
                           "Chemotherapy + anti-VEGF",               #22
                           "Chemotherapy + anti-EGFR",               #23
                           "Chemotherapy + anti-EGFR"                #24
  ), 
  
  `KRAS status`= rep("Wild Type", 24),   
  ##############   1    2       3       4      5     6     7     8     9    10   11     12    13    14     15
  `Effect size`=c(2.01,  1.51,  1.35,  1.93,  1.48, 2.84, 1.14, 1.82, 1.27, 1.58, 2.86,  2.68, 2.38, 1.72, 1.56,
                  
              ###  16     17     18     19    20    #?21    22   23   24
                  3.57,  2.00,  1.52,  2.83,  2.04, 2.29, 1.71, 2.01, 1.12),  
  `Lower_CI` =  c(1.29,  0.96,  0.93,  1.24,  1.02, 1.86, 0.80, 1.27, 0.88, 1.02, 1.40,  1.31,  1.49, 1.03,  1.06,
                  1.92,  1.42,  1.10,  1.40,  1.44, 1.46, 0.97, 1.40, 0.66),
  `Upper_CI` =  c(3.13,  2.37,  1.97,  2.99,  2.16, 4.33, 1.61,  2.56, 1.83, 2.45, 5.84,  5.46,  4.00, 2.85, 2.31, 
                  6.66,  3.33,  2.10,  5.60,  2.94, 3.62, 2.99, 3.16, 1.89),
  `Number of patients` = 
    #  1        2     3     4      5     6     7     8     9    10    11   12   13  14   15   16  17
    c(181,   187,  189,   175,   199 ,  195,  230,  244,  208,  208,  68,  75,  95, 124, 209, 110,261,
      # 18    19    20  21   22  23   24
      297,  88,  224, 108, 69, 97, 133 ), 
  #  1     2     3     4      5     6    7      8    9      10    11    12     13    14   15     16   
  Year =        c(2014, 2014,  2009, 2009, 2014, 2014, 2014, 2014, 2010,  2010, 2014, 2014, 2014,2014, 2015, 2016,
                  #17           18    19    20    21    22    23    24
                  "2014-2017", 2015, 2017, 2018, 2019, 2019, 2018, 2017 ),
  
  check.names = FALSE)



### log-transform hazard ratios and compute standard error based on the
Wild.Type.DF$logyi  <- log(Wild.Type.DF$`Effect size`) 
Wild.Type.DF$logsei <- with(Wild.Type.DF, log(`Upper_CI`) - log(`Lower_CI`)) / (2*1.96)

library(meta)
Wild.Type.meta.res <- metagen(data =Wild.Type.DF, TE = logyi, seTE =logsei,
                              studlab = Wild.Type.DF$Authors,comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                              backtransf = T, hakn = T ) 


pdf("First meta-analysis/Figures/Figure 1.pdf", height = 10,10)

forest(Wild.Type.meta.res, prediction = T, overall = T, 
       leftcols =     c("studlab","TE","seTE","ci"), 
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =T, print.I2.ci = T, 
       print.tau2 = T, print.Q = T, print.pval.Q = T, 
       smlab = "Hazard ratio")
library(grid)
grid.text("Meta-analysis of right versus left (studies with wild type patients)", .5, .9, gp=gpar(cex=2))
dev.off()

pdf("First meta-analysis/Figures/Figure 1.1.pdf", height = 13,13)


funnel(Wild.Type.meta.res,comb.fixed = F,comb.random = T, 
       #text = Wild.Type.meta.res$studlab,
       pch = 1,
       cex = Wild.Type.meta.res$w.random/10,
       contour.levels = 0.95,ref.triangle = T,
       ref = exp(Wild.Type.meta.res$TE.random),
       studlab = T,pos.studlab = 1,cex.studlab = 0.7,
       lty.ref.triangle = 1,backtransf = T, lty.ref = 2
       )

abline(v = 1)

grid.text("Funnel-plot of meta-analysis of right versus left (studies with wild type patients)", .5, .92, gp=gpar(cex=2))

dev.off()

metabias(Wild.Type.meta.res)
metainf(Wild.Type.meta.res, "fixed")




library(meta)
Wild.Type.meta.res.by.sub <- metagen(data =Wild.Type.DF, TE = logyi, seTE =logsei,byvar = `Subgroup Treatment`,
                                     studlab = Wild.Type.DF$Authors,comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                     backtransf = T, hakn = T ) 


pdf("First meta-analysis/Figures/Figure 2.pdf", height = 15,10)

forest(Wild.Type.meta.res.by.sub, prediction = F, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"),subgroup = T,
       type.subgroup.fixed = "diamond",type.subgroup.random = "diamond",
       print.subgroup.labels = T,test.effect.subgroup.fixed = T,
       test.effect.subgroup.random = T, test.effect.subgroup = T,
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = F, print.Q = F, print.pval.Q = F, 
       smlab = "Hazard ratio")


library(grid)
grid.text("Meta-analysis of right versus left (studies with wild type patients)", .5, .9, gp=gpar(cex=2))
grid.text("stratified by type of treatment", .5, 0.88, gp=gpar(cex=2))


dev.off()




Wild.Type.meta.res.RCT.only <- metagen(data =Wild.Type.DF[Wild.Type.DF$`Study design`=="RCT",], 
                                       TE = logyi, seTE =logsei,
                                       studlab = Wild.Type.DF[Wild.Type.DF$`Study design`=="RCT",]$Authors,
                                       comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                       backtransf = T, hakn = T ) 

pdf("First meta-analysis/Figures/Figure 3.pdf", height = 10,10)

forest(Wild.Type.meta.res.RCT.only, prediction = T, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"),
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = T, print.Q = T, print.pval.Q = T, 
       smlab = "Hazard ratio")


grid.text("Meta-analysis of right versus left (studies with wild type patients)", .5, .9, gp=gpar(cex=2))
grid.text("Only RCTs", .5, 0.87, gp=gpar(cex=2))


dev.off()



pdf("First meta-analysis/Figures/Figure 3.1.pdf", height = 13,13)


funnel(Wild.Type.meta.res.RCT.only,
       comb.fixed = F,comb.random = T, 
       pch = 1,
       cex = Wild.Type.meta.res$w.random/10,
       contour.levels = 0.95,ref.triangle = T,
       ref = exp(Wild.Type.meta.res$TE.random),
       studlab = T,pos.studlab = 1,cex.studlab = 0.7,
       lty.ref.triangle = 1,backtransf = T, lty.ref = 2
)

abline(v = 1)

grid.text("Funnel-plot of meta-analysis of right versus left (studies with wild type patients)", .5, .92, gp=gpar(cex=2))
grid.text("Only RCTs", .5, 0.9, gp=gpar(cex=2))

dev.off()

metabias(Wild.Type.meta.res.RCT.only)
metainf(Wild.Type.meta.res.RCT.only, "fixed")


Wild.Type.meta.res.by.sub.RCT.only <- metagen(data =Wild.Type.DF[Wild.Type.DF$`Study design`=="RCT",], TE = logyi, seTE =logsei,byvar = `Subgroup Treatment`,
                                              studlab = Wild.Type.DF[Wild.Type.DF$`Study design`=="RCT",]$Authors,comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                              backtransf = T, hakn = T ) 

pdf("First meta-analysis/Figures/Figure 4.pdf", height = 15,10)

forest(Wild.Type.meta.res.by.sub.RCT.only, prediction = F, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"),subgroup = T,
       type.subgroup.fixed = "diamond",type.subgroup.random = "diamond",
       print.subgroup.labels = T,test.effect.subgroup.fixed = T,
       test.effect.subgroup.random = T, test.effect.subgroup = T,
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = F, print.Q = F, print.pval.Q = F, 
       smlab = "Hazard ratio")


library(grid)
grid.text("Meta-analysis of right versus left (studies with wild type patients)", .5, .9, gp=gpar(cex=2))
grid.text("stratified by type of treatment (only RCTs)", .5, 0.88, gp=gpar(cex=2))


dev.off()



sum(Wild.Type.DF$`Number of patients`)

median(as.numeric(Wild.Type.DF$Year))

######################## RAS Mutated





RAS.mutated.meta = data.frame(
  Authors =     
    c("PRIME",            #1
      "PRIME",            #2   
      "PEAK",             #3
      "PEAK",             #4
      "20050181",         #5
      "20050181",         #6
      "AIO KRK0104",      #7
      "Loupakis",         #8
      "TRIBE",            #9
      "Kamran",           #10
      "OPTIMOX3 DREAM",   #11
      "CAIRO-3",          #12
      "AIO KRK0207"       #13
    ),
  `Study design` =
    c("RCT",           #1
      "RCT",           #2
      "RCT",           #3
      "RCT",           #4
      "RCT",           #5
      "RCT",           #6
      "RCT",           #7
      "Observational", #8
      "RCT",           #9
      "Observational", #10
      "RCT",           #11
      "RCT",           #12
      "RCT"            #13
    ),
  `Treatment`     =c("FOLFOX + Panitumumab ",       #1
                    "FOLFOX",                      #2
                    "FOLFOX + Panitumumab",        #3
                    "FOLFOX + Bevacizumab",        #4
                    "FOLFIRI+ Panitumumab",        #5
                    "FOLFIRI",                     #6
                    "CAPIRI or CAPOX + Cetuximab", #7
                    NA,                            #8
                    "FOLFOX+ Bevacizumab",         #9
                    "FOLFORI",                     #10
                    "Mixed therapies",             #11
                    "CAPOX-B + cetuximab",         #12
                    "FOLFIRI + bevacizumab"        #13
                    
  ),
  `Subgroup Treatment`=  c("Chemotherapy + anti-EGFR",               #1
                           "Chemotherapy alone",                     #2
                           "Chemotherapy + anti-EGFR",               #3
                           "Chemotherapy + anti-VEGF",               #4
                           "Chemotherapy + anti-EGFR",               #5
                           "Chemotherapy alone",                     #6
                           "Chemotherapy + anti-EGFR",               #7
                           NA,                                       #8
                           "Chemotherapy + anti-VEGF",               #9
                           "Chemotherapy alone",                     #10
                           "Mixed therapies",                        #11
                           "Chemotherapy + anti-EGFR",               #12
                           "Chemotherapy + anti-VEGF"                #13
  ),
  `KRAS status` = rep("RAS mutation", 13),
  `Effect size`=
    #   1     2     3     4      5     6      7      8      9      10     11    12     13 
    c(1.17, 1.09, 2.24, 2.8,   0.84, 1.46,  1.3,   0.95,  1.19, 1.63,  1.56, 1.31,  1.06),
  `Lower_CI` =  
    c(0.85, 0.81, 0.87, 1.05,  0.63, 1.09,  0.68,  0.72, 0.85,  1.05,  1.10, 0.98,  0.81),
  `Upper_CI` =  
    c(1.61, 1.48, 5.78, 7.43,  1.11, 1.96,  2.34,  1.46, 1.65,  2.5,  2.21,  1.76,  1.4),
  `Number of patients` = 
    c(230,  228,  25,   29,    259,  260,   51,    494,   209,  143,   239,  233,   276), 
  Year =  
    c(2010, 2010, 2014, 2014,  2014, 2014,  2014, 2018,  2014,  2018,  2015, 2017,  2015),
  check.names = FALSE)


sum(RAS.mutated.meta$`Number of patients`)
median(RAS.mutated.meta$Year)

### log-transform hazard ratios and compute standard error based on the
RAS.mutated.meta$logyi  <- log(RAS.mutated.meta$`Effect size`) 
RAS.mutated.meta$logsei <- with(RAS.mutated.meta, log(`Upper_CI`) - log(`Lower_CI`)) / (2*1.96)

library(meta)
RAS.mutated.meta.res <- metagen(data =RAS.mutated.meta, TE = logyi, seTE =logsei,
                                studlab = RAS.mutated.meta$Authors,comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                backtransf = T, hakn = T ) 


pdf("First meta-analysis/Figures/Figure 5.pdf", height = 15,10)

forest(RAS.mutated.meta.res, prediction = T, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"),
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = T, print.Q = T, print.pval.Q = T, 
       smlab = "Hazard ratio")


grid.text("Meta-analysis of right versus left (studies with KRAS mutated patients)", .5, .9, gp=gpar(cex=2))



dev.off()

pdf("First meta-analysis/Figures/Figure 5.1.pdf", height = 15,10)

funnel(RAS.mutated.meta.res,
       comb.fixed = F,comb.random = T, 
       pch = 1,
       cex = RAS.mutated.meta.res$w.random/10,
       contour.levels = 0.95,ref.triangle = T,
       ref = exp(RAS.mutated.meta.res$TE.random),
       studlab = T,pos.studlab = 1,cex.studlab = 0.7,
       lty.ref.triangle = 1,backtransf = T, lty.ref = 2
)

abline(v = 1)

grid.text("Funnel-plot of meta-analysis of right versus left (studies with KRAS mutated patients)", .5, .92, gp=gpar(cex=2))

dev.off()

metabias(RAS.mutated.meta.res)
metainf(RAS.mutated.meta.res, "fixed")


RAS.mutated.meta.res.by.sub <- metagen(data =RAS.mutated.meta[!is.na(RAS.mutated.meta$`Subgroup Treatment`),], 
                                       TE = logyi, seTE =logsei,byvar = `Subgroup Treatment`,
                                       studlab = RAS.mutated.meta[!is.na(RAS.mutated.meta$`Subgroup Treatment`),]$Authors,
                                       comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                       backtransf = T) 





pdf("First meta-analysis/Figures/Figure 6.pdf", height = 15,10)

forest(RAS.mutated.meta.res.by.sub, prediction = T, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"),
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = T, print.Q = T, print.pval.Q = T, 
       smlab = "Hazard ratio")


grid.text("Meta-analysis of right versus left (studies with KRAS mutated patients)", .5, .9, gp=gpar(cex=2))
grid.text("stratified by type of treatment", .5, 0.88, gp=gpar(cex=2))



dev.off()


########### Interaction terms analysis

RAS.mutated.meta$Year =  as.factor(RAS.mutated.meta$Year)



Wild.Type.DF2= Wild.Type.DF%>%
  select(-c(`Treatment`,`KRAS status`,`Study design`))

RAS.mutated.meta2= RAS.mutated.meta%>%
  select(-c(`Treatment`,`KRAS status`,`Study design`))





Interaction.DF =  left_join(suffix = c( " KRAS-mutated", " Wild type"),by = c("Authors","Year","Subgroup Treatment"),
                            RAS.mutated.meta2,
                            Wild.Type.DF2 )



Interaction.DF= Interaction.DF[complete.cases(Interaction.DF),]

Interaction.DF$log.Interaction = Interaction.DF$`logyi Wild type` - Interaction.DF$`logyi KRAS-mutated`

Interaction.DF$log.Interaction.se =  sqrt(Interaction.DF$`logsei Wild type`^2 + Interaction.DF$`logyi KRAS-mutated`^2)



#######################


library(meta)
Interaction.DF.res <- metagen(data =Interaction.DF, TE = log.Interaction, seTE =log.Interaction.se,
                                studlab = Interaction.DF$Authors,comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                backtransf = T, hakn = T ) 


pdf("First meta-analysis/Figures/Figure 7.pdf", height = 10,10)

forest(Interaction.DF.res, prediction = T, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"),
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect","w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = T, print.Q = T, print.pval.Q = T, 
       smlab = "Hazard ratio")


grid.text("Meta-analysis of interaction terms", .5, .9, gp=gpar(cex=2))



dev.off()



pdf("First meta-analysis/Figures/Figure 7.1.pdf", height = 13,13)


funnel(Interaction.DF.res,
       comb.fixed = F,comb.random = T, 
       pch = 1,
       cex = Interaction.DF.res$w.random/10,
       contour.levels = 0.95,ref.triangle = T,
       ref = exp(Interaction.DF.res$TE.random),
       studlab = T,pos.studlab = 1,cex.studlab = 0.7,
       lty.ref.triangle = 1,backtransf = T, lty.ref = 2
)

abline(v = 1)

grid.text("Funnel-plot of meta-analysis of interaction terms", .5, .91, gp=gpar(cex=2))


dev.off()

metabias(Interaction.DF.res)
metainf(Interaction.DF.res, "fixed")


Interaction.DF.res.by.sub <- metagen(data =Interaction.DF, 
                                       TE = log.Interaction, seTE =log.Interaction.se,
                                       byvar = `Subgroup Treatment`,
                                       studlab = Interaction.DF$Authors,
                                       comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                                       backtransf = T) 





pdf("First meta-analysis/Figures/Figure 8.pdf", height = 12,10)

forest(Interaction.DF.res.by.sub, prediction = T, overall = T, comb.fixed = T,comb.random = T,
       leftcols =     c("studlab","TE","seTE","ci"), backtransf = T,
       leftlabs =     c("Author",	"logHR",	"logHR (SE)","95% CI"), 
       rightcols =    c("effect", "w.fixed",	"w.random"), 
       rightlabs =    c("95% CI"), 
       print.I2 =F, print.I2.ci = F, print.tau2 = T, print.Q = T, print.pval.Q = T, 
       smlab = "Hazard ratio")


grid.text("Meta-analysis of interaction terms", .5, .9, gp=gpar(cex=2))
grid.text("stratified by type of treatment", .5, 0.88, gp=gpar(cex=2))



dev.off()



Combined.meta= full_join(Wild.Type.DF[,-c(names(Wild.Type.DF) == c("Subgroup Treatment"))],
                         RAS.mutated.meta[,-c(names(RAS.mutated.meta) == c("Subgroup Treatment"))])



### log-transform hazard ratios and compute standard error based on the
Combined.meta$logyi  <- log(Combined.meta$`Effect size`) 
Combined.meta$logsei <- with(Combined.meta, log(`Upper_CI`) - log(`Lower_CI`)) / (2*1.96)

Combined.meta.res <- metagen(data =Combined.meta, TE = logyi, seTE =logsei,byvar = `KRAS status`,
                             studlab = Combined.meta$Authors,comb.fixed = T,comb.random = T,method.tau = "EB",sm="HR", 
                             backtransf = T ) 

forest(Combined.meta.res)

