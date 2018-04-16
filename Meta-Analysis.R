R code
library(rmeta)
library(meta)
setwd("/users/xiaoweicheng/desktop")
Figure <- read.csv("Figure.csv", header = TRUE)
Figure
Figure1 <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total,studlab = Source,data=Figure)
Figure1
Figure1.P.CC <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total,studlab = Source,data=Figure,method = "P",allstudies = T)
Figure1.P.CC
Figure1.P <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total,studlab = Source,data=Figure,method = "P",allstudies = T,incr = 0)
Figure1.P
Figure1.MH.CC <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total,studlab = Source,data=Figure,method = "MH",allstudies = T)
Figure1.MH.CC
Figure1.MH <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total,studlab = Source,data=Figure,method = "MH",allstudies = T,incr = 0)
Figure1.MH
forest(Figure1)
forest(Figure1.P.CC)
forest(Figure1.MH)
nrow(subset(Figure,Figure$Bevacizumab.events==0 & Figure$Control.event==0))
nrow(subset(Figure,Figure$Bevacizumab.events!=0 & Figure$Control.event==0))
nrow(subset(Figure,Figure$Bevacizumab.events==0 & Figure$Control.event!=0))
nrow(subset(Figure,Figure$Bevacizumab.events!=0 & Figure$Control.event!=0))
nrow(subset(Figure,Figure$Bevacizumab.events==0))
nrow(subset(Figure,Figure$Control.event==0))
funnel(Figure1, comb.fixed=FALSE, comb.random=FALSE,studlab=FALSE, backtransf=TRUE)
funnel(Figure1.P.CC, comb.fixed=FALSE, comb.random=FALSE,studlab=FALSE, backtransf=TRUE)
funnel(Figure1.MH, comb.fixed=FALSE, comb.random=FALSE,studlab=FALSE, backtransf=TRUE)
Figure2 <- read.csv("Figure2.csv",header = TRUE)
Figure2
Figure2.Tumor.mh <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total, studlab = Source,data = Figure2,method = "MH",sm="RR",byvar = Underlying.Malignancy,allstudies = T)
Figure2.Tumor.mh
c.dose <- Figure2$Dose
c.dose[Figure2$Dose==1]= "low dose"
c.dose[Figure2$Dose==2]= "medium dose"
c.dose[Figure2$Dose==3]= "high dose"
c.dose
Figure2.dose.mh <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total, studlab = Source,data = Figure2,method = "MH",sm="RR",byvar = c.dose,allstudies = T)
Figure2.dose.mh
Figure2.MH <- metabin(event.e = Bevacizumab.events,n.e = Bevacizumab.total,event.c = Control.event,n.c = Control.Total, studlab = Source,data = Figure2,method = "MH",sm="RR",allstudies = T)
Figure2.MH
TumorType <- Figure2$Underlying.Malignancy
Figure2.Tumor.reg <- metareg(Figure2.MH,~TumorType)
Figure2.Tumor.reg
Figure2.dose.reg <- metareg(Figure2.MH,~c.dose)
Figure2.dose.reg
Figure1.bias.rank <- metabias(Figure1,method.bias = "rank",k.min = 5)
Figure1.bias.rank
Figure1.bias.linreg <- metabias(Figure1,method.bias = "linreg",k.min = 5)
Figure1.bias.linreg
Figure1.P.bias.rank <- metabias(Figure1.P,method.bias = "rank",k.min = 5)
Figure1.P.bias.rank
Figure1.MH.bias.rank <- metabias(Figure1.MH,method.bias = "rank",k.min = 5)
Figure1.MH.bias.rank
forest(Figure2.Tumor.mh)
forest(Figure2.dose.mh)
