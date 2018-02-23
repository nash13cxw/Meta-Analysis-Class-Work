library(rmeta)
library(meta)
setwd("/users/xiaoweicheng/downloads")
Rosig <- read.csv("Rosig data.csv", header = TRUE)
Rosig
MI <- subset(Rosig, , -c(Death.TRT,Death.CTRL))
MI
Death <- subset(Rosig, , -c(MI.TRT,MI.CTRL))
Death
nrow(subset(MI,MI$MI.TRT==0 & MI$MI.CTRL==0))
nrow(subset(MI,MI$MI.TRT!=0 & MI$MI.CTRL==0))
nrow(subset(MI,MI$MI.TRT==0 & MI$MI.CTRL!=0))
nrow(subset(MI,MI$MI.TRT!=0 & MI$MI.CTRL!=0))
nrow(subset(MI,MI$MI.TRT==0))
nrow(subset(MI,MI$MI.CTRL==0))
MI.CC = MI
for (i in 1:56)
{
  if(MI$MI.CTRL[i]==0)
    MI.CC$n.TRT[i]=MI.CC$n.TRT[i]+1
    MI.CC$n.CTRL[i]=MI.CC$n.CTRL[i]+1
    MI.CC$MI.TRT[i]=MI.CC$MI.TRT[i]=0.5
    MI.CC$MI.CTRL[i]=MI.CC$MI.CTRL[i]=0.5
}
MI.CC
MI.P.CC <- metabin(event.e = MI.TRT,n.e = n.TRT,event.c = MI.CTRL,n.c = n.CTRL,studlab = Study,data=MI.CC,method = "P",allstudies = T)
MI.P.CC  
sum(MI.P.CC$w.fixed!=0)
MI.P <- metabin(event.e = MI.TRT,n.e = n.TRT,event.c = MI.CTRL,n.c = n.CTRL,studlab = Study,data=MI ,method = "P",allstudies = T,incr = 0)
MI.P
sum(MI.P$w.fixed!=0)
MI.MH.CC <- metabin(event.e = MI.TRT,n.e = n.TRT,event.c = MI.CTRL,n.c = n.CTRL,studlab = Study,data=MI.CC ,method = "MH",allstudies = T)
MI.MH.CC
sum(MI.MH.CC$w.fixed!=0)
MI.MH <- metabin(event.e = MI.TRT,n.e = n.TRT,event.c = MI.CTRL,n.c = n.CTRL,studlab = Study,data=MI ,method = "MH",allstudies = T,incr = 0)
MI.MH
sum(MI.MH$w.fixed!=0)
forest(MI.P.CC)
forest(MI.MH)
install.packages("exactmeta")
library(exactmeta)
dat <- cbind(MI$MI.CTRL,MI$MI.TRT,MI$n.CTRL,MI$n.TRT)
meta.exact(dat, type = "risk difference",BB.grdnum = 100,studyCI = FALSE)
nrow(subset(Death,Death$Death.TRT==0 & Death$Death.CTRL==0))
nrow(subset(Death,Death$Death.TRT!=0 & Death$Death.CTRL==0))
nrow(subset(Death,Death$Death.TRT==0 & Death$Death.CTRL!=0))
nrow(subset(Death,Death$Death.TRT!=0 & Death$Death.CTRL!=0))
nrow(subset(Death,Death$Death.TRT==0))
nrow(subset(Death,Death$Death.CTRL==0))
Death.CC = Death
for (i in 1:56)
{
  if(Death$Death.CTRL[i]==0)
    Death.CC$n.TRT[i]=Death.CC$n.TRT[i]+1
  Death.CC$n.CTRL[i]=Death.CC$n.CTRL[i]+1
  Death.CC$Death.TRT[i]=Death.CC$Death.TRT[i]=0.5
  Death.CC$Death.CTRL[i]=Death.CC$Death.CTRL[i]=0.5
}
Death.CC
Death.P.CC <- metabin(event.e = Death.TRT,n.e = n.TRT,event.c = Death.CTRL,n.c = n.CTRL,studlab = Study,data=Death.CC,method = "P",allstudies = T)
Death.P.CC  
sum(Death.P.CC$w.fixed!=0)
Death.P <- metabin(event.e = Death.TRT,n.e = n.TRT,event.c = Death.CTRL,n.c = n.CTRL,studlab = Study,data=Death ,method = "P",allstudies = T,incr = 0)
Death.P
sum(Death.P$w.fixed!=0)
Death.MH.CC <- metabin(event.e = Death.TRT,n.e = n.TRT,event.c = Death.CTRL,n.c = n.CTRL,studlab = Study,data=Death.CC ,method = "MH",allstudies = T)
Death.MH.CC
sum(Death.MH.CC$w.fixed!=0)
Death.MH <- metabin(event.e = Death.TRT,n.e = n.TRT,event.c = Death.CTRL,n.c = n.CTRL,studlab = Study,data=Death ,method = "MH",allstudies = T,incr = 0)
Death.MH
sum(Death.MH$w.fixed!=0)
forest(Death.P.CC)
forest(Death.MH)
install.packages("exactmeta")
library(exactmeta)
dat1 <- cbind(Death$Death.CTRL,Death$Death.TRT,Death$n.CTRL,Death$n.TRT)
meta.exact(dat1, type = "risk difference",BB.grdnum = 100,studyCI = FALSE)
