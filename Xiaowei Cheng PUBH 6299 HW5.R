library(rmeta)
library(meta)
setwd("/users/xiaoweicheng/downloads")
hw5 <- read.csv("hw5 data.csv", header = TRUE)
hw5
nrow(subset(hw5,hw5$death.beta==0 & hw5$death.c==0))
nrow(subset(hw5,hw5$death.beta!=0 & hw5$death.c==0))
nrow(subset(hw5,hw5$death.beta==0 & hw5$death.c!=0))
nrow(subset(hw5,hw5$death.beta!=0 & hw5$death.c!=0))
nrow(subset(hw5,hw5$death.beta==0))
nrow(subset(hw5,hw5$death.c==0))
hw5.blinding.mh=metabin(event.e = death.beta,n.e = n.beta,event.c = death.c,n.c = n.c, studlab = trial,data = hw5,method = "MH",sm="RR",byvar = blinding,allstudies = T)
hw5.blinding.mh.nocc=metabin(event.e = death.beta,n.e = n.beta,event.c = death.c,n.c = n.c, studlab = trial,data = hw5,method = "MH",sm="RR",byvar = blinding,incr = 0,allstudies = T)
hw5.blinding.mh
hw5.blinding.mh.nocc
forest(hw5.blinding.mh)
forest(hw5.blinding.mh.nocc)
c.length <- hw5$ave.FollowUp
hw5.mh <- metabin(event.e = death.beta, n.e = n.beta,event.c = death.c,n.c = n.c,studlab = trial,data = hw5,method = "MH",sm="RR")
install.packages("metafor")
library("metafor")
hw5.mh.regl <- metareg(hw5.mh,~c.length)
hw5.mh.regl
c.bliding <- hw5$blinding
hw5.mh.regl.blinding <- metareg(hw5.mh,~c.bliding)
hw5.mh.regl.blinding
hw5.noblinding <- ifelse(hw5$blinding == "nobliding",1,0)
hw5.mh.regl.noblinding <- metareg(hw5.mh,~hw5.noblinding)
hw5.cum.length <- metacum(hw5.mh,pooled = 'random',sortvar = ave.FollowUp)
hw5.cum.length
forest(hw5.cum.length)
