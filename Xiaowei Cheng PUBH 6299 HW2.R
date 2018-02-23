library(rmeta)
library(meta)
setwd("/users/xiaoweicheng/downloads")
TubelessData <- read.csv("tubeless data.csv", header = TRUE)
TubelessData
Duration <- TubelessData[TubelessData$Outcome=='duration',]
Duration
LOS <- TubelessData[TubelessData$Outcome=='LOS',]
LOS
Analgesic <- TubelessData[TubelessData$Outcome=='analgesic',]
Analgesic
Haematocrit <- TubelessData[TubelessData$Outcome=='haematocrit',]
Haematocrit
Duration.MA <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Duration,studlab=Study)
summary(Duration.MA)
FE.Duration <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Duration,studlab=Study,comb.random = FALSE)
summary(FE.Duration)
RE.Duration <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Duration,studlab=Study,comb.fixed = FALSE)
summary(RE.Duration)
LOS.MA <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=LOS,studlab=Study)
summary(LOS.MA)
FE.LOS <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=LOS,studlab=Study,comb.random = FALSE)
summary(FE.LOS)
RE.LOS <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=LOS,studlab=Study,comb.fixed = FALSE)
summary(RE.LOS)
Analgesic.MA <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Analgesic,studlab=Study)
summary(Analgesic.MA)
FE.Analgesic <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Analgesic,studlab=Study,comb.random = FALSE)
summary(FE.Analgesic)
RE.Analgesic <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Analgesic,studlab=Study,comb.fixed = FALSE)
summary(RE.Analgesic)
Haematocrit.MA <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Haematocrit,studlab=Study)
summary(Haematocrit.MA)
FE.Haematocrit <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Haematocrit,studlab=Study,comb.random = FALSE)
summary(FE.Haematocrit)
RE.Haematocrit <- metacont(n.e = n.E,mean.e = Mean.E,sd.e = SD.E,n.c = n.C,mean.c = Mean.C,sd.c = SD.C, data=Haematocrit,studlab=Study,comb.fixed = FALSE)
summary(RE.Haematocrit)
forest(Duration.MA)
forest(LOS.MA)
forest(Analgesic.MA)
forest(Haematocrit.MA)

