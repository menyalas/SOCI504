setwd("~/Backup/SOCI504 Replication")

dfSm<- read.delim("20181110_AM_Glick.tsv", encoding="UTF-8")

install.packages("weights")
library(weights)

#Histograms
#MATH ROUTING #RIGHT -GR3 ASSMT
hist(dfSm$C5R4MNR3)
#MATH IRT SCALE SCORE
hist(dfSm$C5R4MSCL)
#MATH T-SCORE
hist(dfSm$C5R4MTSC)
#Math THETA
hist(dfSm$C5R4MTHT)
#MATH STANDARD ERROR OF THETA 
hist(dfSm$C5R4MSET)

#Grade 3 MATH ROUTING #RIGHT -GR3 ASSMT
allGroups<-wtd.mean(dfSm$C5R4MSCL, weight=dfSm$BYCW0, na.rm=TRUE)

#MATH ROUTING #RIGHT -GR3 ASSMT by group

raceNH.w<-subset(dfSm, RACE==1, select = c(C5R4MSCL, BYCW0), TRUE)
raceNH.w<-wtd.mean(raceNH.w$C5R4MSCL, weight=raceNH.w$BYCW0)

raceNH.b<-subset(dfSm, RACE==2, select = c(C5R4MSCL, BYCW0), TRUE)
raceNH.b<-wtd.mean(raceNH.b$C5R4MSCL, weight=raceNH.b$BYCW0)

raceHis.m<-subset(dfSm, P4HISPBD==1, select = c(C5R4MSCL, BYCW0), TRUE)
raceHis.m<-wtd.mean(raceHis.m$C5R4MSCL, weight=raceHis.m$BYCW0)

raceHis.pr<-subset(dfSm, P4HISPBD==2, select = c(C5R4MSCL, BYCW0), TRUE)
raceHis.pr<-wtd.mean(raceHis.pr$C5R4MSCL, weight=raceHis.pr$BYCW0)

raceHis.o<-subset(dfSm, P4HISPBD==3 | P4HISPBD==4 | P4HISPBD==-7 | P4HISPBD==-8, select = c(C5R4MSCL, BYCW0), TRUE)
raceHis.o<-wtd.mean(raceHis.o$C5R4MSCL, weight=raceHis.o$BYCW0)

raceAsi<-subset(dfSm, RACE==5, select = c(C5R4MSCL, BYCW0), TRUE)
raceAsi<-wtd.mean(raceAsi$C5R4MSCL, weight=raceAsi$BYCW0)

racePI<-subset(dfSm, RACE==6, select = c(C5R4MSCL, BYCW0), TRUE)
racePI<-wtd.mean(racePI$C5R4MSCL, weight=racePI$BYCW0)

raceAI<-subset(dfSm, RACE==7, select = c(C5R4MSCL, BYCW0), TRUE)
raceAI<-wtd.mean(raceAI$C5R4MSCL, weight=raceAI$BYCW0)

raceM<-subset(dfSm, RACE==8, select = c(C5R4MSCL, BYCW0), TRUE)
raceM<-wtd.mean(raceM$C5R4MSCL, weight=raceM$BYCW0)

engFail<-subset(dfSm, C1SCTOT >= 0 & C1SCTOT < 37, select = c(C5R4MSCL, BYCW0), TRUE)
engFail<-wtd.mean(engPass$C5R4MSCL, weight=engPass$BYCW0)

engPass<-subset(dfSm, C1SCTOT == -1 | C1SCTOT >= 37 | is.na(C1SCTOT), select = c(C5R4MSCL, BYCW0), TRUE)
engPass<-wtd.mean(engFail$C5R4MSCL, weight=engFail$BYCW0)

genOne.5<-subset(dfSm, P2CHPLAC==2, select = c(C5R4MSCL, BYCW0), TRUE)
genOne.5<-wtd.mean(genOne.5$C5R4MSCL, weight=genOne.5$BYCW0)

genTwo.aft<-subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB > 1 | P4DADCOB > 1 | P5MOMCOB > 1 | P5DADCOB > 1) & c(P4MAGEUS >= 15 | P4DAGEUS >= 15 | P5MAGEUS >= 15 | P5DAGEUS >= 15), select = c(C5R4MSCL, BYCW0), TRUE)
genTwo.aft<-wtd.mean(genTwo.aft$C5R4MSCL, weight=genTwo.aft$BYCW0)

genTwo.pre<-subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB > 1 | P4DADCOB > 1 | P5MOMCOB > 1 | P5DADCOB > 1) & c(P4MAGEUS < 15 | P4DAGEUS < 15 | P5MAGEUS < 15 | P5DAGEUS < 15), select = c(C5R4MSCL, BYCW0), TRUE)
genTwo.pre<-wtd.mean(genTwo.pre$C5R4MSCL, weight=genTwo.pre$BYCW0)

genThree<-subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB == 1 | P4DADCOB == 1 | P5MOMCOB == 1 | P5DADCOB == 1) & c(P4MOMCOB != 2 & P4DADCOB != 2 & P5MOMCOB != 2 & P5DADCOB != 2), select = c(C5R4MSCL, BYCW0), TRUE)
genThree<-wtd.mean(genThree$C5R4MSCL, weight=genThree$BYCW0)

#Make a table

mathIRT.C5R4MSCL<-rbind(allGroups, raceNH.w, raceNH.b, raceHis.m, raceHis.pr, raceHis.o, raceAsi, racePI, raceAI, raceM, engPass, engFail, genOne.5, genTwo.aft, genTwo.pre, genThree)
colnames(mathIRT.C5R4MSCL)<-"IRT.C5R4MSCL"

mathT.C5R4MTSC<-rbind(allGroups, raceNH.w, raceNH.b, raceHis.m, raceHis.pr, raceHis.o, raceAsi, racePI, raceAI, raceM, engPass, engFail, genOne.5, genTwo.aft, genTwo.pre, genThree)
colnames(mathT.C5R4MTSC)<-"T.C5R4MTSC"

mathRouting.C5R4MNR3<-rbind(allGroups, raceNH.w, raceNH.b, raceHis.m, raceHis.pr, raceHis.o, raceAsi, racePI, raceAI, raceM, engPass, engFail, genOne.5, genTwo.aft, genTwo.pre, genThree)
colnames(mathRouting.C5R4MNR3)<-"Routing.C5R4MTSC"

#Don't run this -- it was to make the comparison table. 

mathThetas.BYCPTW0<-rbind(allGroups, raceNH.w, raceNH.b, raceHis.m, raceHis.pr, raceHis.o, raceAsi, racePI, raceAI, raceM, engPass, engFail, genOne.5, genTwo.aft, genTwo.pre, genThree)
colnames(mathThetas.BYCPTW0)<-"BYCPTW0"

math<-cbind(mathIRT.C5R4MSCL, mathT.C5R4MTSC, mathRouting.C5R4MNR3)

write.table(mathThetas, file = "20181111_AM_mathScores.tsv", sep = "\t")
