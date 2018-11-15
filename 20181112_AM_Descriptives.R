setwd("~/Backup/SOCI504 Replication")

dfSm <- read.delim("20181111_AM_Glick.tsv", encoding="UTF8")

install.packages("weights")
library(weights)

#age
naAge<-sum(is.na(dfSm$R2_KAGE))
age<-subset(dfSm, R2_KAGE >= 0, select = c(R2_KAGE, BYCW0))
hist(age$R2_KAGE)
sd(age$R2_KAGE)
wtd.mean(age$R2_KAGE, weight=age$BYCW0)

#gender
wpct(dfSm$GENDER, weight=dfSm$BYCW0, na.rm=TRUE)

#Ethnicity
naEth<-sum(is.na(dfSm$RACE))
wpct(dfSm$RACE, weight=dfSm$BYCW0, na.rm=TRUE)
table(dfSm$RACE)
length(dfSm$RACE)

naHis<-sum(is.na(dfSm$P4HISPBD))
summary(dfSm$P4HISPBD)
table(dfSm$P4HISPBD)
wpct(dfSm$P4HISPBD, weight=dfSm$BYCW0, na.rm = FALSE)

#Generation Status - NEED WEIGHTS APPLIED
one.five<-nrow(subset(dfSm, P2CHPLAC==2))
two.aft<-nrow(subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB > 1 | P5MOMCOB > 1 | P4DADCOB > 1 | P5DADCOB > 1) & c(P4MAGEUS > 15 | P5MAGEUS > 15 | P4DAGEUS > 15 | P5DAGEUS > 15)))
two.pre<-nrow(subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB > 1 | P5MOMCOB > 1 | P4DADCOB > 1 | P5DADCOB > 1) & c(c(P4MAGEUS <= 15 & P4MAGEUS > -1) | c(P5MAGEUS <= 15 & P5MAGEUS > -1) | c(P4DAGEUS <= 15 & P4DAGEUS > -1) | c(P5DAGEUS <= 15 & P5DAGEUS > -1))))
three<-nrow(subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB == 1 | P5MOMCOB == 1) & c(P4MOMCOB < 2 & P5MOMCOB < 2)))
unknown<-nrow(subset(dfSm, c(P2CHPLAC < 0 | is.na(P2CHPLAC)) | c(P4MOMCOB < 0 & P5MOMCOB < 0) | c(is.na(P4MOMCOB) & is.na(P5MOMCOB))))
total<-one.five+two.aft+two.pre+three+unknown

one.five<-nrow(subset(dfSm, P2CHPLAC==2)) / total
two.aft<-nrow(subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB > 1 | P5MOMCOB > 1 | P4DADCOB > 1 | P5DADCOB > 1) & c(P4MAGEUS > 15 | P5MAGEUS > 15 | P4DAGEUS > 15 | P5DAGEUS > 15))) / total
two.pre<-nrow(subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB > 1 | P5MOMCOB > 1 | P4DADCOB > 1 | P5DADCOB > 1) & c(c(P4MAGEUS <= 15 & P4MAGEUS > -1) | c(P5MAGEUS <= 15 & P5MAGEUS > -1) | c(P4DAGEUS <= 15 & P4DAGEUS > -1) | c(P5DAGEUS <= 15 & P5DAGEUS > -1)))) / total
three<-nrow(subset(dfSm, P2CHPLAC==1 & c(P4MOMCOB == 1 | P5MOMCOB == 1) & c(P4MOMCOB < 2 & P5MOMCOB < 2))) / total
unknown<-nrow(subset(dfSm, c(P2CHPLAC < 0 | is.na(P2CHPLAC)) | c(P4MOMCOB < 0 & P5MOMCOB < 0) | c(is.na(P4MOMCOB) & is.na(P5MOMCOB)))) / total
total<-one.five+two.aft+two.pre+three+unknown

#English Proficiency -- NEED WEIGHTS APPLIED
summary(dfSm$C1SCTOT)
table(dfSm$C1SCTOT)
fail<-subset(dfSm, C1SCTOT >= 0 & C1SCTOT < 37, select = c(C1SCTOT, BYCW0))
pass<-subset(dfSm, C1SCTOT == -1 | C1SCTOT >= 37 | is.na(C1SCTOT), select = c(C1SCTOT, BYCW0))

total<-nrow(fail)+nrow(pass)
nrow(fail)/total
nrow(pass)/total

#Family Structure
wpct(dfSm$P2HPARNT, weight=dfSm$BYCW0)

parTwo.bio<-nrow(subset(dfSm, P1HPARNT==1 | P2HPARNT ==1)) / nrow(dfSm)
parTwo.bioOther<-nrow(subset(dfSm, P1HPARNT == 2 | P1HPARNT == 3 | P2HPARNT == 2 | P2HPARNT == 3)) / nrow(dfSm)
parTwo.mother<-nrow(subset(dfSm, P1HPARNT == 4 | P2HPARNT == 4 )) / nrow(dfSm)
parTwo.other<-nrow(subset(dfSm, P1HPARNT >= 5 | P2HPARNT >= 5 )) / nrow(dfSm)

#Family Income

inc<-subset(dfSm, P2INCOME >= 0, select = c(P2INCOME, BYCW0))
hist(income$P2INCOME)
sd(income$P2INCOME)
wtd.mean(income$P2INCOME, weight=income$BYCW0)

#Pre-K Childcare

summary(dfSm$P1PRIMPK)
wpct(dfSm$P1PRIMPK, weight=NULL, na.rm=TRUE)*100

