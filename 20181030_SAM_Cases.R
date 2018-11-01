#Call libraries
library(tidyverse)

#Set Directory
setwd("~/Desktop/SOCI504 Replication")

#Import samples of SPSS and tab delimited data (DS0005)
df <- read.delim("data/DS0001/28023-0001-Data.tsv", encoding="UTF8")

#Identify 13,618 cases
glick<-13618

#Condition01: Valid test score in Spr2002 "Rnd5", Key Variable: C5R4MSCL
summary(df$C5R4MSCL)
hist(df$C5R4MSCL)

#Omit R5 nonvalid R5 test scores (NAs & -9s in code book)
minusR5test<-subset(df, C5R4MSCL != "-9")
nrow(minusR5na)-nrow(minusR5other)
summary(minusR5test$C5R4MSCL)
hist(minusR5test$C5R4MSCL)

#Condition02: Participated in all three waves of the ECLS-K (Spr1999 "Rnd2"; Spr2000 "Rnd4"; Spr2002 "Rnd5")

#Omit students who joined the study in spring grade 1 (R4) - freshened sample
summary(minusR5test$R4FRSHEN)
table(minusR5test$R4FRSHEN)
minusFresh<-subset(minusR5test, R4FRSHEN != "1" | is.na(R4FRSHEN)) #is.na here to KEEP the NAs in the dataframe
nrow(minusR5test)-nrow(minusFresh)
summary(minusFresh$R4FRSHEN)
table(minusFresh$R4FRSHEN)

#Omit nonrespondents in grade 3 (R5)
summary(minusFresh$C5ASMTST)
table(minusFresh$C5ASMTST)
minusNon5<-subset(minusFresh, C5ASMTST != 5 | is.na(C5ASMTST))#is.na here to KEEP the NAs in the dataframe)
nrow(minusFresh)-nrow(minusNon5)
summary(minusNon5$C5ASMTST)
table(minusNon5$C5ASMTST)

#Omit nonrespondents in spring grade 1 (R4)
summary(minusNon5$C4ASMTST)
table(minusNon5$C4ASMTST)
minusNon4<-subset(minusNon5, C4ASMTST != 5 | is.na(C4ASMTST))#is.na here to KEEP the NAs in the dataframe
nrow(minusNon5)-nrow(minusNon4)
summary(minusNon4$C4ASMTST)
table(minusNon4$C4ASMTST)

#Omit cases that were nonrespondents in BOTH spring and fall kindergarden (rounds 1 & 2)
minusNonK <- subset(minusNon4, C1ASMTST != 5 & C2ASMTST != 5 | is.na(C1ASMTST) | is.na(C2ASMTST))#is.na here to KEEP the NAs in the dataframe
nrow(minusNon4)-nrow(minusNonK)
nrow(minusNonK)-glick
summary(minusNonK$C1ASMTST)
summary(minusNonK$C2ASMTST)
table(minusNonK$C1ASMTST)
table(minusNonK$C2ASMTST)

#Examine 26 cases of "5" in minusNonK$C2ASMTST and strike the one (CHILDID #1248011C) that did not seem to participate in all three waves of the study.
R2non<-subset(minusNonK, C2ASMTST == 5)
non<-data_frame(R2non$CHILDID, R2non$C1ASMTST, R2non$C2ASMTST, R2non$C4ASMTST, R2non$C5ASMTST)

#Are there any cases where there are NAs in all C1ASMTST, C2ASMTST, C4ASMTST & C5ASMTST? No, but there are 10 cases that have NAs for Rounds 1-4. Do we count them?
NAs<-subset(minusNonK, is.na(C1ASMTST) & is.na(C2ASMTST) & is.na(C4ASMTST))
n.a<-data_frame(NAs$CHILDID, NAs$C1ASMTST, NAs$C2ASMTST, NAs$C4ASMTST, NAs$C5ASMTST)

#Where are the NAs in the dataframe?
naRows<-rowSums(is.na(minusNonK))
hist(naRows)
naCols<-colSums(is.na(minusNonK))
hist(naCols)

install.packages("stringr")
library(stringr)

colsR6 <- subset(minusNonK, grepl(glob2rx("*6"), x))
