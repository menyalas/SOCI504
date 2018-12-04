setwd("~/SOCI504 Replication")
df <- read.delim("20181203_AM_Glick.tsv", encoding="UTF-8")

library(stargazer)
library(tidyverse)

dfSm <- subset(df, C4R4MSCL>0)

#model 1 
model1 <- lm(dfSm$C5R4MSCL~dfSm$newGender.level +
               dfSm$R2_KAGE+
               dfSm$P1HMAGE + 
               dfSm$reP1HPARNT.level+
               dfSm$P1NUMSIB+
               dfSm$reWKINCOME.log+
               dfSm$edCat.level+
               dfSm$genCat.new.level,
             weights = dfSm$BYPW0, 
             data = dfSm)
summary(model1)
stargazer(model1, digits = 2)

#Model 2
model2 <- lm(dfSm$C5R4MSCL~dfSm$newGender.level +
               dfSm$R2_KAGE+
               dfSm$P1HMAGE + 
               dfSm$reP1HPARNT.level+
               dfSm$P1NUMSIB+
               dfSm$reWKINCOME.log+
               dfSm$edCat.level+
               dfSm$genCat.new.level+
               dfSm$ethCat.level+
               dfSm$OLDS.level,
             weights = dfSm$BYPW0, 
             data = dfSm)
summary(model2)
stargazer(model2, digits = 2)

#MODEL 3
model3 <- lm(dfSm$C5R4MSCL~dfSm$newGender.level +
               dfSm$R2_KAGE+
               dfSm$P1HMAGE + 
               dfSm$reP1HPARNT.level+
               dfSm$P1NUMSIB+
               dfSm$reWKINCOME.log+
               dfSm$edCat.level+
               dfSm$genCat.new.level+
               dfSm$ethCat.level+
               dfSm$OLDS.level+
               dfSm$PreKCare.level+
               dfSm$AttendOpen+
               dfSm$PTA+
               dfSm$ptMeet+
               dfSm$SportArt+
               dfSm$nonengInstruct+
               dfSm$library+
               dfSm$otherOut+
               dfSm$SchoolCh.level+
               dfSm$KinderDay.level,
             weights = dfSm$BYPW0, 
             data = dfSm)
summary(model3)
stargazer(model3, digits = 2)

#MODEl 4
model4 <- lm(dfSm$C5R4MSCL~dfSm$newGender.level +
               dfSm$R2_KAGE+
               dfSm$P1HMAGE + 
               dfSm$reP1HPARNT.level+
               dfSm$P1NUMSIB+
               dfSm$reWKINCOME.log+
               dfSm$edCat.level+
               dfSm$genCat.new.level+
               dfSm$ethCat.level+
               dfSm$OLDS.level+
               dfSm$PreKCare.level+
               dfSm$AttendOpen+
               dfSm$PTA+
               dfSm$ptMeet+
               dfSm$SportArt+
               dfSm$nonengInstruct+
               dfSm$library+
               dfSm$otherOut+
               dfSm$SchoolCh.level+
               dfSm$KinderDay.level, 
             dfSm$C4R4MSCL,
             weights = dfSm$BYPW0, 
             data = dfSm)
summary(model4)
stargazer(model4, digits = 2)