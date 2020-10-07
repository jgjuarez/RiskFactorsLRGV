#GLMM analysis of the risk factors for Aedes aegypti in the LRGV

library(glmmTMB)
library(see)
library(performance)
library(randomForest)
library(lme4)
library(car)
library(emmeans)
library(effects)
library(multcomp)
library(MuMIn)
library(DHARMa)
library(ggplot2); theme_set(theme_bw())
library(tidyverse)
library(bbmle)
library(plyr)
library(dplyr)
library(broom)
library(broom.mixed)
library(lubridate)
library(ggeffects)
library(MASS)
library(mgcv)
library(itsadug)
library(lme4)
library(kableExtra)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(AICcmodavg)
library(readxl)

source(system.file("other_methods","lsmeans_methods.R",package="glmmTMB"))

#This portion of the analysis only evaluates the dataset from the PRE-intervention of 2018

Risk <- read_excel("RiskFactors4Sep.xlsx", 
                   sheet = "Sheet1")

Risk$OpenWindowsFreq <- relevel(as.factor(Risk$OpenWindowsFreq), ref = "No")

Risk$MessyYard <- relevel(as.factor(Risk$MessyYard), ref = "Average")

Risk$Shade <- relevel(as.factor(Risk$Shade), ref = "Half shade")

#For this procedure we will do a Generalized linear Model analysis
#Evaluating the best distribution for the dataset with count data for the indoor collections
RisInPs1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "poisson", data = Risk)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisInPs1)

RisInNB1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "nbinom1", data = Risk)

RisInNB2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "nbinom2", data = Risk)

#Visualization of the data
RisInPsV1 <- simulateResiduals(fittedModel = RisInPs1, n = 250, plot = TRUE)
RisInNB1V1 <- simulateResiduals(fittedModel = RisInNB1, n = 250, plot = TRUE)
RisInNB2V1 <- simulateResiduals(fittedModel = RisInNB2, n = 250, plot = TRUE)

#Checking the AIC values for each model
AIC(RisInPs1)
AIC(RisInNB1)
AIC(RisInNB2)

#explore interaction contrasts of best fit with a NB2 distribution

m1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m1, type ="III")

m2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + AP2.2 + Window1 + Window2 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m2, type ="III")

m3 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + Window1 + Window2 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m3, type ="III")

m4 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + Window1 + Window2 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m4, type ="III")

m5 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + OtherContainers + Income+ AP2.1 + Window1 + Window2 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m5, type ="III")

m6 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + Income+ AP2.1 + Window1 + Window2 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m6, type ="III")

m7 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + Income+ AP2.1 + Window1 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m7, type ="III")

m8 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + Income+ AP2.1 + Window1 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m8, type ="III")

AICtab(m1,m2,m3,m4,m5,m6,m7,m8, base = T, weights = T, sort = FALSE)

summary(m7)

results <- check_collinearity(m7)
results
plot(results)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
df1.1 <- tidy(m7)

df1.2 <- tidy(confint(m7, method = "uniroot"))


#Creating the figures

my_title <- expression(paste("Indoor female ", italic("Ae. aegypti")))

plot_model(m7, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m7, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m7, type = "pred", axis.title = my_title, title = "")

#For this procedure we will do a Generalized linear Model analysis
#Evaluating the best distribution for the dataset with count data for the outdoor collections
RisOutPs2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + MessyYard + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "poisson", data = Risk)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisOutPs2)

#Testing distributions that better fit overdispersed count data
RisOutNB1 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + MessyYard + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "nbinom1", data = Risk)

RisOutNB2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + MessyYard + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "nbinom2", data = Risk)

#Checking the AIC values for each model
AIC(RisOutPs2)
AIC(RisOutNB1)
AIC(RisOutNB2)

#Visualization of the data
RisOutPsV2 <- simulateResiduals(fittedModel = RisOutPs2, n = 250, plot = TRUE)
RisOutNB1V2 <- simulateResiduals(fittedModel = RisOutNB1, n = 250, plot = TRUE)
RisOutNB2V2 <- simulateResiduals(fittedModel = RisOutNB2, n = 250, plot = TRUE)

#explore interaction contrasts of best fit
m9 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + MessyYard + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m9, type ="III")

m10 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + MessyYard + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
               , family = "nbinom1", data = Risk)
car::Anova(m10, type ="III")

m11 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + MessyYard + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Door1 + Door2 
               , family = "nbinom1", data = Risk)
car::Anova(m11, type ="III")

m12 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + OpenDoorFreq + WaterStorage + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Door1 + Door2 
               , family = "nbinom1", data = Risk)
car::Anova(m12, type ="III")

m13 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + OpenDoorFreq + OtherContainers + Tires + Income  + AP2.1 + AP2.2 + Window1 + Door1 + Door2 
               , family = "nbinom1", data = Risk)
car::Anova(m13, type ="III")

m14 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + OpenDoorFreq + Tires + Income  + AP2.1 + AP2.2 + Window1 + Door1 + Door2 
               , family = "nbinom1", data = Risk)
car::Anova(m14, type ="III")

m15 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Vegetation + OpenDoorFreq + Tires + Income  + AP2.1 + AP2.2 + Door1 + Door2 
               , family = "nbinom1", data = Risk)
car::Anova(m15, type ="III")

AICtab(m9,m10,m11,m12,m13,m14,m15, base = T, weights = T, sort = FALSE)
summary(m15)

df4 <- tidy(m15)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
df5 <- tidy(confint(m15, method = "uniroot"))


















          ###THIS IS THE PREVIOUS CODE



#For this procedure we will do a Generalized linear Model analysis
#Evaluating the best distribution for the dataset with count data for the indoor collections
RisInPs1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "poisson", data = Risk)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisInPs1)

RisInNB1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "nbinom1", data = Risk)

RisInNB2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "nbinom2", data = Risk)

#Visualization of the data
RisInPsV1 <- simulateResiduals(fittedModel = RisInPs1, n = 250, plot = TRUE)
RisInNB1V1 <- simulateResiduals(fittedModel = RisInNB1, n = 250, plot = TRUE)
RisInNB2V1 <- simulateResiduals(fittedModel = RisInNB2, n = 250, plot = TRUE)

#Checking the AIC values for each model
AIC(RisInPs1)
AIC(RisInNB1)
AIC(RisInNB2)

#explore interaction contrasts of best fit with a NB2 distribution

m1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m1, type ="III")

m2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m2, type ="III")

m3 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m3, type ="III")

m4 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1  
              , family = "nbinom2", data = Risk)
car::Anova(m4, type ="III")

m5 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + AP2.1 + AP2.2 + Window1 + Door1  
              , family = "nbinom2", data = Risk)
car::Anova(m5, type ="III")

m6 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + AP2.2 + Window1 + Door1  
              , family = "nbinom2", data = Risk)
car::Anova(m6, type ="III")

m7 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OtherContainers + AP2.2 + Window1 + Door1  
              , family = "nbinom2", data = Risk)
car::Anova(m7, type ="III")

m8 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + AP2.2 + Window1 + Door1  
              , family = "nbinom2", data = Risk)
car::Anova(m8, type ="III")

m9 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + Window1 + Door1  
              , family = "nbinom2", data = Risk)
car::Anova(m9, type ="III")
summary(m8)

AICtab(m1,m2,m3,m4,m5,m6,m7,m8, base = T, weights = T, sort = FALSE)


m1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + OpenDoorFreq + WaterStorage + OtherContainers + Income+ AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m1, type ="III")

results <- check_collinearity(m1)
results
plot(results)

#explore interaction contrasts of best fit with a poisson distribution

m1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "poisson", data = Risk)
car::Anova(m1, type ="III")

m2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + AP2.2 + Window1 + Window2 + Door1
              , family = "poisson", data = Risk)
car::Anova(m2, type ="III")

m3 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + OtherContainers + AP2.1 + Window1 + Window2 + Door1
              , family = "poisson", data = Risk)
car::Anova(m3, type ="III")

m4 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + OtherContainers + AP2.1 + Window1 + Window2 + Door1
              , family = "poisson", data = Risk)
car::Anova(m4, type ="III")

m5 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + OtherContainers + AP2.1 + Window1 + Door1
              , family = "poisson", data = Risk)
car::Anova(m5, type ="III")

AICtab(m1,m2,m3,m4,m5, base = T, weights = T, sort = FALSE)

summary(m3)

df2 <- tidy(m3)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
df3 <- tidy(confint(m3, method = "uniroot"))

#Creating the figures

my_title <- expression(paste("Indoor female ", italic("Ae. aegypti")))

plot_model(m6, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, type = "pred", axis.title = my_title, title = "")



#For this procedure we will do a Generalized linear Model analysis
#Evaluating the best distribution for the dataset with count data for the outdoor collections
RisOutPs2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + OtherContainers + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "poisson", data = Risk)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisOutPs2)

#Testing distributions that better fit overdispersed count data
RisOutNB1 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + OtherContainers + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "nbinom1", data = Risk)

RisOutNB2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + OtherContainers + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "nbinom2", data = Risk)

#Checking the AIC values for each model
AIC(RisOutPs2)
AIC(RisOutNB1)
AIC(RisOutNB2)

#Visualization of the data
RisOutPsV2 <- simulateResiduals(fittedModel = RisOutPs2, n = 250, plot = TRUE)
RisOutNB1V2 <- simulateResiduals(fittedModel = RisOutNB1, n = 250, plot = TRUE)
RisOutNB2V2 <- simulateResiduals(fittedModel = RisOutNB2, n = 250, plot = TRUE)

#explore interaction contrasts of best fit
m6 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + OtherContainers + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m6, type ="III")

m7 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + OtherContainers + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m7, type ="III")

m8 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m8, type ="III")

m9 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + Tires + AP2.1 + AP2.2 + Window1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m9, type ="III")

m10 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + WaterStorage + Tires + AP2.1 + AP2.2 + Door2 
               , family = "nbinom2", data = Risk)
car::Anova(m10, type ="III")

m11 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + Tires + AP2.1 + AP2.2 + Door2 
               , family = "nbinom2", data = Risk)
car::Anova(m11, type ="III")

m12 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + Tires + AP2.1 + Door2 
               , family = "nbinom2", data = Risk)
car::Anova(m12, type ="III")

m13 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + Shade + Tires + AP2.1
               , family = "nbinom2", data = Risk)
car::Anova(m13, type ="III")

AICtab(m6, m7,m8,m9,m10,m11,m12,m13, base = T, weights = T, sort = FALSE)
summary(m12)

df4 <- tidy(m12)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
df5 <- tidy(confint(m12, method = "uniroot"))
