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

Risk <- read_excel("RiskFactors1July.xlsx", 
                      sheet = "Pre")

Risk$OpenWindowsFreq <- relevel(as.factor(Risk$OpenWindowsFreq), ref = "Never")

Risk$MessyYard <- relevel(as.factor(Risk$MessyYard), ref = "Extremely orderly")

Risk$Shade <- relevel(as.factor(Risk$Shade), ref = "No shade")

#This analysis is with the full extent of the dataset

#Evaluating the best distribution for the dataset with count data for the indoor collections
RisInPs <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
                   + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
summary(RisInPs)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisInPs)

RisInCMP <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
                   + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
summary(RisInCMP)

#Checking the AICc values for each model
AICc(RisInPs)
AICc(RisInCMP)

#Visualization of the data
RisInPsV1 <- simulateResiduals(fittedModel = RisInPs, n = 250, plot = TRUE)
RisInNB2V1 <- simulateResiduals(fittedModel = RisInCMP, n = 250, plot = TRUE)

#explore interaction contrasts of best fit

m1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m1, type ="III")

m2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m2, type ="III")

m3 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Shade + Window2 + Door1 + Door2 + TypeAC
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m3, type ="III")

m4 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Shade + Door1 + Door2 + TypeAC
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m4, type ="III")

m5 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + MessyYard + Shade + Door1 + Door2 + TypeAC
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m5, type ="III")

m6 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + MessyYard + Shade + Door1 + Door2
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m6, type ="III")

m7 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + AP2.1 + MessyYard + Shade + Door1 + Door2
              + (1|Community/HouseID) + (1|CovRate200m), family = "compois", data = Risk)
car::Anova(m7, type ="III")

AICtab(m1,m2,m3,m4,m5,m6,m7, base = T, weights = T, sort = FALSE)

anova(m5,m6)

df <- tidy(m6)
df1 <- tibble(df)

tab_model(m6)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
confint(m6, method = "uniroot")

summary(m6)

#Creating the figures

my_title <- expression(paste("Indoor female ", italic("Ae. aegypti")))

plot_model(m6, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, type = "pred", axis.title = my_title, title = "")

#Making the figures for the GLMM models

tiff("IndGLMM.tiff",width=9, height=9, units="in", res=300)

p <- plot_model(m7, type = "int", axis.title = my_title, title = "") #this is the interaction term figures
p1 <- p[[3]] + ylim(0,250) + xlim(0,3)
p2 <- p[[2]] + ylim(0,600) + xlim(-2,2)
p3 <- p[[1]] + ylim(0,500) + xlim(-2,2)

gridExtra::grid.arrange(p1,p2,p3)

dev.off()

#This analysis is with the full extent of the dataset

#Evaluating the best distribution for the dataset with count data for the outdoor collections
RisOutPs <- glmmTMB(AEoutFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
                   + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
summary(RisOutPs)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisOutPs)

#Checking the AICc values for each model
AIC(RisOutPs)

#Visualization of the data
RisOutPsV1 <- simulateResiduals(fittedModel = RisOutPs, n = 250, plot = TRUE)

#explore interaction contrasts of best fit
m8 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
                    + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m8, type ="III")

m9 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Door1 + Door2 + TypeAC
              + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m9, type ="III")

m10 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Door1 + Door2
              + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m10, type ="III")

m11 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + MessyYard + Tires + Shade + Window1 + Door1 + Door2
               + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m11, type ="III")

m12 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + MessyYard + Tires + Shade + Window1 + Door2
               + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m12, type ="III")

m13 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + Tires + Shade + Window1 + Door2
               + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m13, type ="III")

m14 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + Tires + Shade + Window1
               + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m14, type ="III")

m15 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + Tires + Shade + Window1
               + (1|Community/HouseID) + (1|CovRate200m), family = "poisson", data = Risk)
car::Anova(m15, type ="III")

AICtab(m8,m9,m10,m11,m12,m13,m14, m15, base = T, weights = T, sort = FALSE)

df2 <- tidy(m15)
df3 <- tibble(df2)

tab_model(m15)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
confint(m15, method = "uniroot")

summary(m15)

#Figure for the outdoor Aedes

my_title_out <- expression(paste("Outdoor female ", italic("Ae. aegypti")))

plot_model(m15, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title_out)

plot_model(m15, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title_out, )

plot_model(m15, type = "pred", mdrt.values = "meansd", axis.title = my_title_out)


#Making the figures for the GLMM models

tiff("OutGLMM.tiff",width=9, height=9, units="in", res=300)

p <- plot_model(m10, type = "int", axis.title = my_title, title = "") #this is the interaction term figures
p1 <- p[[3]] + ylim(0,250) + xlim(0,3)
p2 <- p[[2]] + ylim(0,600) + xlim(-2,2)
p3 <- p[[1]] + ylim(0,500) + xlim(-2,2)

gridExtra::grid.arrange(p1,p2,p3)

dev.off()













#This portion of the analysis only evaluates the dataset from the PRE-intervention of 2018

Risk <- read_excel("RiskFactors1July.xlsx", 
                   sheet = "2018Pre")

Risk$OpenWindowsFreq <- relevel(as.factor(Risk$OpenWindowsFreq), ref = "No")

Risk$MessyYard <- relevel(as.factor(Risk$MessyYard), ref = "No")

Risk$Shade <- relevel(as.factor(Risk$Shade), ref = "No shade")

#For this procedure we will do a Generalized linear Model analysis
#Evaluating the best distribution for the dataset with count data for the indoor collections
RisInPs1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "poisson", data = Risk)

#Visualization of the data
RisInPsV1 <- simulateResiduals(fittedModel = RisInPs1, n = 250, plot = TRUE)

#explore interaction contrasts of best fit

m1 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + OpenWindowsFreq + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "poisson", data = Risk)
car::Anova(m1, type ="III")









m2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door2 
          , family = "poisson", data = Risk)
car::Anova(m2, type ="III")

m3 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Door2 
          , family = "poisson", data = Risk)
car::Anova(m3, type ="III")

m4 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + MessyYard + AP2.1 + AP2.2 + Window1 + Door2 
          , family = "poisson", data = Risk)
car::Anova(m4, type ="III")

m5 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + MessyYard + AP2.1 + Window1 + Door2 
          , family = "poisson", data = Risk)
car::Anova(m5, type ="III")

m6 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + TypeAC + WaterStorage + MessyYard + AP2.1 + Window1
          , family = "poisson", data = Risk)
car::Anova(m6, type ="III")

AICtab(m1,m2,m3,m4,m5,m6, base = T, weights = T, sort = FALSE)

df <- tidy(m6)
df1 <- tibble(df)

tab_model(m6)
#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
confint(m6, method = "uniroot")

summary(m6)

#Creating the figures

my_title <- expression(paste("Indoor female ", italic("Ae. aegypti")))

plot_model(m6, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, type = "pred", axis.title = my_title, title = "")

#Making the figures for the GLMM models

tiff("IndGLM.tiff",width=9, height=9, units="in", res=300)

p <- plot_model(m7, type = "int", axis.title = my_title, title = "") #this is the interaction term figures
p1 <- p[[3]] + ylim(0,250) + xlim(0,3)
p2 <- p[[2]] + ylim(0,600) + xlim(-2,2)
p3 <- p[[1]] + ylim(0,500) + xlim(-2,2)

gridExtra::grid.arrange(p1,p2,p3)

dev.off()

#For this procedure we will do a Generalized linear Model analysis
#Evaluating the best distribution for the dataset with count data for the indoor collections
RisOutPs2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + Shade + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                , family = "poisson", data = Risk)

#Evaluation of the overdispersion of the dataset 
check_overdispersion(RisOutPs2)

#Testing distributions that better fit overdispersed count data
RisOutNB1 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + Shade + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                    , family = "nbinom1", data = Risk)

RisOutNB2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + Shade + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
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
m7 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + Shade + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
                     , family = "nbinom2", data = Risk)
car::Anova(m7, type ="III")

m8 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1 + Door2 
              , family = "nbinom2", data = Risk)
car::Anova(m8, type ="III")

m9 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Window2 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m9, type ="III")

m10 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + WaterStorage + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Door1
              , family = "nbinom2", data = Risk)
car::Anova(m10, type ="III")

m11 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + TypeAC + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Door1
               , family = "nbinom2", data = Risk)
car::Anova(m11, type ="III")

m12 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + MessyYard + Tires + AP2.1 + AP2.2 + Window1 + Door1
               , family = "nbinom2", data = Risk)
car::Anova(m12, type ="III")

m13 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + MessyYard + Tires + AP2.1 + AP2.2 + Window1
               , family = "nbinom2", data = Risk)
car::Anova(m13, type ="III")

AICtab(m7,m8,m9,m10,m11,m12, m13, base = T, weights = T, sort = FALSE)

anova(m12, m13)

df2 <- tidy(m13)
df3 <- tibble(df2)

tab_model(m13)
#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
confint(m13, method = "uniroot")

summary(m13)

#Creating the figures

my_title <- expression(paste("Indoor female ", italic("Ae. aegypti")))

plot_model(m13, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m13, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m13, type = "pred", axis.title = my_title, title = "")

#Making the figures for the GLMM models

tiff("IndGLM.tiff",width=9, height=9, units="in", res=300)

p <- plot_model(m13, type = "int", axis.title = my_title, title = "") #this is the interaction term figures
p1 <- p[[3]] + ylim(0,250) + xlim(0,3)
p2 <- p[[2]] + ylim(0,600) + xlim(-2,2)
p3 <- p[[1]] + ylim(0,500) + xlim(-2,2)

gridExtra::grid.arrange(p1,p2,p3)

dev.off()























m2 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + TypeAC
              , family = "poisson", data = Risk)
car::Anova(m2, type ="III")

m3 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + TypeAC
              , family = "poisson", data = Risk)
car::Anova(m3, type ="III")

m4 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Shade + Window1 + Window2 + TypeAC
              , family = "poisson", data = Risk)
car::Anova(m4, type ="III")

m5 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Window1 + Window2 + TypeAC
              , family = "poisson", data = Risk)
car::Anova(m5, type ="III")

m6 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + MessyYard + Window1 + Window2 + TypeAC
              , family = "poisson", data = Risk)
car::Anova(m6, type ="III")

m7 <- glmmTMB(AEinFem ~ offset(log(WeeksIN)) + OpenWindowsFreq + WaterStorage + AP2.1 + MessyYard + Window1 + Window2
              , family = "poisson", data = Risk)
car::Anova(m7, type ="III")
summary(m7)

AICtab(m1,m2,m3,m4,m5,m6, base = T, weights = T, sort = FALSE)

df <- tidy(m6)
df1 <- tibble(df)

tab_model(m6)

#This next code takes a lot of time, skip to the joint_test and plot effects if already tested
confint(m6, method = "uniroot")

summary(m6)

#Creating the figures

my_title <- expression(paste("Indoor female ", italic("Ae. aegypti")))

plot_model(m6, vline.color = "lightgray", transform = "plogis", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, vline.color = "lightgray", sort.est = TRUE, show.value = TRUE, value.offset = .3, title = my_title)

plot_model(m6, type = "pred", axis.title = my_title, title = "")

#Making the figures for the GLMM models

tiff("IndGLMM.tiff",width=9, height=9, units="in", res=300)

p <- plot_model(m7, type = "int", axis.title = my_title, title = "") #this is the interaction term figures
p1 <- p[[3]] + ylim(0,250) + xlim(0,3)
p2 <- p[[2]] + ylim(0,600) + xlim(-2,2)
p3 <- p[[1]] + ylim(0,500) + xlim(-2,2)

gridExtra::grid.arrange(p1,p2,p3)

dev.off()


#Evaluating the best distribution for the dataset with count data for the outdoor collections
RisInPs2 <- glmmTMB(AEoutFem ~ offset(log(WeeksOUT)) + OpenWindowsFreq + WaterStorage + AP2.1 + AP2.2 + MessyYard + Tires + Shade + Window1 + Window2 + Door1 + Door2 + TypeAC
                    , family = "poisson", data = Risk)
summary(RisInPs2)
