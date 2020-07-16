#I need to recode the different variables for window and door just to have yes and no for quality purposes, then get count data for total holes.  

library(dplyr)
library(car)
library(sjmisc)
library(readxl)

RiskMod <- read_excel("RiskFactors27May.xlsx", 
                      sheet = "Complete KAP")

#Lets create a df just to select the windows from the dataset and then adjust for the sum and quality

Window <- dplyr::select(RiskMod, 283:552)


#P.112 to 126 code XXX.5
GlazingHoles <- dplyr::select(Window, contains(".5"))

Glazing <- ifelse(GlazingHoles[,] == 1,0, 
                      ifelse(GlazingHoles[,] == 2,1, 
                      ifelse(GlazingHoles[,] == 3,1, 0)))

GlazingTot <- matrix(rowSums(Glazing))
View(GlazingTot)

#P.112 to 126 code XXX.6

ExtScreen <- dplyr::select(Window, contains(".6"))

ExtScreenPr <- ifelse(ExtScreen[,] == 1,1, 
                  ifelse(ExtScreen[,] == 2,0,0))

ExtScreenTot <- matrix(rowSums(ExtScreenPr))
View(ExtScreenTot)

#P.112 to 126 code XXX.7

ExtScreenOpn <- dplyr::select(Window, contains(".7"))

ExtScreenOpnPr <- ifelse(ExtScreenOpn[,] == 1,1, 
                      ifelse(ExtScreenOpn[,] == 2,0,0))

ExtScreenOpnTot <- matrix(rowSums(ExtScreenOpnPr))
View(ExtScreenOpnTot)

#P.112 to 126 code XXX.8

ExtScreenSeal <- dplyr::select(Window, contains(".8"))

ExtScreenSealHoles <- ifelse(ExtScreenSeal[,] == 1,0, 
                         ifelse(ExtScreenSeal[,] == 2,1,
                         ifelse(ExtScreenSeal[,] == 3,1,0)))

ExtScreenSealTot <- matrix(rowSums(ExtScreenSealHoles))
View(ExtScreenSealTot)

#P.112 to 126 code XXX.9

ExtScreenHoles <- dplyr::select(Window, contains(".9"))

ExtScreenHolesPr <- ifelse(ExtScreenHoles[,] == 1,0, 
                          ifelse(ExtScreenHoles[,] == 2,1,
                                 ifelse(ExtScreenHoles[,] == 3,1,0)))

ExtScreenHolesTot <- matrix(rowSums(ExtScreenHolesPr))
View(ExtScreenHolesTot)

#P.112 to 126 code XXX.10 

WindowAC <- dplyr::select(Window, contains(".10"))

WindowACPr <- ifelse(WindowAC[,] == 1,1, 
                           ifelse(WindowAC[,] == 2,0,0))
                               
WindowACTot <- matrix(rowSums(WindowACPr))
View(WindowACTot)

#P.112 to 126 code XXX.11 

WindowACHoles <- dplyr::select(Window, ends_with(".11"))

ACholes <- ifelse(WindowACHoles[,] == 1,0, 
                     ifelse(WindowACHoles[,] == 2,1,
                            ifelse(WindowACHoles[,] == 3,1,0)))

ACholesTot <- matrix(rowSums(ACholes))
View(ACholesTot)

#Lets create a df just to select the door from the dataset and then adjust for the sum and quality

Door <- dplyr::select(RiskMod, 555:665)

#P.127 to 133 code XXX.2

ExteriorD <- dplyr::select(Door, ends_with(".2"))

ExteriorDPr <- ifelse(ExteriorD[,] == 1,1, 
                     ifelse(ExteriorD[,] == 2,0,0))

ExteriorDTot <- matrix(rowSums(ExteriorDPr))
View(ExteriorDTot)

#P.127 to 133 code XXX.6

DoorScreen <- dplyr::select(Door, ends_with(".6"))

DoorScreenPr <- ifelse(DoorScreen[,] == 1,1, 
                  ifelse(DoorScreen[,] == 2,0,0))

DoorScreenPrTot <- matrix(rowSums(DoorScreenPr))
View(DoorScreenPrTot)

#P.127 to 133 code XXX.7

DSeal <- dplyr::select(Door, ends_with(".7"))

DSealHoles <- ifelse(DSeal[,] == 1,0, 
                      ifelse(DSeal[,] == 2,1,
                             ifelse(DSeal[,] == 3,1,0)))

DSealHolesTot <- matrix(rowSums(DSealHoles))
View(DSealHolesTot)

#P.127 to 133 code XXX.8

DScreen <- dplyr::select(Door, ends_with(".8"))

DScreenHoles <- ifelse(DScreen [,] == 1,0, 
                     ifelse(DScreen [,] == 2,1,
                            ifelse(DScreen [,] == 3,1,0)))

DScreenHolesTot <- matrix(rowSums(DScreenHoles))
View(DScreenHolesTot)

#P.127 to 133 code XXX.12

HoleUnder <- dplyr::select(Door, ends_with(".12"))

HoleUnderPr <- ifelse(HoleUnder [,] == 1,1, 
                       ifelse(HoleUnder [,] == 2,0,0))

HoleUnderTot <- matrix(rowSums(HoleUnderPr))
View(HoleUnderTot)

#P.127 to 133 code XXX.13

ThresholdCovers <- dplyr::select(Door, ends_with(".13"))

ThresholdCoversPr <- ifelse(ThresholdCovers [,] == 1,1, 
                      ifelse(ThresholdCovers [,] == 2,0,0))

ThresholdCoversTot <- matrix(rowSums(ThresholdCoversPr))
View(ThresholdCoversTot)

#P.127 to 133 code XXX.14

DBrush <- dplyr::select(Door, ends_with(".14"))

DBrushPr <- ifelse(DBrush [,] == 1,1, 
                            ifelse(DBrush [,] == 2,0,0))

DBrushTot <- matrix(rowSums(DBrushPr))
View(DBrushTot)

#Now that we have the recoded variables we can conduct the Principal Component analysis

library(FactoMineR)
library(factoextra)
library(ade4)
library(ExPosition)
library(readxl)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(dplyr)

Surveys <- read_excel("RiskFactors1July.xlsx", 
                      sheet = "Surveys")

#This is the Window PCA index

winpca <- Surveys[,c(41:48)]
View(winpca)

h1 <- princomp(winpca, cor = T)
summary(h1)

loadings(h1)

h1pred <- predict(h1)[,1]

df <- tibble(h1pred)

h1.1 <- PCA(winpca, scale.unit = T, ncp = 5, graph = F)
summary(h1.1)

h1pred <- predict.PCA(h1.1, newdata = winpca)

#Visualization of the PCA results
fviz_eig(h1, addlabels = TRUE, ylim = c(0, 30))

winvar <- get_pca_var(h1)
corrplot(winvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_pca_var(h1, col.var = "contrib", alpha.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h1, choice = "var", axes = 1:2, top = 10)

fviz_pca_ind(h1, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
)

#This is the Door PCA index

doorpca <- Surveys[,c(49:56)]
View(doorpca)

h2 <- princomp(doorpca, cor = T)
summary(h2)

loadings(h2)

h2pred <- predict(h2)[,1]

df2 <- tibble(h2pred)

h2.1 <- PCA(doorpca, scale.unit = T, ncp = 5, graph = F)
summary(h2.1)

#Visualization of the PCA results

fviz_eig(h2, addlabels = TRUE, ylim = c(0, 30))

doorvar <- get_pca_var(h2)
corrplot(doorvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_pca_var(h2, col.var = "contrib", alpha.var = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h2, choice = "var", axes = 1:2, top = 10)

fviz_pca_ind(h2, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
)

#This is the Yard FMAD index

yardpca <- Surveys[,c(27,29:37,39:40)]
View(yardpca)

h3.1 <- FAMD(yardpca, ncp = 5, graph = T)
summary(h3.1)

h3pred <- predict.FAMD(h3.1, newdata = yardpca)

df3 <- data.frame(h3pred, stringsAsFactors = TRUE)

df3.1 <- df3 %>% select(1)

#Visualization of the FMAD results
fviz_eig(h3.1, addlabels = TRUE, ylim = c(0, 15))

yardvar <- get_famd_var(h3.1)
corrplot(yardvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_famd_var(h3.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_famd_var(h3.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h3.1, choice = "var", axes = 1:2, top = 10)

#Figure transformation for the PCM models
tiff("PMC.tiff",width=9, height=9, units="in", res=300)

p1 <- fviz_famd_var(h3.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Yard variables - FAMD")

p2 <- fviz_famd_var(h3.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Yard quantitative - FAMD")

p3 <- fviz_pca_var(h1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Window quantitative - PCA")

p4 <- fviz_pca_var(h2, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Door quantitative - PCA")

gridExtra::grid.arrange(p1,p2,p3,p4)

dev.off()

#This is the KAP1 FMAD index

kap1pca <- Surveys[,c(10,14:15,18,20:21)]
View(kap1pca)

h4.1 <- FAMD(kap1pca, ncp = 5, graph = T)
summary(h4.1)

h4pred <- predict.FAMD(h4.1, newdata = kap1pca)

df4 <- data.frame(h4pred, stringsAsFactors = TRUE)

df4.1 <- df4 %>% select(1)

h4.1$quali.var[["coord"]]

#Visualization of the FMAD results
fviz_eig(h4.1, addlabels = TRUE, ylim = c(0, 20))

kap1var <- get_famd_var(h4.1)
corrplot(kap1var$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_famd_var(h4.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_famd_var(h4.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h4.1, choice = "var", axes = 1:2, top = 10)

#Figure transformation for the PCM2 models
tiff("PMC2.tiff",width=9, height=9, units="in", res=300)

p5 <- fviz_famd_var(h4.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "KAP1 variables - FAMD")

p6 <- fviz_famd_var(h4.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "KAP1 quantitative - FAMD")

gridExtra::grid.arrange(p5,p6)

dev.off()

#This is the KAP2 FMAD index

kap2pca <- Surveys[,c(16:17,22:24,26)]
View(kap2pca)

h5.1 <- FAMD(kap2pca, ncp = 5, graph = T)
summary(h5.1)

h5pred <- predict.FAMD(h5.1, newdata = kap2pca)

h5.1$quali.var[["coord"]]

#Visualization of the FMAD results
fviz_eig(h5.1, addlabels = TRUE, ylim = c(0, 15))

kap2var <- get_famd_var(h5.1)
corrplot(kap2var$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_famd_var(h5.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_famd_var(h5.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h5.1, choice = "var", axes = 1:2, top = 10)

#Figure transformation for the PCM2 models
tiff("PMC2.tiff",width=9, height=9, units="in", res=300)

p5 <- fviz_famd_var(h4.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "KAP1 variables - FAMD")

p6 <- fviz_famd_var(h4.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "KAP1 quantitative - FAMD")

p7 <- fviz_famd_var(h5.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "KAP2 variables - FAMD")

p8 <- fviz_famd_var(h5.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "KAP2 quantitative - FAMD")

gridExtra::grid.arrange(p5,p6,p7,p8)

dev.off()

