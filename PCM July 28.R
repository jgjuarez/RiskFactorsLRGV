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
library(psych)
Surveys <- read_excel("RiskFactors1July.xlsx", 
                      sheet = "Surveys")

#This is the KAP1 FMAD index

kap1pca <- Surveys[,c(4,9,14:15,18,26)]
View(kap1pca)

pairs.panels(kap1pca, gap = 0, pch = 21)

h1.1 <- MCA(kap1pca, ncp = 5, graph = T)
summary(h1.1)

get_eigenvalue(h1.1)

h1pred <- predict.MCA(h1.1, newdata = kap1pca)

df1 <- data.frame(h1pred, stringsAsFactors = TRUE)

df1.1 <- df1 %>% select(1)

h1.1$var[["contrib"]]
h1.1$var[["coord"]]

#Visualization of the FMAD results
fviz_contrib(h1.1, choice = "var", axes = 1:2, top = 10)

fviz_eig(h1.1, addlabels = TRUE, ylim = c(0, 15))

kap1var <- get_mca_var(h1.1)
corrplot(kap1var$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_mca_var(h1.1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10)) + labs(title = "AP1 variable categories - MCA")

fviz_mca_var(h1.1, choice = "mca.cor",repel = TRUE, ggtheme = theme_minimal()) + labs(title = "AP1 variable - MCA")

fviz_mca_biplot(h1.1, select.ind = list(contrib = 5), select.var = list(contrib = 5), ggtheme = theme_minimal(), repel = TRUE, col.var = "contrib")


#This is the KAP2 PCA index

kap2pca <- Surveys[,c(20:21,23:24, 27)]
View(kap2pca)

pairs.panels(kap2pca, gap = 0, pch = 21)

h2.1 <- PCA(kap2pca, ncp = 5, graph = T)
summary(h2.1)

h2pred <- predict.PCA(h2.1, newdata = kap2pca)

df2 <- data.frame(h2pred, stringsAsFactors = TRUE)

df2.1 <- df2 %>% select(1)

df2.2 <- df2 %>% select(2)

corrmatrix1 <- as.data.frame(round(cor(kap2pca, h2.1$ind$coord)^2*100,0))

corrmatrix1[with(corrmatrix1, order(-corrmatrix1[,1])),]

#Visualization of the PCA results

fviz_eig(h2.1, addlabels = TRUE, ylim = c(0, 35))

kap2var <- get_pca_var(h2.1)
corrplot(kap2var$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_pca_var(h2.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h2.1, choice = "var", axes = 1:2, top = 10)

fviz_pca_ind(h2.1, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
)

#Figure transformation for the PCM1 models
tiff("MCA1.tiff",width=5, height=13, units="in", res=300)

p1 <- fviz_mca_var(h1.1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10)) + labs(title = "AP1 variable categories - MCA")

p2 <- fviz_mca_var(h1.1, choice = "mca.cor",repel = TRUE, ggtheme = theme_minimal()) + labs(title = "AP1 variable - MCA")

p3 <- fviz_pca_var(h2.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "AP2 variable - PCA")

gridExtra::grid.arrange(p1,p2,p3, ncol = 1)

dev.off()

#This is the Yard FMAD index

yardpca <- Surveys[,c(29,31:37)]
View(yardpca)

pairs.panels(yardpca, gap = 0, pch = 21)

h3.1 <- FAMD(yardpca, ncp = 5, graph = F)
summary(h3.1)

h3pred <- predict.FAMD(h3.1, newdata = yardpca)

df3 <- data.frame(h3pred, stringsAsFactors = TRUE)

df3.1 <- df3 %>% select(1)

df3.2 <- df3 %>% select(2)

h3.1$quali.var[["coord"]]
h3.1$quali.var[["contrib"]]
h3.1$quali.var[["cos2"]]

#Visualization of the FMAD results
fviz_eig(h3.1, addlabels = TRUE, ylim = c(0, 18))

yardvar <- get_famd_var(h3.1)

corrplot(yardvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_contrib(h3.1, choice = "var", axes = 1:2, top = 10)

fviz_famd_var(h3.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Yard quantitative variables - FAMD")

fviz_famd_var(h3.1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10)) + labs(title = "Yard variables - FAMD")

fviz_famd_var(h3.1, "quali.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10)) + labs(title = "Yard variable categories - FAMD")

#Figure transformation for the PCM2 models

tiff("Yard.tiff",width=5, height=13, units="in", res=300)

p4 <- fviz_famd_var(h3.1, "quali.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10)) + labs(title = "Yard variable categories - FAMD")

p5 <- fviz_famd_var(h3.1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10)) + labs(title = "Yard variables - FAMD")

p6 <- fviz_famd_var(h3.1, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Yard quantitative variables - FAMD")

gridExtra::grid.arrange(p4,p5,p6)

dev.off()


#This is the Window PCA index

winpca <- Surveys[,c(41:48)]
View(winpca)

pairs.panels(winpca, gap = 0, pch = 21)

h4.1 <- PCA(winpca, ncp = 5, graph = F)
summary(h4.1)

h4pred <- predict.PCA(h4.1, newdata = winpca)

df4 <- data.frame(h4pred, stringsAsFactors = TRUE)

df4.1 <- df4 %>% select(1)

df4.2 <- df4 %>% select(2)

corrmatrix2 <- as.data.frame(round(cor(winpca, h4.1$ind$coord)^2*100,0))

corrmatrix2[with(corrmatrix2, order(-corrmatrix2[,1])),]

#Visualization of the PCA results
fviz_eig(h4.1, addlabels = TRUE, ylim = c(0, 30))

winvar <- get_pca_var(h4.1)
corrplot(winvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_pca_var(h4.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Window variables - PCA")

fviz_contrib(h4.1, choice = "var", axes = 1:2, top = 10)

fviz_pca_ind(h4.1, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
) + labs(title = "Window variable - PCA")

#This is the Door PCA index

doorpca <- Surveys[,c(49:56)]
View(doorpca)

pairs.panels(doorpca, gap = 0, pch = 21)

h5.1 <- PCA(doorpca, scale.unit = T, ncp = 5, graph = F)
summary(h5.1)

h5pred <- predict.PCA(h5.1, newdata = doorpca)

df5 <- data.frame(h5pred, stringsAsFactors = TRUE)

df5.1 <- df5 %>% select(1)

df5.2 <- df5 %>% select(2)

corrmatrix3 <- as.data.frame(round(cor(doorpca, h5.1$ind$coord)^2*100,0))

corrmatrix3[with(corrmatrix3, order(-corrmatrix3[,1])),]
h5.1$var[["coord"]]
#Visualization of the PCA results

fviz_eig(h5.1, addlabels = TRUE, ylim = c(0, 30))

doorvar <- get_pca_var(h5.1)
corrplot(doorvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_pca_var(h5.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Door variables - PCA")

fviz_contrib(h5.1, choice = "var", axes = 1, top = 10)

fviz_pca_ind(h5.1, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
) + labs(title = "Door variables - PCA")

#Figure transformation for the PCM models
tiff("WindowDoor.tiff",width=10, height=4.5, units="in", res=300)

p7 <- fviz_pca_var(h4.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Window variables - PCA")

p8 <- fviz_pca_var(h5.1, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + labs(title = "Door variables - PCA")

gridExtra::grid.arrange(p7,p8, nrow = 1)

dev.off()
