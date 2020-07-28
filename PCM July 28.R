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

#Figure transformation for the PCM2 models
tiff("MCA1.tiff",width=13.5, height=6, units="in", res=300)

p5 <- fviz_mca_var(h1.1, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, select.var = list(cos2 = 10))+ labs(title = "AP1 variable categories - MCA")

p6 <- fviz_mca_var(h1.1, choice = "mca.cor",repel = TRUE, ggtheme = theme_minimal()) + labs(title = "AP1 variable - MCA")

gridExtra::grid.arrange(p5,p6, nrow = 1)

dev.off()

#This is the KAP2 PCA index

kap2pca <- Surveys[,c(20:21,23:24, 27)]
View(kap2pca)

h2.1 <- PCA(kap2pca, ncp = 5, graph = T)
summary(h2.1)

h2pred <- predict.PCA(h2.1, newdata = kap2pca)

df2 <- data.frame(h2pred, stringsAsFactors = TRUE)

df2.1 <- df2 %>% select(1)

df2.2 <- df2 %>% select(2)

corrmatrix <- as.data.frame(round(cor(kap2pca, h2.1$ind$coord)^2*100,0))

corrmatrix[with(corrmatrix, order(-corrmatrix[,1])),]



