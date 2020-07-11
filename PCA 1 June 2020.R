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

Surveys <- read_excel("RiskFactors1July.xlsx", 
                      sheet = "Surveys")

#This is the Window PCA index

winpca <- Surveys[,c(40:47)]
View(winpca)

h1 <- princomp(winpca, cor = T)
summary(h1)

loadings(h1)

h1pred <- predict(h1)[,1]

#Visualization of the PCA results
fviz_eig(h1, addlabels = TRUE, ylim = c(0, 30))

winvar <- get_pca_var(h1)
corrplot(winvar$cos2, is.corr = FALSE) #visualization of the cos2 of variables on all dimensions

fviz_pca_var(h1, col.var = "cos2", alpha.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_contrib(h1, choice = "var", axes = 1:2, top = 10)

fviz_pca_ind(h1, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
)




h1.1 <- PCA(winpca, scale.unit = T, ncp = 5, graph = F)
summary(h1.1)

eig.val <- get_eigenvalue(h1.1) 
eig.val



h1.1$call$ecart.type  #"standard error of the variables"



res.desc <- dimdesc(h1.1, axes = c(1,2), proba = 0.05) # Description of dimension 1 
res.desc$Dim.1

fviz_pca_ind(h1.1)

fviz_pca_ind(h1.1, pointsize = "cos2", pointshape = 21, fill = "#E7B800", repel = TRUE # Avoid text overlapping (slow if many points) 
             )
fviz_pca_ind(h1, col.ind = "contrib", pointsize = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE # Avoid text overlapping (slow if many points) 
             )
fviz_pca_ind(h1.1, geom.ind = "point", # show points only (nbut not "text") 
             col.ind = Surveys$Community, # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "lightgreen","red","black","gray","lightblue"), addEllipses = TRUE, # Concentration ellipses 
             legend.title = "Groups" 
             )


fviz_pca_biplot(h1.1, # Fill individuals by groups 
                geom.ind = "point", pointshape = 21, pointsize = 2.5, fill.ind = Surveys$Income, col.ind = "black", # Color variable by groups 
                legend.title = list(fill = "Income", color = "Clusters"), repel = TRUE
                # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+
  # Indiviual fill color
  ggpubr::color_palette("npg")

#Save the results into a single CSV file
write.infile(h1.1, "pca.csv", sep = "\t")

loadings(h1)
            
predict(h1)[,1]











res.famd1 <- FAMD(KAPpca1, graph = FALSE)
summary(res.famd1)

var <- get_famd_var(res.famd1)
# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)

var2 <- get_famd_ind(res.famd1)
# Coordinates of variables
head(var2$coord)
# Cos2: quality of representation on the factore map
head(var2$cos2)
# Contributions to the  dimensions
head(var2$contrib)




df <- predict(res.famd1, newdata = KAPpca1)
?predict

get_eigenvalue(res.famd1)
fviz_screeplot(res.famd1)
fviz_famd_var(res.famd1, repel = TRUE)
fviz_contrib(res.famd1, "var", axes = 1)
fviz_contrib(res.famd1, "var", axes = 2)
fviz_famd_var(res.famd1, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_famd_var(res.famd1, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


predict(res.famd1, newdata = Surveys)

fviz_eig(res.famd1)
get_famd_ind(res.famd1)




fviz_famd_ind(res.famd1) 
fviz_famd_var(res.famd1)



