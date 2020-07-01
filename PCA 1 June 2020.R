# PCA analyses for the AGO surveillance of Ae. aegypti in the LRGV

library(FactoMineR)
library(factoextra)



#I need to recode the different variables for window and door just to have yes and no for quality purposes. 

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



