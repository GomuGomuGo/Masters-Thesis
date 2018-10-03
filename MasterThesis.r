Data <- read.csv("C:/Users/admin/OneDrive/Kidney.csv",na.strings = "?",stringsAsFactors=FALSE)


## Colnames of Data
# [1] "Age"                     "Blood_Pressure"          "Specific_Gravity"       
# [4] "Albumin"                 "Sugar"                   "Red_Blood_Cells"        
# [7] "Pus_Cell"                "Pus_Cell_Clumps"         "Bacteria"               
# [10] "Blood_Glucose_Random"    "Blood_Urea"              "Serum_Creatinine"       
# [13] "Sodium"                  "Potassium"               "Hemoglobin"             
# [16] "Packed_Cell_Volume"      "White_Blood_Cell_Count"  "Red_Blood_Cel_Count"    
# [19] "Hypertension"            "Diabetes_Mellitus"       "Coronary_Artery_Disease"
# [22] "Appetite"                "Pedal_Edema"             "Anemia"                 
# [25] "Class" 


## Recoding values of Specific_Gravity
Data[Data$Specific_Gravity %in% 1.005,]$Specific_Gravity <- 0
Data[Data$Specific_Gravity %in% 1.01,]$Specific_Gravity <- 0.25
Data[Data$Specific_Gravity %in% 1.015,]$Specific_Gravity <- 0.50
Data[Data$Specific_Gravity %in% 1.02,]$Specific_Gravity <- 0.75
Data[Data$Specific_Gravity %in% 1.025,]$Specific_Gravity <- 1

## Recoding the values of Albumin

Data[Data$Albumin %in% 1,]$Albumin <- 0.2
Data[Data$Albumin %in% 2,]$Albumin <- 0.4
Data[Data$Albumin %in% 3,]$Albumin <- 0.6
Data[Data$Albumin %in% 4,]$Albumin <- 0.8
Data[Data$Albumin %in% 5,]$Albumin <- 1.0

## Recoding the values of Sugar
Data[Data$Sugar %in% 1,]$Sugar <- 0.2
Data[Data$Sugar %in% 2,]$Sugar <- 0.4
Data[Data$Sugar %in% 3,]$Sugar <- 0.6
Data[Data$Sugar %in% 4,]$Sugar <- 0.8
Data[Data$Sugar %in% 5,]$Sugar <- 1.0

## Recoding RedBlood Cells
Data[Data$Red_Blood_Cells %in% "normal",]$Red_Blood_Cells <- "0"
Data[Data$Red_Blood_Cells %in% "abnormal",]$Red_Blood_Cells <- "1"
Data$Red_Blood_Cells <- as.integer(Data$Red_Blood_Cells)
## Recoding Pus_Cell
Data[Data$Pus_Cell %in% "normal",]$Pus_Cell <- "0"
Data[Data$Pus_Cell %in% "abnormal",]$Pus_Cell <- "1"
Data$Pus_Cell <- as.integer(Data$Pus_Cell)


## Reoding Pus_Cell_Clumps
Data[Data$Pus_Cell_Clumps %in% "notpresent",]$Pus_Cell_Clumps <- "0"
Data[Data$Pus_Cell_Clumps %in% "present",]$Pus_Cell_Clumps <- "1"
Data$Pus_Cell_Clumps <- as.integer(Data$Pus_Cell_Clumps)


## Reoding Bacteria
Data[Data$Bacteria %in% "notpresent",]$Bacteria <- "0"
Data[Data$Bacteria %in% "present",]$Bacteria <- "1"
Data$Bacteria <- as.integer(Data$Bacteria)


## Reoding Hypertension
Data[Data$Hypertension %in% "no",]$Hypertension <- "0"
Data[Data$Hypertension %in% "yes",]$Hypertension <- "1"
Data$Hypertension <- as.integer(Data$Hypertension)

## Recoding Diabetes_Mellitus
Data[Data$Diabetes_Mellitus %in% "no",]$Diabetes_Mellitus <- "0"
Data[Data$Diabetes_Mellitus %in% "yes",]$Diabetes_Mellitus <- "1"
Data[Data$Diabetes_Mellitus %in% " yes",]$Diabetes_Mellitus <- "1"
Data$Diabetes_Mellitus <- as.integer(Data$Diabetes_Mellitus)

## Recoding Coronary_Artery_Disease

Data[Data$Coronary_Artery_Disease %in% "no",]$Coronary_Artery_Disease <- "0"
Data[Data$Coronary_Artery_Disease %in% "yes",]$Coronary_Artery_Disease <- "1"
Data$Coronary_Artery_Disease <- as.integer(Data$Coronary_Artery_Disease)

## Recoding Appetite
## Good is coded as 1
## Poor is coded as 0
Data[Data$Appetite %in% "poor",]$Appetite <- "0"
Data[Data$Appetite %in% "good",]$Appetite <- "1"
Data$Appetite <- as.integer(Data$Appetite)

## Recoding Pedal_Edema
## Yes is coded as 1
## No is coded as 0
Data[Data$Pedal_Edema %in% "yes",]$Pedal_Edema <- "1"
Data[Data$Pedal_Edema %in% "no",]$Pedal_Edema <- "0"
Data$Pedal_Edema <- as.integer(Data$Pedal_Edema)

## Recoding Anemia
## Yes is coded as 1
## No is coded as 0
Data[Data$Anemia %in% "yes",]$Anemia <- "1"
Data[Data$Anemia %in% "no",]$Anemia <- "0"
Data$Anemia <- as.integer(Data$Anemia)

## Data for FeatureSelection
FeatureData <- Data
FeatureData <- scale(FeatureData)
FeatureData <- as.data.frame(FeatureData)
FeatureData$Class <- Data$Class
library("FSelector")
weights <- relief(Class~.,data= FeatureData,neighbours.count = 5, sample.size = 20)

## FactoredData, here Class in converted to Factor
FactoredData <- FeatureData
FactoredData$Class <- as.factor(FactoredData$Class)
weightsfactored <- relief(Class~.,data= FactoredData,neighbours.count = 5, sample.size = 20)


## ReliefData, here all the variables are converted to factors when they are factors
ReliefData <- Data
ReliefData$Specific_Gravity <- as.factor(ReliefData$Specific_Gravity)
ReliefData$Albumin <- as.factor(ReliefData$Albumin)
ReliefData$Sugar <- as.factor(ReliefData$Sugar)
ReliefData$Red_Blood_Cells <- as.factor(ReliefData$Red_Blood_Cells)
ReliefData$Pus_Cell <- as.factor(ReliefData$Pus_Cell)
ReliefData$Pus_Cell_Clumps <- as.factor(ReliefData$Pus_Cell_Clumps)
ReliefData$Bacteria <- as.factor(ReliefData$Bacteria)
ReliefData$Hypertension <- as.factor(ReliefData$Hypertension)
ReliefData$Diabetes_Mellitus <- as.factor(ReliefData$Diabetes_Mellitus)
ReliefData$Coronary_Artery_Disease <- as.factor(ReliefData$Coronary_Artery_Disease)
ReliefData$Appetite <- as.factor(ReliefData$Appetite)
ReliefData$Pedal_Edema <- as.factor(ReliefData$Pedal_Edema)
ReliefData$Anemia <- as.factor(ReliefData$Anemia)
ReliefData$Class <- as.factor(ReliefData$Class)

library("CORElearn")
estReliefF <- attrEval(Class ~ ., ReliefData, estimator="ReliefFexpRank", ReliefIterations=0)

a <- ReliefData
ReliefData[is.na(ReliefData$Bacteria),]$Bacteria <- 1

library("FSelector")
library("dplyr")
## Scaling Specific Variables
d <- ReliefData
d <- d %>% mutate_each_(funs(scale),vars=c("Age","Blood_Pressure","Blood_Glucose_Random", 
                                      "Blood_Urea","Serum_Creatinine","Sodium",
                                      "Potassium","Hemoglobin","Packed_Cell_Volume",
                                      "White_Blood_Cell_Count","Red_Blood_Cel_Count" )) 
## Relief feature selected Factors
set.seed(1994)
ReliefFactors <- relief(Class~.,data= ReliefData,neighbours.count = 30, sample.size = 20)
library("ggplot2")
## Ggplot for Feature relevance
ggplot(ReliefFactors, aes(x=row.names(ReliefFactors), y=attr_importance, 
                          fill=row.names(ReliefFactors))) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x = element_text(angle=90, vjust=1)) +
  geom_text(aes(label = round(attr_importance,4)),vjust = -0.5) + element_rect()



## Number of Missing Values Per Column
MissingValuesPerColumn <- c()
for(i in 1:25){
  a <- sum(is.na(Data[i]))
  MissingValuesPerColumn <- c(MissingValuesPerColumn,a)
}

MissingValues <- data.frame(colnames(Data),MissingValuesPerColumn)
colnames(MissingValues)[1] <- "Feature"
## GGPlot for missing avlues per feature
 ggplot(MissingValues, aes(x=Feature, y= NumofMissingValues, 
                          fill=Feature)) + 
  geom_bar(stat="identity", position="identity") +theme(axis.text.x = element_text(angle=90, vjust=1))+
  geom_text(aes(label = NumofMissingValues),vjust = -0.5) 
  
 

 ### GLM Model
 GLMData <- ReliefData[,c(-5,-13,-14)]
 GLMData <- na.omit(GLMData)
 GLMModel <- glm(Class ~., data = GLMData, family = "binomial",control = list(maxit = 150))
 BayesGLMModel <- bayesglm(Class ~., data = GLMData, family = "binomial")
 GLMNetModel <- glmnet(GLMData$Class,GLMData[,-22], family = "binomial")
 
 
 
 ### ReducedData, Data Manipulations are done here 
 ReducedData <- Data[,c(4,5,20,9,16,19,6,3,25)]
 ReducedData[is.na(ReducedData$Diabetes_Mellitus),]$Diabetes_Mellitus <- 0
 ReducedData[is.na(ReducedData$Hypertension),]$Hypertension <- 0
 
 
 library("doParallel")
 detectCores()
 
 
 
 ### Closing all the Existing Connections
 
 closeAllConnections()
 
 
 ### Making a Cluster  with 6 of 8 available cores
 
 Cluster <- makeCluster(8)
 ### Register tHe Cluster
 registerDoParallel(Cluster)
 
 
 ### library pracma has to be laoded 
 library("pacman")
 ### Load Multiple Packages in R
 pacman::p_load(dplyr,methods,FNN,xlsx,kohonen,graphics,epicalc,pracma,xlsxjars,sqldf,kknn,doParallel)
 
 
 MissingDataInstances <- function(Data){ ## Function begins here
   Data <- Data
   Row <- 1
   Column <- 1
   MissingRowIndices <- c()
   MissingColumnIndices <- c()
   for(Row in 1 : nrow(Data)){ ## Outer for loop begins here
     for(Column in 1:ncol(Data)){ ## Inner for loop begins here
       if(is.na(Data[Row,Column])){ ## If conditions starts here
         MissingRowIndices <- c(MissingRowIndices, Row)
         MissingColumnIndices <- c(MissingColumnIndices,Column)
       } ## if condition ends here
       
     } ## Inner for loop ends here
   } ##  Outer for loop ends here  
   MissingRowsandColumns <- list(MissingRowIndices,unique(MissingColumnIndices))
   return (MissingRowsandColumns)
 } ## Function ends here
 
 #######################################################################
 #                                                                   ###
 #                                                                   ###
 # Here, we are finding the row numbers  and column numbers          ###
 # that has NA values                                                ###
 #                                                                   ###
 #######################################################################
 
 RowsWithNAs <-  MissingDataInstances(ReducedData)[1]
 RowsWithNAs <- unlist(RowsWithNAs)
 
 
 ColumnsWithNas <- MissingDataInstances(ReducedData)[2]
 ColumnsWithNas <- unlist(ColumnsWithNas)
 


Class <- ReducedData$Class
#ReducedData <- ReducedData[,-8]
 
 
#ReducedData <- scale(ReducedData)
#ReducedData <- data.frame(ReducedData)
## Making a MissingDataFrame
#ReducedData$Class <- Class



## Hyperplanedataset is generated below.
## -1 in the below code should be replaced with ColumnsWithNas
HyperPlaneData <- ReducedData[,-ColumnsWithNas]  

#######################################################################
###
###
# Replacing NA's in missing data to zero, In this                   ###
# function, whereever NA's are found all the column                 ###
# values are replace with zeros                                     ###
#######################################################################

ReplaceMissingDataWithZeros <- function(Data){ ## Function begins here
  Class <- Data$Class ## Removing the Class Variable from the dataset
  Data <- Data[,-length(Data)]
  Row = 1
  Column = 1
  
  for(Row in 1:nrow(Data)){ ## Outer for loop begins here
    for(Column in 1:ncol(Data)){ ## Inner for loop begins here
      if(is.na(Data[Row,Column])){ ## if condition starts here
        for(i in 1: ncol(Data)){ ## Inner most forloop begins here
          Data[Row,i] <- 1000
        } ## if condition ends here
        
      } ## inner most for loop ends here
    } ## Inner for loop ends here
    
  } ## Outer for loop ends here
  Data$Class <- Class
  return(Data)
} ## Function ends here


#######################################################################
#                                                                   ###
#                                                                   ###
# This function returns the neighbors and takes Data a              ###
# dataframe as a input                                              ###
#                                                                   ###
#######################################################################

returnNeighbors <- function(Data1,k){
  Data <- Data1
  NearestNeigbors <- knn.index(Data[,-length(Data)], k)
  ###return (as.array(x))
  return (NearestNeigbors)
  
}

#### MissingData is manipulated using ReplaceMissingDataWithZeros
ReducedData <- ReplaceMissingDataWithZeros(ReducedData)

### Neighbors are found using returnneighbors function
MissingDataNeighbors <- returnNeighbors(ReducedData,399)


#######################################################################
#                                                                   ###
#                                                                   ###
# Here, Missing rows are removed from the   MissingDataNeighbors    ###
# matrix and  the columns that contains the missing row values are  ###
# removed.                                                          ###
# 1:nrow(MissingData)-1 - length(RowsWithNAs)                       ###
# nrow(MissingData)-1  is number of neighbors                       ### 
# length(RowsWithNAs)  is the number of rows with NA's              ###
#######################################################################
NAsRemovedNeighbours <-  MissingDataNeighbors[-RowsWithNAs, 1:((nrow(ReducedData)-1) - length(RowsWithNAs))]

## HyperPlane neighbors are calculated
HyperPlaneNeighbors <- returnNeighbors(HyperPlaneData,20)

#######################################################################
#                                                                   ###
#                                                                   ###
# Missing Plane Neighbors are the ones which has row with Missing   ###
# values                                                            ###
#                                                                   ###
#######################################################################
MissingPlaneNeighbors <- HyperPlaneNeighbors[RowsWithNAs,]




#######################################################################
#                                                                   ###
#                                                                   ###
# SphereProbabilities function calculates the probabilities of a    ###
# missing point belonging to the sphere.                            ###
# This function takes individual rows of NAsRemovedNeighbours and   ###
# and "b" as functton arguments and returns the array of            ###
# probabilities                                                     ###
#######################################################################

SphereProbabilities <- function(IndividualNeighbors, c,MissingPlaneNeighbors1){ ## function begins here
  MissingPlaneNeighbors1 <- MissingPlaneNeighbors1
  
  IndividualNeighbors <- IndividualNeighbors ## individual row of NAsRemovedNeighbours matrix
  c <- c ## Number of neighbors to be checked in each sphere, "b" in Saurabh's dissertation
  SpherePositions <- c()
  
  ## Missing Row is the index of the row having missing values in original dataset
  for(MissingRow in 1:nrow(MissingPlaneNeighbors1)){ ## Outer for loop begins here
    b <- MissingPlaneNeighbors1[MissingRow,1:c]
    Spheres <- split(IndividualNeighbors, ceiling(seq_along(IndividualNeighbors)/5))
    ## 5 in the above line is the size of each sphere
    Probabilities <- c()
    for (SphereNumber in 1 : length(Spheres)){ ## for loop begins here
      
      Sum <- 0
      Sum <- sum(b %in% (unlist(Spheres[SphereNumber],use.names = FALSE)))
      Probabilities <- c(Probabilities,Sum)
    } ## for loop ends here
    Position <- which.max(Probabilities) ## Individual position of eachin missing row index
    SpherePositions <- c(SpherePositions,Position)  ##  SpherePositions are the positions for
    ## all the mising row indices for individual rows in original dataset containing non missing data
    ##Probabilities1 <- rbind(Probabilities1,Probabilities)
  } ## Outer for loop ends here
  return (SpherePositions)  ## function returns the array of probabilities of individual Spheres
} ## function ends here


SphereProbabilities4EntireDataSet <- foreach(Row = 1:nrow(NAsRemovedNeighbours),.combine = rbind) %dopar% SphereProbabilities(NAsRemovedNeighbours[Row,],20,MissingPlaneNeighbors)

#######################################################################
#                                                                   ###
#                                                                   ###
# This function puts the hyper plane neighbors into the NA's rem    ###
# obed neighbor dataset.                                            ###
# This function takes individual rows of NAsRemovedNeighbours and   ###
# and "Hyperplane neighbors" and Spherepsotions as arguments        ###
#  and returns modified dataset                                     ###
#######################################################################



InsertHyperPlaneNeighbors <- function(OriginalDataset,HyperPlaneNeighbors,SpherePosition){ ## Function begins here
  OriginalDataset <- OriginalDataset
  HyperPlaneNeighbors <- HyperPlaneNeighbors
  SpherePosition <- SpherePosition
  for(i in 1:length(HyperPlaneNeighbors)){ ## For loop begins here
    Position = 5 * SpherePosition[i]  ## 5 is used because number of points in each Sphere is 5
    OriginalDataset <- append(OriginalDataset,HyperPlaneNeighbors[i],Position)
  } ## for loop ends here
  return(OriginalDataset)
} ## Function ends here


#######################################################################
#                                                                   ###
#                                                                   ###
# This function puts the hyper plane neighbors into the NA's rem    ###
# obed neighbor dataset.                                            ###
# This function takes individual rows of NAsRemovedNeighbours and   ###
# and "Hyperplane neighbors" and Spherepsotions as arguments        ###
#  and returns modified dataset                                     ###
#######################################################################

ModifiedDatasetwithInsertedNeighbors <- function(OriginalDataset,HyperPlaneNeighbors,SpherePosition){ ## function begins here
  OriginalDataset <- OriginalDataset
  HyperPlaneNeighbors <- HyperPlaneNeighbors  ## here, HyperPlaneNeighbors is same as RowsWithNAs
  SpherePosition <- SpherePosition
  ModifiedDataset <- foreach(Row = 1:nrow(OriginalDataset),.combine = rbind,.export = c("InsertHyperPlaneNeighbors")) %dopar% InsertHyperPlaneNeighbors(OriginalDataset[Row,],HyperPlaneNeighbors,SpherePosition[Row,])
  MissingPlaneNeighbors <- returnNeighbors(Data1 = HyperPlaneData,399)  ## Neighbors when AGe column is removed
  MissingRowsNeighbors <- MissingPlaneNeighbors[HyperPlaneNeighbors,]
  for(i in 1:length(HyperPlaneNeighbors)){ ## for loop begins here
    FirstHalf <- ModifiedDataset[1:((HyperPlaneNeighbors[i]) - 1),]
    SecondHalf <- ModifiedDataset[(HyperPlaneNeighbors[i]):nrow(ModifiedDataset),]
    FirstHalf <- rbind(FirstHalf,MissingRowsNeighbors[i,])
    ModifiedDataset <- rbind(FirstHalf,SecondHalf)
    
  } ## for loop ends here
  return(ModifiedDataset)
} ## function ends here


###  EntireDataSetNeighbors is matrix that has HyperPlane Neighbors and the  MissingRow
###  numbers inserted into the NeighborMatrix that has missing rows missed
EntireDataSetNeighbors <- ModifiedDatasetwithInsertedNeighbors(NAsRemovedNeighbours,RowsWithNAs,SphereProbabilities4EntireDataSet)



## BELOW FUNCTION RETURNS THE CLASS OF  EACH NEIGHBORING ELEMENT

NearestNeighborClass <- function(Data1){
  Data <- Data1
  
  ## Get the Nearest Neighbors from returnNeighbors Function
  NearestNeighbors <- EntireDataSetNeighbors
  
  ## Create the NearestNeighbor Class from Data and NearestNeighbors
  NearestNeighborClass <- matrix(,nrow(Data),nrow(Data))
  NearestNeighborClass[,1] <- Data$Class
  for(row in 1:nrow(Data)){
    for(column in 2:nrow(Data)){
      NearestNeighborClass[row, column] <- Data$Class[NearestNeighbors[row,column - 1]]
    }
  }
  return (NearestNeighborClass)
}


### Matrix with sum of neighboring class elements

### BELOW FUNCTION RETURNS THE TOTAL NUMBER  OF SUCCESSE FOR EVERY ITERATION AND RETURNS A MATRIX 
### WITH THE SUM OF THE SUCCESSE FOR EVERY ITERATION IN EVERY CELL

NearestNeighborClassSuccesses <- function (Data){
  Data <- Data
  
  NearestNeighborClass <- NearestNeighborClass(Data)
  
  NearestNeighborClassSuccesses <- foreach(row = 1:nrow(Data), .combine = rbind) %:%
    foreach(size = seq(10,nrow(Data),10),.combine = cbind) %dopar% sum(NearestNeighborClass[row,1:size])
  
  return(NearestNeighborClassSuccesses)
}

### This function calculates the number of trials required
NearestNeighborClassTrials <- function(Data){
  Data <- Data
  ### Repating the every with sequence starting from 10 nrow(Data) -1
  
  NearestNeighborClassTrials<- matrix(seq(10,nrow(Data), 10), nrow(Data), length(seq(10,nrow(Data), 10)),byrow = TRUE)
  return (NearestNeighborClassTrials)
  
}


FinalElbowPointCoordinates_FullDataSet <- function(curve,Data){
  Data <- Data
  FinalCoordinates <- data.frame()
  for(i in seq(round(nrow(Data)/2,0),nrow(Data),10)){
    XCoordinate <- c()
    YCoordinate <- c()
    TotalTrials <- c()
    ##count = 0
    a <- ElbowPoint_FullDataSet(i,curve)
    XCoordinate <- c(XCoordinate,a[1])
    YCoordinate <- c(YCoordinate,a[2])
    TotalTrials <- c(TotalTrials,i)
    Coordinates <- data.frame(TotalTrials,XCoordinate,YCoordinate)
    FinalCoordinates <- rbind(FinalCoordinates,Coordinates)
    Coordin <<- FinalCoordinates
  }
  return (FinalCoordinates)
}

FunctionProbabilityMatrix <- function(Data){
  Data <- Data
  ## Getting the Suuccesse of every itereation for the sample from NearestNeighborClassSuccesses function
  x <- NearestNeighborClassSuccesses(Data)
  ## Getting the Suuccesse of every itereation for the sample from NearestNeighborClassTrials function
  z <- NearestNeighborClassTrials(Data)
  
  ProbabilityMatrix <- foreach(row = 1:nrow(Data), .combine = rbind) %:%
    foreach(size = 1: length( seq(10,nrow(Data),10)),.combine = cbind) %dopar% (sum(x[row,1:size])/sum(z[row,1:size]))
  return(ProbabilityMatrix)
  
}

AllElbowPoints_FullDataSet <- function(Data1, ProbabilityMatrix1){
  Data <-Data1
  ProbabilityMatrix <- ProbabilityMatrix1
  PatientsandElbowPoints <- data.frame()
  for (i in 1:nrow(ProbabilityMatrix)){
    curve <- unlist(ProbabilityMatrix[i,])
    
    
    #### FinalElbowPointCoordinates is called to calculate all the final coordinates
    ###  of a  single sample sample
    
    ### Here FinalCoordinates is a dataframe and  will be generated for every sample.
    
    FinalCoordinates <- FinalElbowPointCoordinates_FullDataSet(curve,Data)
    
    ### ElbowPointProbability is called to check for the convergence and also 
    ### this function returns individual probability at elbow points and the number of trials 
    ### required to obtain the same
    IndividualElbowPoint <- ElbowPointProbability_FullDataSet(FinalCoordinates)
    
    
    
    PatientsandElbowPoints <- rbind(PatientsandElbowPoints,IndividualElbowPoint)    
  }
  
  colnames(PatientsandElbowPoints) <- c("TotalTrials","Number of Patients","Probability")
  return (PatientsandElbowPoints)
  
}


ElbowPoint_FullDataSet <- function(n, curve){
  
  Trials <- seq(20,n,10)
  curve1 <- curve
  m <- length(Trials)
  curve <- curve1[1:m]
  nPoints = length(curve)
  
  colnames <- c("curve", "Trials")
  allCoord = data.frame(Trials, curve)
  
  ## pull out first point
  firstPoint = allCoord[1,]
  
  
  
  # get vector between first and last point - this is the line
  lineVec = tail(allCoord,n=1) - firstPoint
  
  allCoord <- as.matrix(allCoord)
  
  firstPoint = repmat(as.matrix(firstPoint), nPoints,1)
  
  
  # normalize the line vector
  lineVecN = lineVec / sqrt(lineVec[1]^2  + lineVec[2]^2)
  
  # find the distance from each point to the line:
  # vector between all points and first point
  library("pracma")
  
  ### Function that generates  Scalar Product at dimension level
  
  dotProduct <- function(x,y){
    a <- matrix()
    x <- x
    y <- y
    for(i in 1:nrow(x)){
      a[i] <- ((x[i,1] * y[i,1]) + (x[i,2] * y[i,2]))
      
    }
    return (as.matrix(a))
  }
  
  vecFromFirst = bsxfun("-", allCoord, firstPoint);
  
  
  ### vecFromFirst = bsxfun("-", allCoord, repmat(firstPoint,nPoints,1));
  scalarProduct = dotProduct(vecFromFirst, repmat(as.matrix(lineVecN),nPoints,1))
  
  
  
  
  vecFromFirstParallel = as.numeric(scalarProduct) * lineVecN
  
  
  ScalarMatrix <- function(x,y){
    a <- matrix(,length(x),2)
    x <- as.numeric(x)
    for(i in 1:length(x)){
      a[i,1] <- x[i] * y[1,1]
      a[i,2] <- x[i] * y[1,2]
    }
    return (a)
  }
  
  vecFromFirstParallel <- ScalarMatrix(scalarProduct, lineVecN )
  
  
  
  
  NormVectorLine <- function(x){
    a <- matrix()
    for(i in 1:nrow(x)){
      a[i] <- sqrt(x[i,1]^2  + x[i,2]^2)
    }
    return(as.matrix(a))
  }
  
  
  vecToLine = vecFromFirst - vecFromFirstParallel
  
  
  distToLine = NormVectorLine(vecToLine)
  
  
  
  
  
  BestPointIndex <- which(distToLine == max(distToLine), arr.ind = TRUE)
  BestPointIndexRow <- BestPointIndex[1,1]
  
  
  idxOfBestPoint = c(allCoord[BestPointIndexRow,1],
                     allCoord[BestPointIndexRow,2])
  
  return(idxOfBestPoint)
  
}


### Once the Entire set of FInal Coordinates are obtained
### Below function is called to check for the convergence
ElbowPointProbability_FullDataSet <- function(FinalCoordinates){
  counter = 0
  for(j in 2 : nrow(FinalCoordinates)){
    ElbowDifference  = FinalCoordinates$YCoordinate[j] -  FinalCoordinates$YCoordinate[j - 1]
    if(ElbowDifference == 0){
      counter = counter + 1
    }
    if (counter > 10) break
    
    
  }
  return  (c(FinalCoordinates$TotalTrials[j],FinalCoordinates$XCoordinate[j],FinalCoordinates$YCoordinate[j]))
}
ProbabilityMatrix_FullDataSet <- FunctionProbabilityMatrix(ReducedData)
FinalElbowPointsforSOM_FullDataSet <- AllElbowPoints_FullDataSet(ReducedData,ProbabilityMatrix_FullDataSet)

 

### ROC Analysis
library(pROC)
roc(ReducedData$Class ~ FinalElbowPointsforSOM_FullDataSet$Probability)  ## Proposed Method

GLMData <- Data[,c(1,4,9,20,16,19,6,3,25)]
GLMData <- na.omit(GLMData)
GLMData$Class <- as.factor(GLMData$Class)
GLMModel <- glm(Class ~., data = GLMData, family = "binomial")  ## GLMModel
roc(GLMData$Class ~ GLMModel$fitted.values)

## ROC Analysis with Symptom
roc(GLMData$Class ~ GLMModel$fitted.values)
roc(Data$Class ~ FinalElbowPointsforSOM_FullDataSet$Probability)
### Stopping the Cluster
stopImplicitCluster()
stopCluster(Cluster)




### Testing Purpose
### ReducedData, Data Manipulations are done here 

library("CORElearn")
estReliefF <- attrEval(Class ~ ., d, estimator="ReliefFexpRank", ReliefIterations=20)
estReliefF <- data.frame(estReliefF)
colnames(estReliefF) <- "Attribute_Importance"
## Ggplot for Feature relevance
ggplot(estReliefF, aes(x=row.names(estReliefF), y=Attribute_Importance, 
                          fill=row.names(estReliefF))) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x = element_text(angle=90, vjust=1)) +
  geom_text(aes(label = round(Attribute_Importance,4)),vjust = -0.5) + element_rect() + labs(x = "Feature")


