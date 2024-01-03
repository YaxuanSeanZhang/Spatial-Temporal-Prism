#load R packages ---------
library(ggpubr)
library(Rfast)
library(Hmisc)
library(sf)
library(raster)
library(dplyr)
library(spData)
library( leaflet )
library(leaflet.extras)
library( magrittr )
library(lubridate)
library(xts)
library(TraMineR)
library(adehabitatHR)
library(ggplot2)
library(spatialEco)
library(factoextra)
library( magrittr )
library(gridExtra)
library(grid)
library(lattice)
library(mclust)
library(geosphere)
library(data.table)
library(intervals)
library(Ckmeans.1d.dp)
library(pracma)
library(randomForest)
library(caret)
library(cluster)
library(RColorBrewer)

source("_Spatial_Operation.R")
source("_Temporal_Operation.R")

#Step1: load data and data pre-processing----
source("Step1-Read-Data-Preprocessing.R")

#Step2: Spatial Fixity----
#use 'Work' Activity as example
activity_type = 'WO'
source("Step2-Spatial-Fixity.R")

#Step3: Temporal Fixity----
source("Step3-Temporal-Fixity.R")

#Step4: Spatio-Temporal Fixity----
source("Step4-Spatio-Temporal-Fixity.R")

#Step5: Spatio-Temporal Anchor and Summary Analysis----
source("Step5-Anchor-Summary.R")
