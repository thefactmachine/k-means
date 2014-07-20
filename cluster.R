rm(list=ls())
setwd('/Users/zurich/Google Drive/FactMachine-SITE/FM-Site-STRUCTURE/08-KMeans/code/k-means')
dfComplete <- read.csv('countryData.csv')
dataMat <- data.matrix(dfComplete[, -c(1,2)])
rownames(dataMat) <- dfComplete$code

fnReturnColours <- function(intClusterNumber) {
  blue = "#3958b7"
  red =  "#e0051a"
  green = "#4daf4a"
  purple = "#9a56db"
  brown = "#b26016"
  
  if (intClusterNumber == 2) {
    colors <- c(blue, red, green, purple, brown)
  }
  
  if (intClusterNumber == 3) {
    #1 = europe, #2 = russia, 3 = centAfr
    colors <- c(blue, red, brown, purple, green)
  }
  if (intClusterNumber == 4) {
    # 1 = europe, 2 = russia, 3 = brazil, 4 = central Afr
    colors <- c(blue, red, green, brown, purple)
  } 
  
  if (intClusterNumber == 5) {
    #1 = centralAfr, 2 = europe, 3 = india, 4 = brazil, 5 = russia
    colors <- c(brown, blue , green, purple, red)
  } 
  
  return(colors)
}

# scale to mean = 0, sd =1. Matrix = 145 x 6
dataMat <- scale(dataMat)
#dataMat <- dataMat[,1:3]

#make sure Z value within specific parameter
vctLogical <- apply(dataMat, MARGIN =1, function(x) {all(abs(x) < 4)})
dataMat <- dataMat[vctLogical,]

# create covariance matrix
covMat <- (t(dataMat) %*% dataMat) / nrow(dataMat)

# singular value decomposition svd$u = 6 x 6 
svd <- svd(covMat)
#svdReduction: [145 x 6] * [6 x 2] = 145 x 2
svdReduction <- dataMat %*% svd$u[,1:2]
svdReduction <-scale(svdReduction)
dfReduction <- data.frame(X = svdReduction[,1], Y = svdReduction[,2])
#p <- ggplot(dfReduction, aes(X, Y))
#p + geom_point()

############################################

numCluster <- 5
currentScheme <- fnReturnColours(numCluster)
set.seed(456)
kmOutput <- kmeans(dataMat, numCluster, nstart = 100)

#lets put the cluster vector in a dataframe
library(rworldmap)
library(rworldxtra)

dfCluster <- data.frame(code = names(kmOutput$cluster), 
                        clusterNumber = kmOutput$cluster)

sPDF <- joinCountryData2Map(dfCluster ,joinCode = "ISO3" ,
                            nameJoinColumn = "code" ,mapResolution = "coarse")

brks <- seq(from = 0.5, to = numCluster + 0.5, by = 1)
mapParams <- mapCountryData(sPDF,nameColumnToPlot="clusterNumber" ,addLegend=FALSE,
                            catMethod = brks,
                            borderCol = "#f2f2f2",
                            missingCountryCol = "#bcbcbc",
                            colourPalette=currentScheme[1:numCluster])



