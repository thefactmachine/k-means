rm(list=ls())
require(sqldf)


library(RCurl)

euro <- getURL("https://raw.githubusercontent.com/thefactmachine/k-means/master/euroCountries.csv")
country <- getURL("https://raw.githubusercontent.com/thefactmachine/k-means/master/countryData.csv")

dfEuro <- read.csv(text = euro)
dfComposite <- read.csv(text = country)


#get complete / incomplete cases
dfComplete <- dfComposite[complete.cases(dfComposite),]
dfInComplete <- dfComposite[!complete.cases(dfComposite),]

fnReturnColoursEuro <- function(intClusterNumber) {
    blue = "#3958b7"
    red =  "#e0051a"
    green = "#4daf4a"
    purple = "#9a56db"
    brown = "#b26016"
    
    if (intClusterNumber == 2) {
        colors <- c(red, blue, green, purple, brown)
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
        colors <- c(purple, green , red, blue, brown)
    } 
    
    return(colors)
}

euroVector <- dfEuro$code
dfEuro <- subset(dfComplete, code %in% euroVector)
euroDataMat <- data.matrix(dfEuro[, -c(1,2)])
rownames(euroDataMat) <- dfEuro$code
# scale to mean = 0, sd =1. Matrix = 145 x 6
euroDataMat <- scale(euroDataMat)
#make sure Z value within specific parameter
vctLogical <- apply(euroDataMat, MARGIN =1, function(x) {all(abs(x) < 4)})
euroDataMat <- euroDataMat[vctLogical,]
# create covariance matrix
numClusterEuro <- 5
euroCurrentScheme <- fnReturnColoursEuro(numClusterEuro)

set.seed(456)
kmOutputEuro <- kmeans(euroDataMat, numClusterEuro, nstart = 100)
dfEuroCluster <- data.frame(code = names(kmOutputEuro$cluster), 
                            clusterNumber = kmOutputEuro$cluster)
sPDFEuro <- joinCountryData2Map(dfEuroCluster ,joinCode = "ISO3" ,
                                nameJoinColumn = "code" ,mapResolution = "high")
brks <- seq(from = 0.5, to = numClusterEuro + 0.5, by = 1)
mapParams <- mapCountryData(sPDFEuro,nameColumnToPlot="clusterNumber" ,addLegend=FALSE,
                            catMethod = brks,
                            borderCol = "#f2f2f2",
                            mapRegion = "eurasia",
                            missingCountryCol = "#bcbcbc",
                            lwd = 0.2,
                            colourPalette=euroCurrentScheme[1:numClusterEuro])
