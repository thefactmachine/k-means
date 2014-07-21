setwd('/Users/zurich/Google Drive/CURRENT-TO-BE-MOVED/cluster')
rm(list=ls())
require(XLConnect)
require(sqldf)
#load Gender Data
wb <- loadWorkbook("Gender_Inequality.xlsx")
dfGender <- readWorksheet(wb, sheet = "GenderInequality", header = TRUE)
dfGender <- dfGender[,c(2,4,6)]
names(dfGender) <- c("name", "code", "genderInequality")
dfGender[dfGender$genderInequality == "..", c(3)] <- NA
dfGender$genderInequality <- as.numeric(dfGender$genderInequality)


#load Innovation Data
wb <- loadWorkbook("Innovation.xlsx")
dfInnovation <- readWorksheet(wb, sheet = "Innovation", header = TRUE)
dfInnovation <- dfInnovation[, c(2,4,5)]
names(dfInnovation) <- c("name", "code", "internetUser")
dfInnovation[dfInnovation$internetUser == "..", c(3)] <- NA
dfInnovation$internetUser <- as.numeric(dfInnovation$internetUser)

#GDP per capital 2011 to 2000.
wb <- loadWorkbook("GDP_per_capita.xlsx")
dfGDP <- readWorksheet(wb, sheet = "GDPPCapitaGrowth", header = TRUE)
names(dfGDP) <- c("name", "code", "GDPGrowth")
dfGDP[dfGDP$GDPGrowth == "..", c(3)] <- NA
dfGDP$GDPGrowth <- as.numeric(dfGDP$GDPGrowth)


#Population Growth, Median Age
wb <- loadWorkbook("Population_trends.xlsx")
dfPG <- readWorksheet(wb, sheet = "PTrends", header = TRUE)
dfPG <- dfPG[, c(1,2,5,6)]
names(dfPG) <- c("name", "code", "predPopGrowth", "medAge")
dfPG[dfPG$predPopGrowth == "..", c(3)] <- NA
dfPG[dfPG$medAge == "..", c(4)] <- NA
dfPG$predPopGrowth <- as.numeric(dfPG$predPopGrowth)
dfPG$medAge <- as.numeric(dfPG$medAge)



#load Social Data
wb <- loadWorkbook("Social_integration.xlsx")
dfSocial <- readWorksheet(wb, sheet = "Social", header = TRUE)
dfSocial <- dfSocial[, c(2,4,5)]
names(dfSocial) <- c("name", "code", "homicide")
dfSocial[dfSocial$homicide == "..", c(3)] <- NA
dfSocial$homicide <- as.numeric(dfSocial$homicide)

#load HDI (Human Development Index)
wb <- loadWorkbook("Human_Development_Index.xlsx")
dfHDI <- readWorksheet(wb, sheet = "HDI", header = TRUE)
dfHDI <- dfHDI[, c(2, 4, 5, 6, 7)]
names(dfHDI) <- c("name", "code", "lifeExp", "meanSchool", "gniPerCap")

#load Euro Countries 
wb <- loadWorkbook("EuroCountries.xlsx")
dfEuro <- readWorksheet(wb, sheet = "euro", header = TRUE)
names(dfEuro) <- c("name", "code", "euro")
dfEuro <- subset(dfEuro, euro == "YES")






#require(sqldf)
joinString <- "select dfHDI.*, dfSocial.homicide from dfHDI inner join 
dfSocial on dfHDI.code = dfSocial.code"
dfComposite <- sqldf(joinString)

joinString <- "select dfComposite.*, dfInnovation.internetUser from dfComposite 
inner join dfInnovation on dfComposite.code = dfInnovation.code"
dfComposite <- sqldf(joinString)

joinString <- "select dfComposite.*, dfGender.genderInequality from dfComposite 
inner join dfGender on dfComposite.code = dfGender.code"
dfComposite <- sqldf(joinString)

joinString <- "select dfComposite.*, dfPG.predPopGrowth, dfPG.medAge 
from dfComposite inner join dfPG on dfComposite.code = dfPG.code"
dfComposite <- sqldf(joinString)

joinString <- "select dfComposite.*, dfGDP.GDPGrowth
from dfComposite inner join dfGDP on dfComposite.code = dfGDP.code"
dfComposite <- sqldf(joinString)




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
