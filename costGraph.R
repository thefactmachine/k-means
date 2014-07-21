
setwd('/Users/zurich/Google Drive/CURRENT-TO-BE-MOVED/cluster')
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

#create matrix from data frame.
dataMat <- data.matrix(dfComplete[, -c(1,2)])
rownames(dataMat) <- dfComplete$code



# scale to mean = 0, sd =1. Matrix = 145 x 6
dataMat <- scale(dataMat)
#dataMat <- dataMat[,1:3]

#make sure Z value within specific parameter
vctLogical <- apply(dataMat, MARGIN =1, function(x) {all(abs(x) < 4)})
dataMat <- dataMat[vctLogical,]








maxCluster <- 30
dfResult <- data.frame(matrix(nrow = maxCluster-1, ncol =2))
for (i in 2:maxCluster) {
    kmOutput <- kmeans(dataMat, i, nstart = 100)
    dfResult[i-1, 1] <- i
    dfResult[i-1, 2] <- kmOutput$tot.withinss
}

library(ggplot2)
p <- ggplot(dfResult, aes(X1, X2))
p + geom_line() + geom_point(aes(colour = "red"))




