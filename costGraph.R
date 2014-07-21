rm(list=ls())
require(sqldf)
library(RCurl)

country <- getURL("https://raw.githubusercontent.com/thefactmachine/k-means/master/countryData.csv")
dfComposite <- read.csv(text = country)

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
p <- p + theme(panel.background = element_rect(fill = 'white', colour = 'blue'))
p + geom_line(aes(colour = "blue")) + geom_point(aes(colour = "red"))


