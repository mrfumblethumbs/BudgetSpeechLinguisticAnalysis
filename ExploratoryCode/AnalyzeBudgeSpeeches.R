# 
# Analyse Budget Speeches
# 
# April 09/13 by bcg
#   - added saving to files for text and plot output
#
# April 05/13 by bcg
#   - first cut
# 

# set working and data directories
setwd("/home/brian1/MyDataAnalysis/analyzeBudgetSpeeches")

dataSet <- "Federal"
#dataSet <- "Federal_Con"
#dataSet <- "Federal_Lib"
#dataSet <- "Ontario"

dataDir <- paste(getwd(),"/", dataSet,sep="")
#dataDir <- paste(getwd(),"/Federal", sep="")
#dataDir <- paste(getwd(),"/Federal_Con", sep="")
#dataDir <- paste(getwd(),"/Federal_Lib", sep="")

# file name for text output
analysisFileName <- paste("Diagrams/", dataSet, "_analysis.txt",sep="")


# load the analysis utilities
source("ExploratoryCode/AnalysisUtilities.R")


# create a term document matrix from the corpus
budget.TDM <- createFilteredCorpus(dataDir)


# do correspondence analysis on the TDM
retVal <- correspondenceAnalysis(budget.TDM)
budget.CA <- retVal[[1]]
budget.CA.desc <- retVal[[2]]
budget.CA.names <- retVal[[3]]


sink(analysisFileName, append=FALSE)
textSummary <- paste("Total number of documents analysed in dataset ", dataSet, " : ", length(budget.CA.names[[1]]))
print(textSummary)
sink()


# examine eigenvalues
sink(analysisFileName, append=TRUE)
#sink("Diagrams/analysis.txt", append=FALSE)
print(budget.CA$eig, digits=4)
sink()

plotFileName <- paste("Diagrams/", dataSet, "_CA_eigenvalues.bmp",sep="")
bmp(plotFileName, width=7, height=7, units="in", res=300)
plot(budget.CA$eig, main="Correspondence Analysis Eigenvalues")
dev.off()


# do cluster denogram and plot it
# the "centroid" clustering seems to produce nicer clusters
# budget.deno <- distanceDenogram(budget.CA)
# budget.deno <- distanceDenogram(budget.CA, distMethod="manhattan")
# budget.deno <- distanceDenogram(budget.CA, distMethod="manhattan", hclustMethod="single")
budget.deno <- distanceDenogram(budget.CA, distMethod="euclidean", hclustMethod="centroid")

plotFileName <- paste("Diagrams/", dataSet, "_distanceDenogram.bmp",sep="")
bmp(plotFileName, width=7, height=4, units="in", res=300)
plotDistanceDenogram(budget.deno)
dev.off()

#plotDistanceDenogram(budget.deno, 8)


# what are the most popular words?
popWords <- mostPopularWords(budget.TDM, sort(budget.CA.names[[1]]), 20)
sink(analysisFileName, append=TRUE)
popWords
sink()

# plot the primary dimensions of the Correspondence Analysis
plotFileName <- paste("Diagrams/", dataSet, "_CA_partycolours.bmp",sep="")
bmp(plotFileName, width=7, height=7, units="in", res=300)
plotDimDescData(budget.CA, budget.CA.desc, budget.CA.names, progressLines=TRUE)
dev.off()



