#
# PCA Analysis
#
# April 03/13 by bcg
#   - first cut
#   - doesn't really work

library(tm)
library(wordcloud)

# define folder to use
#txt2="c:/speeches"
#txt2="c:/mine/speeches2"
setwd("G:/MyDataAnalysis/analyzeBudgetSpeeches")
#dataDir <- paste(getwd(),"/Federal", sep="")
dataDir <- paste(getwd(),"/Federal_Con", sep="")

# put all the files into a single corpus
b <- Corpus(DirSource(dataDir))

# filter out stuff that just confuses the analysis
b <- tm_map(b, tolower)               #Changes case to lower case
b <- tm_map(b, stripWhitespace) 			#Strips White Space
b <- tm_map(b, removeWords, stopwords("english"))
b <- tm_map(b, removePunctuation)

# create a term document matrix
tdm <- TermDocumentMatrix(b)
dtm <- DocumentTermMatrix(b)

# take a quick look at tdm
# shows that columns are individual documents
inspect(tdm[1:20, 1:5])


# create a barplot for the data in each of the budgets
# list of all the file names
fileList <- tdm$dimnames$Docs



# try a correspondence analysis, usng FactoMineR
# in our case, in allows us to see how "similar" the 
# different budget speeches are
library(FactoMineR)
rei_pca=PCA(as.matrix(tdm),graph=FALSE)

# summary of the objects available for analysis
print.PCA(rei_pca)
str(rei_pca)

# let's look at details of the dimensions
rei_pca$eig
rei_pca$col$coord
rei_pca$col$inertia


plot.PCA(rei_pca, type="n", xaxt="n", yaxt="n",
        xlab="", ylab="", title="PCA analysis",
        cex=1,
        pch=16,
        invisible="row")

# run a description of the dimensions of the MCA
# NOTE : the $col data is not in same order between dimensions!
#        this makes plotting a bit tricky!
dd <- dimdesc(rei_pca)

dd$`Dim.1`
dd$`Dim.2`
dd$`Dim.3`


rnm<-dimnames(dd$`Dim.2`$quanti)
d1<-format(rei_ca$eig[[2]][1], digits=4)
d2<-format(rei_ca$eig[[2]][2], digits=4)
xLabel <- paste("Dim-1 : ", d1, "% of variance")
yLabel <- paste("Dim-2 : ", d2, "% of variance")
plot(dd$`Dim.1`$quanti[rnm[[1]],], dd$`Dim.2`$quanti[rnm[[1]],], 
#     col=5:length(dd$`Dim.3`$quanti[rnm[[1]],]),
    cex=5.6,
    pch=16, 
    type="p",
    xlab=xLabel, ylab=yLabel,
   main="Correspondence Analysis (arbitrary colouring)")

# plot the cluster denogram based on the distances
allX <- data.frame(c(dd$`Dim.1`$quanti,dd$`Dim.2`$quanti,dd$`Dim.3`$quanti))
allDist<-dist(allX, method="euclidean")
fitAll<-hclust(allDist, method="ward")
plot(fitAll)

# let's try to algorithmetically calculate clusters
d <- dist(rei_pca$quanti, method="euclidian")
fit <- hclust(d, method="ward")
plot(fit, xlab="", ylab="", 
     main="cluster denogram based on quanti-value distance",
     sub="")


#
# a quick-and-dirty test of returning multiple values
# from a function
testfunc <- function()
{
  retValue <- list(1, 2, "blah", "zip", 5)
  return(retValue)
  
}

test1 <- testfunc()
str(test1)
test1[[1]]
test1[[2]]
test1[[3]]
test1[[4]]
test1[[5]]
