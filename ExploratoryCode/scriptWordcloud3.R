# Analyse a set of budget speeches.
#

library(tm)
library(wordcloud)
library(FactoMineR)

# define folder to use
#txt2="c:/speeches"
#txt2="c:/mine/speeches2"
setwd("G:/MyDataAnalysis/analyzeBudgetSpeeches")
dataDir <- paste(getwd(),"/Federal", sep="")
#dataDir <- paste(getwd(),"/Federal_Con", sep="")
#dataDir <- paste(getwd(),"/Federal_Lib", sep="")

# put all the files into a single corpus
b <- Corpus(DirSource(dataDir))

# filter out stuff that just confuses the analysis
b <- tm_map(b, tolower)             	#Changes case to lower case
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

# matrix for the most popular words
numWords = 20
popularWords <- matrix(nrow=numWords, ncol=length(fileList))
colnames(popularWords)<-fileList
colCount <- 1
for (fileName in fileList)
{
  termFrequency <- rowSums(as.matrix(tdm[,tdm$dimnames$Docs==fileName]))
  termFrequency.matrix <- as.matrix(rowSums(as.matrix(tdm[,tdm$dimnames$Docs==fileName])))
  wf = rowSums(as.matrix(termFrequency))
  wf = rowSums(termFrequency.matrix)
  termFrequency2 = termFrequency.matrix[wf>quantile(wf,probs=.997),]
  #termFrequency2 = termFrequency[wf>quantile(wf,probs=.97),]
  v1<- sort(termFrequency2,decreasing=TRUE)
  barplot(v1, las=2, main=fileName, horiz=TRUE)
  
  popularWords[, colCount] <- names(v1[1:numWords])
  colCount <- colCount + 1
}

# show the top-20 words for all the speeches
popularWords


# try a correspondence analysis, usng FactoMineR
# in our case, in allows us to see how "similar" the 
# different budget speeches are
rei_ca=CA(as.matrix(tdm),graph=FALSE)

# summary of the objects available for analysis
print.CA(rei_ca)
str(rei_ca)

# let's look at details of the dimensions
rei_ca$eig
rei_ca$col$coord
rei_ca$col$inertia

# plot(rei_ca)

# plot(rei_ca$row$coord, type="n", xaxt="n", yaxt="n",
#      xlab="", ylab="")
# text(rei_ca$row$coord[,1],rei_ca$row$coord[,2], 
#      labels=rownames(as.matrix(tdm)),
#      col=hsv(0,0,0.6,0.5))

plot.CA(rei_ca, type="n", xaxt="n", yaxt="n",
        xlab="", ylab="", title="correspondence analysis",
        cex=2,
        pch=16,
        invisible="row")



# run a description of the dimensions of the MCA
# NOTE : the $col data is not in same order between dimensions!
#        this makes plotting a bit tricky!
dd <- dimdesc(rei_ca)
dd$`Dim 1`$col
dd$`Dim 2`$col

# show the dimensional data
dd$`Dim 1`$col
dd$`Dim 2`$col
dd$`Dim 3`$col


#dd$`Dim 3`$col['flaherty_budget2006.txt',]
rnm<-dimnames(dd$`Dim 1`$col)

# plot(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], 
#      col=dd$`Dim 3`$col[rnm[[1]],])

# plot as points
d1<-format(rei_ca$eig[[2]][1], digits=4)
d2<-format(rei_ca$eig[[2]][2], digits=4)
xLabel <- paste("Dim-1 : ", d1, "% of variance")
yLabel <- paste("Dim-2 : ", d2, "% of variance")
plot(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], 
     #     col=1:8,
     col=5:length(dd$`Dim 3`$col[rnm[[1]],]),
     cex=5.6,
     pch=16, 
     type="p",
     xlab=xLabel, ylab=yLabel,
     main="Correspondence Analysis (arbitrary colouring)")
# add axis lines
abline(0,0)
abline(0,10000)
# put the years into the dots
# NOTE : this currently assumes a fixed format for the file names! (xxx_YYYY.txt)
labels <- as.vector(substr(rnm[[1]], 5, 8))
text(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], labels, col='black')

# put up a legend
legNames <- rnm[[1]]
totNames <- length(legNames)
halfNames <- totNames / 2
legend(-.75, max(dd$`Dim 2`$col[legNames[1:halfNames],]), 
       legNames[1:halfNames],
#       col=5:length(dd$`Dim 3`$col[legNames[1:halfNames],]),
       col=5:halfNames,
       cex=0.8,
       pch=16)
legend(-.25, max(dd$`Dim 2`$col[legNames[1:halfNames],]), 
       legNames[(halfNames+1):totNames],
#       col=(6+length(dd$`Dim 3`$col[legNames[halfNames],])):length(dd$`Dim 3`$col[legNames[(halfNames+1):totNames],]),
       col=(halfNames+5):totNames,
       cex=0.8,
       pch=16)


# now plot with colours indicating DIM-3, and with a legend
plot(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], 
#     col=1:8,
#     col=7+round(abs(10*(dd$`Dim 3`$col[rnm[[1]],]))),
     col=7+round(16+(10*(dd$`Dim 3`$col[rnm[[1]],]))),
     cex=5.6,
     pch=16, 
     type="p",
     xlab=xLabel, ylab=yLabel,
     main="Correspondence Analysis - colour as Dim-3")

# add axis lines
abline(0,0)
abline(0,10000)

#text(dd$`Dim 1`$col[1:8], dd$`Dim 2`$col[1:8])
#text(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], labels=rnm[[1]])

# add a legend
#legend(min(dd$`Dim 1`$col[rnm[[1]],]), max(dd$`Dim 2`$col[rnm[[1]],]), 
legNames <- rnm[[1]]
totNames <- length(legNames)
halfNames <- totNames / 2

legend(-.75, max(dd$`Dim 2`$col[legNames[1:halfNames],]), 
       legNames[1:halfNames],
       #       col=5:length(dd$`Dim 3`$col[legNames[1:halfNames],]),
       col=7+round(16+(10*(dd$`Dim 3`$col[legNames[1:halfNames],]))),
       cex=0.8,
       pch=16)
legend(-.25, max(dd$`Dim 2`$col[legNames[1:halfNames],]), 
       legNames[(halfNames+1):totNames],
       #       col=(6+length(dd$`Dim 3`$col[legNames[halfNames],])):length(dd$`Dim 3`$col[legNames[(halfNames+1):totNames],]),
       col=7+round(16+(10*(dd$`Dim 3`$col[legNames[(halfNames+1):totNames],]))),
       cex=0.8,
       pch=16)



# legend(.1, max(dd$`Dim 2`$col[rnm[[1]],]), 
#        rnm[[1]],
# #       col=1:length(dd$`Dim 3`$col[rnm[[1]],]),
# #       col=10*(4+dd$`Dim 3`$col[rnm[[1]],]),
#        col=7+round(16+(10*(dd$`Dim 3`$col[rnm[[1]],]))),
#        cex=0.8,
#        pch=16)
# #       lty=c(1,1))

# put the years into the dots
# NOTE : this currently assumes a fixed format for the file names! (xxx_YYYY.txt)
labels <- as.vector(substr(rnm[[1]], 5, 8))
text(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], labels, col='black')

# x1<-c(0,0)
# y1<-c(min(dd$`Dim 2`$col[rnm[[1]],]), max(dd$`Dim 2`$col[rnm[[1]],]))
# lines(x1,y1)

#ty<- (max(dd$`Dim 2`$col[rnm[[1]],]) + min(dd$`Dim 2`$col[rnm[[1]],])) / 2
#text(min(dd$`Dim 1`$col[rnm[[1]],]), .5:1.5, labels=rnm[[1]])


# plot as points
# colour by party
d1<-format(rei_ca$eig[[2]][1], digits=4)
d2<-format(rei_ca$eig[[2]][2], digits=4)
partyColour <- substr(rnm[[1]],1,1)
partyColour<-sub("C","blue",partyColour)
partyColour<-sub("L","red",partyColour)
xLabel <- paste("Dim-1 : ", d1, "% of variance")
yLabel <- paste("Dim-2 : ", d2, "% of variance")
plot(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], 
     #     col=1:8,
     col=partyColour,
     cex=3,
#     cex=5.6,
     pch=16, 
     type="p",
     xlab=xLabel, ylab=yLabel,
     main="Correspondence Analysis (party colouring)")
# add axis lines
abline(0,0)
abline(0,10000)
# put the years into the dots
# NOTE : this currently assumes a fixed format for the file names! (xxx_YYYY.txt)
labels <- as.vector(substr(rnm[[1]], 5, 8))
text(dd$`Dim 1`$col[rnm[[1]],], 
     dd$`Dim 2`$col[rnm[[1]],], 
     labels, 
     col='white',
     cex=.5)

# add lines showing progress-path over time
lines(dd$`Dim 1`$col[sort(rnm[[1]]),], 
      dd$`Dim 2`$col[sort(rnm[[1]]),], 
      col='green',
      cex=3)



# let's look at associations (ie. terms which correlate)
# findAssocs(tdm, "tax", 0.9)
# findAssocs(tdm, "climate", 0.9)
# findAssocs(tdm, "environment", 0.9)
# findAssocs(tdm, "science", 0.9)
# findAssocs(tdm, "pension", 0.9)
# findAssocs(tdm, "commonsense", 0.9)
# findAssocs(tdm, "accountability", 0.9)
# findAssocs(tdm, "deficit", 0.9)
# findAssocs(tdm, "recession", 0.9)
# findAssocs(tdm, "banks", 0.9)
# findAssocs(tdm, "wisdom", 0.9)
# findAssocs(tdm, "sacrifice", 0.9)


# calculate the distance between each of the speeches
dim1 <- dd$`Dim 1`$col
dim1[rnm[[1]],]

#mat1 <- matrix(ncol=2, nrow=11)
#rownames(mat1) <- rnm[[1]]

# plot the cluster denogram based on the distances
# allX <- data.frame(c(dd$`Dim 1`$col,dd$`Dim 2`$col,dd$`Dim 3`$col))
# allDist<-dist(allX, method="euclidean")
# fitAll<-hclust(allDist, method="ward")
# plot(fitAll)


# let's try to algorithmetically calculate clusters
d <- dist(rei_ca$col$coord, method="euclidian")
fit <- hclust(d, method="ward")
plot(fit, xlab="", ylab="", 
     main="cluster denogram based on dim-value distance",
     sub="")

# cut the tree into 6 clusters
# then draw red borders around the clusters
groups <- cutree(fit, k=6)
rect.hclust(fit, k=6, border="red")
  
# print out the groups
sort(groups)

    