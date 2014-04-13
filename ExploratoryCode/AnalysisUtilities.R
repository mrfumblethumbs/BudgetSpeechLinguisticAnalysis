# 
# Analysis Utilities
#   - a series of functions relating to the analysis of the text files
# 
# April 20/13 by bcg
#  - fix to createFilteredCorpus() to read as UTF-8
# April 04/13 by bcg
#   - first cut


# =====  load required libraries  ================
library(tm)
library(wordcloud)
library(FactoMineR)


# ================================================
# =====  createFilteredTDM()  ====================
# ================================================
# 
# load all the data files into a single corpus,
# then apply a standard set of filters, then
# calculate a term document matrix.
# INPUT   : sourceDirectory (fully-qualified)
# RETURN  : term document matrix
#
createFilteredCorpus <- function(sourceDirectory)
{
  # load all the files into a single corpus
  b <- Corpus(DirSource(sourceDirectory, encoding="UTF-8"))
  
  # filter out stuff that just confuses the analysis
  # white space, stop words punctuation
  b <- tm_map(b, tolower)               #Changes case to lower case
  b <- tm_map(b, stripWhitespace) 			
  b <- tm_map(b, removeWords, stopwords("english"))
  b <- tm_map(b, removePunctuation)
  
  # create a term document matrix
  tdm <- TermDocumentMatrix(b)
  
  return(tdm)
  
}



# ================================================
# =====  mostPopularWords()  =====================
# ================================================
# 
# From a given term document matrix, calculate
# the most popular words for each document.
# Actually picks the 70th percentile, and then
# returns the top_N of that.  Typically used to
# the tope 10-20 words from a large document, so
# that algorithm should be fine.
# 
# INPUT   : term document matrix
#           vector of file names
#           number of top words 
# RETURN  : matrix of most popular words, arranged 
#           with each row as a different document
#
mostPopularWords <- function(tdm, fileList, numWords)
{
  if (numWords < 1)
  {
    print("ERROR : numWords too small")
    return
  }
  
  # create matrix for the most popular words
  popularWords <- matrix(nrow=numWords, ncol=length(fileList))
  colnames(popularWords)<-fileList
  colCount <- 1
  
  for (fileName in fileList)
  {
    termFrequency <- rowSums(as.matrix(tdm[,tdm$dimnames$Docs==fileName]))
    termFrequency.matrix <- as.matrix(rowSums(as.matrix(tdm[,tdm$dimnames$Docs==fileName])))
    wf = rowSums(as.matrix(termFrequency))
    wf = rowSums(termFrequency.matrix)
    termFrequency2 = termFrequency.matrix[wf>quantile(wf,probs=.7),]
    v1<- sort(termFrequency2,decreasing=TRUE)
    
    popularWords[, colCount] <- names(v1[1:numWords])
    colCount <- colCount + 1
  }
  
  return (popularWords)
  
}



# ================================================
# =====  barplotMostPopularWords()  ==============
# ================================================
# 
# From a given term document matrix, calculate
# the most popular words for each document, then
# create a barplot for each document.
# Actually picks the 70th percentile, and then
# returns the top_N of that.  Typically used to
# the tope 10-20 words from a large document, so
# that algorithm should be fine.
# 
# INPUT   : term document matrix
#           number of top words 
# RETURN  : -
#
barplotMostPopularWords <- function(tdm, numWords)
{
  if (numWords < 1)
  {
    print("ERROR : numWords too small")
    return
  }
  
  
  for (fileName in fileList)
  {
    termFrequency <- rowSums(as.matrix(tdm[,tdm$dimnames$Docs==fileName]))
    termFrequency.matrix <- as.matrix(rowSums(as.matrix(tdm[,tdm$dimnames$Docs==fileName])))
    wf = rowSums(as.matrix(termFrequency))
    wf = rowSums(termFrequency.matrix)
    termFrequency2 = termFrequency.matrix[wf>quantile(wf,probs=.7),]
    v1<- sort(termFrequency2[1:numWords],decreasing=TRUE)
    
    barplot(v1, las=2, main=fileName, horiz=TRUE)
  }
  
}



# ================================================
# =====  correspondenceAnalysis()  ===============
# ================================================
# 
# From a given term document matrix, calculate
# the correspondence analysis.  Then calculate
# it's description, and from that extract the
# names of the documents.
# 
# INPUT   : term document matrix
# RETURN  : row and column points factor map.
#           description analysis
#           names of the documents in the description analysis
#
correspondenceAnalysis <- function(tdm)
{
  # calculate the correspondence analysis
  corrAn=CA(as.matrix(tdm),graph=FALSE)
  
  # corr. anal. description analysis
  dd <- dimdesc(corrAn)
  
  # names
  docNames<-dimnames(dd$`Dim 1`$col)
  
  retValue <- list(corrAn, dd, docNames)
  
}



# ================================================
# =====  distanceDenogram()  =====================
# ================================================
# 
# From a given correspondence analysis calculate
#  distances and use those to do a hierarchial 
# cluster analysis.
# 
# INPUT   : correspondence analysis
#           dist() method, default = euclidian
#           hclust() method, default = ward
# RETURN  : object of class hclust
#
distanceDenogram <- function(corrAnalysis, distMethod="euclidian", hclustMethod="ward")
{
  d <- dist(corrAnalysis$col$coord, method=distMethod)
  denFit <- hclust(d, method=hclustMethod)
  
  return(denFit)
  
}



# ================================================
# =====  plotDistanceDenogram()  =================
# ================================================
# 
# Plot a calculated hierarchial cluster analysis.
# 
# INPUT   : object of class hclust
#           number of clusters
#           (used to cut tree into groups, if >1)
# RETURN  : -
#
plotDistanceDenogram <- function(denFit, numClusters=0)
{
  plot(denFit, xlab="", ylab="", 
          main="cluster denogram based on dimension-value distances",
          sub="")
  
  # cut the tree into clusters
  # then draw red borders around the clusters
  if (numClusters > 1)
  {
    groups <- cutree(denFit, k=numClusters)
    rect.hclust(denFit, k=numClusters, border="red")
  }
  
}     # =====  end of plotDistanceDenogram()  =====



# ================================================
# =====  plotDimDescData()  ======================
# ================================================
# 
# Plot the Dim Description data .
# Colour the data according to party 
# (Lib=red, Cons=blue)
# 
# INPUT   : corr. analysis object
#           dim. desc. object
#           file_names dataframe
#           draw year-year trajectory line (T/F)
# RETURN  : -
#
plotDimDescData <- function(rei_ca, dd, rnm, progressLines=FALSE)
{
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
       main="Correspondence Analysis Primary Dimensions (party colouring)")
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
  # first we need to sort the list of names by year
  # (not so easy, since format is XXX_YYYY.txt)
  if (progressLines == TRUE)
  {
    bNames <- matrix(ncol=2, nrow=length(rnm[[1]]))
    bNames[,1]<-rnm[[1]]
    bNames[,2]<-substr(rnm[[1]], 5, 8)
    sortedNames <- bNames[order(bNames[,2])]
    
#     lines(dd$`Dim 1`$col[sort(rnm[[1]]),], 
#           dd$`Dim 2`$col[sort(rnm[[1]]),], 
    lines(dd$`Dim 1`$col[sortedNames,], 
          dd$`Dim 2`$col[sortedNames,], 
          col='green',
          cex=3)
  }
  
}     # =====  end of plotDimDescData()  =====



# ================================================
# =====  plotAuthorDimDescData()  ================
# ================================================
# 
# Plot the Dim Description data .
# 
# INPUT   : corr. analysis object
#           dim. desc. object
#           file_names dataframe
#           draw year-year trajectory line (T/F)
# RETURN  : -
#
plotAuthorDimDescData <- function(rei_ca, dd, rnm, progressLines=FALSE)
{
  # plot as points
  # colour by party
  d1<-format(rei_ca$eig[[2]][1], digits=4)
  d2<-format(rei_ca$eig[[2]][2], digits=4)
#   partyColour <- substr(rnm[[1]],1,1)
#   partyColour<-sub("C","blue",partyColour)
#   partyColour<-sub("L","red",partyColour)
#   xLabel <- paste("Dim-1 : ", d1, "% of variance")
  yLabel <- paste("Dim-2 : ", d2, "% of variance")
  plot(dd$`Dim 1`$col[rnm[[1]],], dd$`Dim 2`$col[rnm[[1]],], 
       #     col=1:8,
#       col=partyColour,
       col="blue",
       cex=3,
       #     cex=5.6,
       pch=16, 
       type="p",
       xlab=xLabel, ylab=yLabel,
       main="Correspondence Analysis Primary Dimensions (party colouring)")
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
  # first we need to sort the list of names by year
  # (not so easy, since format is XXX_YYYY.txt)
  if (progressLines == TRUE)
  {
    bNames <- matrix(ncol=2, nrow=length(rnm[[1]]))
    bNames[,1]<-rnm[[1]]
    bNames[,2]<-substr(rnm[[1]], 5, 8)
    sortedNames <- bNames[order(bNames[,2])]
    
    #     lines(dd$`Dim 1`$col[sort(rnm[[1]]),], 
    #           dd$`Dim 2`$col[sort(rnm[[1]]),], 
    lines(dd$`Dim 1`$col[sortedNames,], 
          dd$`Dim 2`$col[sortedNames,], 
          col='green',
          cex=3)
  }
  
}     # =====  end of plotAuthorDimDescData()  =====





