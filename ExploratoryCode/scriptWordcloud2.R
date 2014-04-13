# OOPS - forgot to note which web site I got this from
#
# Generate a wordcloud from a text document
# In this case I'm looking at Cdn. Gov't. budget speeches.
#

library(tm)
library(wordcloud)

# define folder and filename to use
txt2="c:/speeches"
fileName = "flaherty_budget2012.txt"

# grab the file into a corpus
b <- Corpus(DirSource(txt2, pattern=fileName))

# filter out stuff that just confuses the analysis
b <- tm_map(b, tolower)           		#Changes case to lower case
b <- tm_map(b, stripWhitespace) 			#Strips White Space
b <- tm_map(b, removeWords, stopwords("english"))
b <- tm_map(b, removePunctuation)

# create a term document matrix
tdm <- TermDocumentMatrix(b)

# create a matrix of word usage, then calc. the frequency
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word = names(v1),freq=v1)

# create the wordcloud (the "mullet of the internet")
wordcloud(d1$word,d1$freq, max.words=20, random.order=FALSE, rot.per=0)
text(1,1,fileName)

# make a barplot
barplot(d1$freq[1:20],names.arg=d1$word[1:20], las=2, main=fileName)


# create a cluster plot
# remove sparse terms
tdm.sparse <- removeSparseTerms(tdm, sparse=.95)

wf = rowSums(m1)
m1p = m1[wf>quantile(wf,probs=.97),]
#remove columns with all zero's
#m1p = m1p[,colSums(m1p) != 0]

#td.df <- as.data.frame(inspect(tdm.sparse))
td.df <- as.data.frame(m1p)
#td.df2 <- as.data.frame(td.df1[td.df1>10])

tdf.scale <- scale(td.df)
d <- dist(tdf.scale, method="euclidian")
#d <- dist(tdf.scale, method="manhattan")
fit <- hclust(d, method="ward")
plot(fit, cex=0.9)

groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")


# do a correspondence analysis
library(FactoMineR)
#rei_ca <- CA(m1, graph=FALSE)
#rei_ca <- CA(m1p, graph=FALSE)
rei_ca <- CA(td.df, graph=FALSE)
plot(rei_ca$row$coord, type="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(rei_ca$row$coord, rei_ca$row$coord, labels=rownames(td.df), col=hsv(0,0,0.6,0.5))
#text(rei_ca$row$coord[,1], rei_ca$row$coord[,2], labels=rownames(td.df), col=hsv(0,0,0.6,0.5))
