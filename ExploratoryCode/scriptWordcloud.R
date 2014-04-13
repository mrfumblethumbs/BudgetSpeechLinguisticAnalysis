# OOPS - forgot to note which web site I got this from
#
# We copy and paste President Barack Obama’s “Yes We Can” speech in a text document and read it in. 
# For a word cloud we need a dataframe with two columns, one with words and the the other with frequency.
# We read in the transcript from “http://politics.nuvvo.com/lesson/4678-transcript-of-obamas-speech-yes-we-can” 
# and paste in the file located in the local directory- C:/Users/KUs/Desktop/new. 
# Note tm is a powerful package and will read ALL the text documents within the particular folder

library(tm)
library(wordcloud)
txt2="c:/speech"
#b = Corpus(DirSource(txt2), 
#           readerControl = list(language="english"”))
b = Corpus(DirSource(txt2))
b<- tm_map(b, tolower)           		#Changes case to lower case
b<- tm_map(b, stripWhitespace) 			#Strips White Space
b <- tm_map(b, removePunctuation) 		#Removes Punctuation
tdm <- TermDocumentMatrix(b)
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word = names(v1),freq=v1)
wordcloud(d1$word,d1$freq)

# Now it seems we need to remove some of the very commonly occuring words like “the” and “and”. 
# We are not using the standard stopwords in english (the tm package provides that see Chapter 13 
# Text Mining case studies), as the words “we” and “can” are also included .
b <- tm_map(b, removeWords, c("and", "the"))
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word = names(v1),freq=v1)
wordcloud(d1$word,d1$freq)

#But let’s see how the wordcloud changes if we remove all English Stopwords.
b <- tm_map(b, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(b)
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word = names(v1),freq=v1)
wordcloud(d1$word,d1$freq)

