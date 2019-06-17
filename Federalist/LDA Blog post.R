# Text Analysis Project - Topic Modeling : The Federalist Papers
library(tm)           
library(dplyr)        
library(LDAvis)       
#library(SnowballC)    
library(topicmodels)  
library(stringr)      

# Load in the Federalist data
con <- url("https://www.gutenberg.org/files/18/18.txt") 
open(con, "r")
text.v <- readLines(con, n = -1, skipNul = TRUE)
close(con)

# Determine metadata boundaries
start.v <- which(text.v == "FEDERALIST. No. 1")
end.v <- which(text.v == "End of the Project Gutenberg EBook of The Federalist Papers, by ")

# Replace the text vector with the portion that is just the literary body.
text.v <- text.v[c((start.v):(end.v-1))]

# Replace occurrences of double-hyphens with spaces. Otherwise some words parse poorly.
length(grep("--",text.v))
text.v <- str_replace_all(text.v, "--", " ")

# Determine starting positions of chapters
chap.positions.v <- grep("^FEDERALIST*", text.v)

# Two different versions of Federalist #70 are included in the Project Gutenberg
# file. Both are undisputed works of Alexander Hamiltion. We will remove the
# first one here.
start70a <- chap.positions.v[70]-1
end70a <- chap.positions.v[71]-1
text.v <- text.v[-c(start70a:end70a)]
chap.positions.v <- grep("^FEDERALIST*", text.v)

# Remove more intra-document metadata and re-gen chapter start locations
indJournal <- grep("*Independent Journal*", text.v)
dailyAdv <- grep("*Daily Advertiser*", text.v)
nyPacket <- grep("*New York Packet*", text.v)
mcLean <- grep("*M[cC]LEAN*", text.v)
text1787 <- grep("1787[.]", text.v)
toThePeople <- grep("To the People of the State of New York", text.v)
generalIntro <- grep("General Introduction", text.v)

removeMeta <- c(indJournal, nyPacket, dailyAdv, mcLean, text1787, toThePeople, generalIntro)
text.v <- text.v[-c(removeMeta)]
chap.positions.v <- grep("^FEDERALIST*", text.v)

#Manipulate papers into one per row, first into a list then finally into a
#dataframe.
workList = list(NULL)
for(i in 1:length(chap.positions.v)) {
  if(i < length(chap.positions.v)) {
    workList[[i]] <- text.v[(chap.positions.v[i]+1):(chap.positions.v[i + 1]-1)]
  }
  else {
    workList[[i]] <- text.v[(chap.positions.v[i]+1):length(text.v)]
  }
}

workList2 = data.frame(Text = rep(NA, length(chap.positions.v)))
for(i in 1:length(chap.positions.v)) {
  workList2[i,1] = as.vector(paste(unlist(workList[[i]]), collapse = " "))
}
workList2$Paper = sprintf("Fed %d", 1:85)
workList2$Author = c("Hamiltion", rep("Jay",4), rep("Hamilton",4), "Madison", rep("Hamilton",3),
                     "Madison", rep("Hamilton",3), rep("Madison",3), rep("Hamilton",16),
                     rep("Madison",12), rep("Disputed",10), rep("Hamilton",3), rep("Disputed",2),
                     "Jay", rep("Hamilton",21))
workList2$Label = sprintf("%s:%s",workList2$Paper, workList2$Author)

# Create a text corpus from the text for use in the statistical analysis
documents <- Corpus(VectorSource(workList2$Text))
documents <- tm_map(documents, removePunctuation)
documents <- tm_map(documents,content_transformer(tolower))
documents <- tm_map(documents, removeNumbers)

# The authors' names are in each paper. Those are removed, along with their
# common nom-de-plume, that of Roman general Publius.
documents <- tm_map(documents, removeWords, c(stopwords("english"), "jay", "hamilton", "madison", "publius"))
documents <- tm_map(documents, stripWhitespace)

# Stem the document for topic modeling
documents <- tm_map(documents, stemDocument) # stemDocument as part of the tm package
#documents <- tm_map(documents, wordStem)    # wordStem as part of the SnowballC package

#######################
# End of generic text cleaning and corpus creation section.
#######################

#######################
## Topic Modeling Code
#######################
# An excerpt of cleaned and stemmed text from Federalist No. 13
str_sub(documents[[13]]$content, start=1, end = 60)

# dtMatrix excerpt and word frequencies
dtMatrix <- DocumentTermMatrix(documents)
inspect(dtMatrix)

#########################
# LDA Model Building    #
#########################

#Set parameters for Gibbs sampling. changed nstart to 10 to increase number of
#runs.
burnin <- 1000
iter <- 1000
thin <- 100
nstart <- 10
seed <- rep(list(1),nstart)
best <- TRUE

#Number of topics - one pass with 3, and one pass with 4 to see if an
#additional topic is relevant. LDA() takes several minutes.
#k3 <- 3
ldaOut3 <- LDA(dtMatrix,3, method="Gibbs", 
               control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter))

#Run LDA using Gibbs sampling, k = 4. LDA() takes several minutes.
ldaOut4 <- LDA(dtMatrix, 4, method="Gibbs", 
               control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter))

##########################
# Output when topics = 3 #
##########################

#docs to topics
ldaOut.topics3 <- as.matrix(topics(ldaOut3))

#top 10 terms in each topic
ldaOut.terms3 <- as.matrix(terms(ldaOut3,10))
colnames(ldaOut.terms3) <- c("Topic 3.1", "Topic 3.2", "Topic 3.3")

# Overall topic counts for all 85 papers
rbind(ldaOut.terms3, c(rep("Paper Count",3)), table(ldaOut.topics3))

#probabilities associated with each topic assignment
topicProbabilities3 <- as.data.frame(ldaOut3@gamma)
colnames(topicProbabilities3) <- c("pTopic1", "pTopic2", "pTopic3")

# Organize papers by topic
ldaOut.topics3.ID <- c("A Nation of Laws", 
                       "Foreign Relations and External Conflict",
                       "Structure Amidst Differing Agendas")
sprintf("Topic 3.%d: %s",1:length(ldaOut.topics3.ID), ldaOut.topics3.ID)

(ldaOut.topics3.Set <- cbind(Paper = workList2$Label, round(topicProbabilities3,3), N = ldaOut.topics3, 
                             TopicDesc = unname(apply(ldaOut.topics3, 1, FUN = function(x) (ldaOut.topics3.ID[x])))) )

# Five highest probabilities in topic 3.1
head(ldaOut.topics3.Set %>% arrange(desc(`pTopic1`)),5)
# Five highest probabilities in topic 3.2
head(ldaOut.topics3.Set %>% arrange(desc(`pTopic2`)),5)
# Five highest probabilities in topic 3.3
head(ldaOut.topics3.Set %>% arrange(desc(`pTopic3`)),5)

##########################
# Output when topics = 3 #
##########################

#docs to topics
ldaOut.topics4 <- as.matrix(topics(ldaOut4))

#top 10 terms in each topic
(ldaOut.terms4 <- as.matrix(terms(ldaOut4,10)))
colnames(ldaOut.terms4) <- c("Topic 4.1", "Topic 4.2", "Topic 4.3", "Topic 4.4")

# Overall topic counts for all 85 papers
rbind(ldaOut.terms4, c(rep("Paper Count",4)), table(ldaOut.topics4))

#probabilities associated with each topic assignment
topicProbabilities4 <- as.data.frame(ldaOut4@gamma)
colnames(topicProbabilities4) <- c("pTopic1", "pTopic2", "pTopic3", "pTopic4")

# Organize papers by topic
ldaOut.topics4.ID <- c("Powers and Limitations of the Executive Branch", 
                       "Structure Amidst Differing Agendas",
                       "A Nation of Laws", 
                       "Internal/External Relations and Conflict")

sprintf("Topic 4.%d: %s",1:length(ldaOut.topics4.ID), ldaOut.topics4.ID)

(ldaOut.topics4.Set <- cbind(Paper = workList2$Label, round(topicProbabilities4,3), N = ldaOut.topics4, 
                           TopicDesc = unname(apply(ldaOut.topics4, 1, FUN = function(x) (ldaOut.topics4.ID[x])))) )

# Five highest probabilities in topic 4.1
head(ldaOut.topics4.Set %>% arrange(desc(`pTopic1`)),5)
# Five highest probabilities in topic 4.2
head(ldaOut.topics4.Set %>% arrange(desc(`pTopic2`)),5)
# Five highest probabilities in topic 4.3
head(ldaOut.topics4.Set %>% arrange(desc(`pTopic3`)),5)
# Five highest probabilities in topic 4.4
head(ldaOut.topics4.Set %>% arrange(desc(`pTopic4`)),5)

##############################
# Visualization section      #
##############################

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Find required quantities
  phi <- as.matrix(posterior(fitted)$terms)
  theta <- as.matrix(posterior(fitted)$topics)
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, str_count(temp, pattern = '\\S+'))
  }

  # temp_frequency <- inspect(doc_term)
  temp_frequency <- as.matrix(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta=theta, vocab = vocab,
                                 doc.length = doc_length,term.frequency = freq_matrix$Freq)
  return(json_lda)
}

json1 <- topicmodels_json_ldavis(ldaOut4,documents,dtMatrix)
# You might want the files to go to a specific output directory. If so, change
# 'vis2' to a full path destination.
serVis(json1, out.dir = 'vis2', open.browser = TRUE)
