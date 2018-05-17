# Unsupervised Learning 
data_corpus_inauguralPost70 <- corpus_subset(data_corpus_inaugural, Year > 2000)
textplot_xray(kwic(data_corpus_inauguralPost70, "america*"), 
              kwic(data_corpus_inauguralPost70, "people"))
# Set up workspace
rm(list = ls())
setwd("/Users/pauloserodio/Dropbox/Academic/Teaching/Summer Schools & Workshops/EUI Florence 2018/Course Main Folder/Lectures/Day 5 - Unsupervised Learning/Lab/")

# Loading packages
##install.packages("lsa")
#install.packages("factoextra")

library(quanteda)
library(quanteda.corpora)

## 1 PCA

# 1.1 Two functions in base R:

?prcomp # SVD on the (centered) input data
?princomp # eigendecomposition on the covariance matrix of the input data -- can also use option for covariance matrix 

# Remember to center your data! -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!

library(factoextra) # makes it easy to work with PCA

# 1.2 Example
data("data_corpus_sotu")

SOTU_dfm <- dfm(data_corpus_sotu[145:223,], 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)

SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

# Elbow plot
plot(SOTU_pca, type = "l")

# How much variance do the first few PCs account for?
summary(SOTU_pca)

# Eigenvalues
head(get_eigenvalue(SOTU_pca))

fviz_eig(SOTU_pca, addlabels = TRUE, ylim = c(0, 50))

# Loadings for each variable: columns contain the eigenvectors
SOTU_pca$rotation[1:10, 1:5]

# Value of the rotated data: your "new", dimensionality reduced data
View(SOTU_pca$x)

# Visualization resources:

# Tutorial from factoextra author about how to use his package to explore and visualize PCA results: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# See here for visualizing PCA with the ggbiplot library: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


##########  Clustering #############

######putting together some example R code

load('/Users/pauloserodio/Dropbox/Academic/Teaching/Summer Schools & Workshops/EUI Florence 2018/Course Main Folder/Lectures/Day 5 - Unsupervised Learning/Lab/FlakeMatrix.RData')

##that loads flake_matrix

extra_stop<- c('rep', 'jeff', 'flake', '2022252635', 'matthew', 'jagirdar', 'email', 'byline','specht', 'sarabjit', 'dateline') 

flake_matrix<- flake_matrix[-c(603, 604),-which(colnames(flake_matrix)%in% extra_stop)]

flake_norm<- flake_matrix
for(z in 1:nrow(flake_norm)){
	flake_norm[z,]<- flake_norm[z,]/sum(flake_norm[z,])
	}
	

n.clust<- 3
set.seed(8675309) ##complicated objective function
k_cluster<- kmeans(flake_norm, centers = n.clust)
table(k_cluster$cluster)

##labeling the topics
##just use the ``biggest" in each category
key_words<- matrix(NA, nrow=n.clust, ncol=10)
for(z in 1:n.clust){
	key_words[z,]<- colnames(flake_matrix)[order(k_cluster$center[z,], decreasing=T)[1:10]]
	}

##we can then try to compare the ``relative" strong words

key_words2<- matrix(NA, nrow=n.clust, ncol=10)
for(z in 1:n.clust){
	diff<- k_cluster$center[z,] - apply(k_cluster$center[-z, ], 2, mean)
	key_words2[z,]<- colnames(flake_matrix)[order(diff, decreasing=T)[1:10]]
	}



################################
####### Topic Modelling ########
################################

# Check for these packages, #install them if you don't have them
 #install.packages("tidytext")
 #install.packages("topicmodels")
 #install.packages("ldatuning")
 #install.packages("stringi")
 #install.packages("rjson")

libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi")
lapply(libraries, require, character.only = TRUE)

## 1 Preprocessing

# Load data
blm_tweets <- read.csv("blm_samp.csv", stringsAsFactors = F)

# Create date vectors
blm_tweets$datetime <- as.POSIXct(strptime(blm_tweets$created_at, "%a %b %d %T %z %Y",tz = "GMT")) # full date/timestamp
blm_tweets$date <- mdy(paste(month(blm_tweets$datetime), day(blm_tweets$datetime), year(blm_tweets$datetime), sep = "-")) # date only

# Collapse tweets so we are looking at the total tweets at the day level
blm_tweets_sum <- blm_tweets %>% group_by(date) %>% summarise(text = paste(text, collapse = " "))

# Remove non ASCII characters
blm_tweets_sum$text <- stringi::stri_trans_general(blm_tweets_sum$text, "latin-ascii")

# Removes solitary letters
blm_tweets_sum$text <- gsub(" [A-z] ", " ", blm_tweets_sum$text)

# Create DFM
blm_dfm <-dfm(blm_tweets_sum$text, stem = F, remove_punct = T, tolower = T, remove_twitter = T, remove_numbers = TRUE, remove = c(stopwords("english"), "http","https","rt", "t.co"))

## 2 Selecting K

# Identify an appropriate number of topics (FYI, this function takes a while)
k_optimize_blm <- FindTopicsNumber(
  blm_dfm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = 6L,
  verbose = TRUE
)

FindTopicsNumber_plot(k_optimize_blm)

# Where do these metrics come from? 

# Go here for the citations (and another tutorial)
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

# What should you consider when choosing the number of topics you use in a topic model?

# What does robustness mean here?

## 3 Visualizing Word weights

# Set number of topics
k <- 19

# Fit the topic model with the chosen k
blm_tm <- LDA(blm_dfm, k = k, method = "Gibbs",  control = list(seed = 1234))

# Other parameters that we do not use here (because they increase the time the model takes) can be passed to the control parameter
?`LDAcontrol-class`
# iter : num iterations
# thin : every thin iteration is returned for iter iterations
# burnin : number of initial iterations discarded

## Letter soup

# gamma = posterior topic distribution over documents
blm_tm@gamma
# Docs x topic_proportions array
rowSums(blm_tm@gamma) # Each row sums to 1

# beta = log word distributions over topics
blm_tm@beta
# Topics x log word proportion

# alpha = scaling parameter for symmetric Dirichlet distribution over topic distributions
# in this implementation, alpha is estimated as 50/k but can be set in parameters of LDA()
blm_tm@alpha 

# z = topic assignments of specific words
blm_tm@z

# Quickly extracts the word weights and transforms them into a data frame
blm_topics <- tidy(blm_tm, matrix = "beta") 

# Side note: You can pass objects between tidytext() and topicmodels() functions because tidytext() implements topic models from topicmodels()

# Generates a df of top terms
blm_top_terms <- blm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Creates a plot of the weights and terms by topic
blm_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


## 4 Visualizing topic trends over time

# Store the results of the distribution of topics over documents
doc_topics <- blm_tm@gamma

# Store the results of words over topics
words_topics <- blm_tm@beta

# Transpose the data so that the days are columns
doc_topics <- t(doc_topics)

# Arrange topics
# Find the top topic per column (day)
max<-apply(doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(blm_tweets_sum$date))

# Plot
blm_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

blm_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("BLM-Related Tweets from 2014 to 2016 over Topics") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4) + # Freddie Gray (Topic)
  geom_vline(xintercept=as.numeric(shootings[2]), color = "black", linetype=4)  + # Sandra Bland
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



####################################
## Correlated Topic Models (CTM)  ##
####################################

#install.packages("lattice")
#install.packages("bursts")
#install.packages("lda")
#install.packages("stm")

libraries <- c("Matrix", "ldatuning", "topicmodels", "readtext", "dplyr", "stm", "quanteda", "lda", "bursts", "tidytext", "ggplot2", "lattice", "quanteda.corpora")
lapply(libraries, require, character.only = T)


# What is a CTM?

# Loading the data and creating a DFM
data(data_corpus_movies)

movies_corp <- corpus_sample(data_corpus_movies, size = 20)

movies_dfm <- dfm(movies_corp, remove = stopwords("english"), remove_punct = T)

# FYI, this function takes a long time to run
as_ctm <- CTM(movies_dfm, k = 10, control = list(seed = 1234))

# Summarization
betas <- t(as_ctm@beta)

words <- as_ctm@terms

# Gets the top terms
get_terms(as_ctm,10)

# Since tidytext doesn't work with CTM, I manually extract the top 10 terms for each topic with this function
df <- lapply(1:ncol(betas), function(x){data.frame(Topic = x, Term = words, Beta = betas[,x]) %>% arrange(desc(Beta)) %>% slice(1:10)})

df_ctm <- bind_rows(df)


df_ctm %>%
  mutate(Term = reorder(Term, Beta)) %>%
  ggplot(aes(Term, Beta, fill = factor(Topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip()


# Sigma is the Variance Covariance Matrix of the all Topics
topic_var_matrix<-as_ctm@Sigma
topic_cor_matrix <- cov2cor(topic_var_matrix)

#Visualizing correlation matrix between topics
rgb.palette <- colorRampPalette(c("green", "blue"), space = "rgb")
levelplot(topic_cor_matrix, xlab="", ylab="", col.regions=rgb.palette(120))


## Structural Topic Models (STM)

# What is an STM?

# Loading data: Political blogs from the 2008 election on a conservative-liberal dimension

#install.packages("stm")
data("poliblog5k", package="quanteda.corpora")

head(poliblog5k.meta)

head(poliblog5k.voc)

# Fits an STM model with 3 topics
blog_stm <- stm(poliblog5k.docs, poliblog5k.voc, 3,
            prevalence=~rating + s(day), data=poliblog5k.meta)


# A plot that summarizes the topics by what words occur most commonly in them
plot(blog_stm,type="labels")

# A summary plot of the topics that ranks them by their average proportion in the corpus
plot(blog_stm, type="summary")

# A visualization of what words are shared and distinctive to two topics
plot(blog_stm, type="perspectives", topics=c(1,2))

# Estimates a regression with topics as the dependent variable and metadata as the independent variables
prep <- estimateEffect(1:3 ~ rating + s(day) , blog_stm, meta=poliblog5k.meta)

# Plots the distribution of topics over time
plot(prep, "day", blog_stm, topics = c(1,2), 
     method = "continuous",xaxt = "n", xlab = "Date")

# Plots the Difference in coverage of the topics according to liberal or conservative ideology
plot(prep, "rating", model=blog_stm,
     method="difference",cov.value1="Conservative",cov.value2="Liberal")


##### LDA using twitter data ######



library(topicmodels)
library(ggplot2)

###First, you need to go to my github and download the data

###Save the two folders to your desktop

###Get the list of files
g1 <- list.files("/Users/pauloserodio/Dropbox/Academic/Teaching/Summer Schools & Workshops/EUI Florence 2018/Course Main Folder/Lectures/Day 5 - Unsupervised Learning/Lab/govdates/", full.names=TRUE)
g2 <- list.files("/Users/pauloserodio/Dropbox/Academic/Teaching/Summer Schools & Workshops/EUI Florence 2018/Course Main Folder/Lectures/Day 5 - Unsupervised Learning/Lab/oppdates/",  full.names=TRUE)
files<-c(g1, g2)



###read in the tweets
tweets <- lapply(files, readLines)


##Combine all the tweets per day to form the documents
tweets<-lapply(tweets, function(x) paste(x, collapse=" "))
txt <- unlist(tweets)



##Tokenize and clean the text

txt<-tokenize(txt, removePunct = TRUE, removeTwitter = FALSE )

##Convert to lower case

txt<-toLower(txt)

##Convert to Document Feature Matrix (AKA Document Term Matrix)

mat <-dfm(txt, stem=TRUE, language = "spanish", ignoredFeatures = stopwords(kind="spanish"))



##Run LDA 

##Set number of topics
k <-50


SEED<-2010

##Run the topic model
TM<-list(Gibbs = LDA(mat, k = k, method = "Gibbs",  control = list(seed = SEED, burnin = 300,thin = 30, iter = 300)))

##Store the results of the distribution of topics over documents
doc_topics<-TM[["Gibbs"]]@gamma


##Store the results of words over topics

words_topics<-TM[["Gibbs"]]@beta

###Look at a visualization of the topics

###transpose the data so that the days are columns
doc_topics<-t(doc_topics)

View(doc_topics)

#arrange the topic assignments from greatest to least

#find the max from each column
max<-apply(doc_topics, 1, which.max)

##write a function that finds the second max
which.max2<-function(x){
  which(x == sort(x,partial=(k-1))[k-1])
  
}

##find the second max
max2<- apply(doc_topics, 1, which.max2)


##make an index of the days for the plot
dates<-seq(as.Date("2013/12/18"), by="days", length=162)

##divide the topic-days by coaltion
gov1<-max[1:162]
gov2<-max2[1:162]

opp1<-max[163:324]
opp2<-max2[163:324]

##make data frames
gov_topics<-data.frame(dates, gov1, gov2)
opp_topics<-data.frame(dates, opp1, opp2)

##Plot the topic topics over time

##government

z<-ggplot(gov_topics, aes(x=dates, y=gov1, pch="First")) 

z + geom_point(aes(x=dates, y=gov2, pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("Government")  + xlab(NULL) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point()+ 
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 



##opposition

z<-ggplot(opp_topics, aes(x=dates, y=opp1, pch="First")) 

z + geom_point(aes(x=dates, y=opp2, pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("Opposition")  + xlab(NULL) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point()+ 
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 


###Now let's look at the words in each of these topics

### Rather than just looking at the highest-probability words, we'll look at the most specific words

normalized.topics <- exp(TM[["Gibbs"]]@beta) / rowSums(exp(TM[["Gibbs"]]@beta))
calculate.specificity <- function(mod) {
  if(!inherits(mod,"LDA") & !inherits(mod,"CTM") ) stop("mod object must inherit from LDA or CTM")
  terms <- posterior(mod)$terms
  topics <- posterior(mod)$topics
  Nwords<-ncol(terms)
  Ntopics<-ncol(topics)
  Ndocs<-nrow(topics)
  ptopic <- apply(topics,2,sum)/Ndocs
  pwords <- apply(terms,2,function(x) sum(x*ptopic))
  numer <- terms*ptopic
  denom  <- matrix(pwords,nrow=Ntopics,ncol=Nwords,byrow=TRUE)
  return(numer/denom)
}
K<-k
normalized.words <- calculate.specificity(TM[["Gibbs"]])
normalized.words <- apply(exp(TM[["Gibbs"]]@beta), 2, function(x) x/sum(x))

scores <- apply(normalized.topics, 2, function(x) 
  x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) )
colnames(scores) <- TM[["Gibbs"]]@terms
words <- apply(scores, 1, function(x) 
  colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])
f.scores <- apply(scores, 1, function(x) 
  x[order(x, decreasing = TRUE)[1:num.words]])
n.topics <- rep(seq(1, K, 1), each=num.words)
order.topics <- rep(seq(1, num.words, 1), times=K)
info.df <- data.frame(
  topic = n.topics,
  word = c(words),
  order = as.character(order.topics),
  score = c(f.scores),
  stringsAsFactors=F)
info.df$order <- factor(info.df$order, levels=as.character(10:1))

info.df$specificity <- NA
for (i in 1:length(info.df$topic)){
  info.df$specificity[i] <- normalized.words[info.df$topic[i], which(colnames(scores) %in% info.df$word[i])]
}
info.df$topic <- paste0("Topic ", info.df$topic)
info.df$topic <- factor(info.df$topic, levels=paste0("Topic ", 1:K))

topten<-vector("list", K)

for (i in 1:K){
  j<-10*(i-1)+1
  m<-10*i
  topten[[i]]<-cbind(info.df$word[j:m])
}


####Now let's look at a few of the topics of interest

gov1
topten[20]
##Seems like a reasonable list of words the government is gonna tweet about a lot

gov2

topten[21]
##A very specific attack related to a specific event

opp1
topten[37]
##Another reasonable list of opposition party words
topten[2]
###But this also seems reasonable, and we see too many stop words

opp2
topten[25]
##seems to switch to a more economics-focused discussion

topten[34]
##maybe we need to remove the twitter handles?




########### STM ##########





###create covariates
team<-rep("gov", 162)
team[163:324]<-rep("opp", 162)
dates<-seq(as.Date("2013/12/18"), by="days", length=162)

days<-dates
days[163:324]<-dates

data<-data.frame(team, txt, as.numeric(days))



##use STM's cleaning functions


processed <- textProcessor(data$txt, metadata=data, language="spanish", stem=TRUE)



##remove some words for speed purposes
out_20 <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=20)


##Search K; we're not going to run this now

model<-searchK(out_20$documents, out_20$vocab, K=c(25, 50, 75))


###Run the model with the same number of topics as before

fitSpec20 <- stm(out_20$documents,out_20$vocab,K=50, init.type="LDA", 
                    content=~team, prevalence = ~team + as.numeric(days), max.em.its=30, data=out_20$meta, seed=5926696)



##find biggest topics

plot.STM(fitSpec25, type="summary")

big<-c(13, 42, 45, 38, 28)

int<-c(20, 11, 23)
###Words in those topics of interest

labelTopics(fitSpec20, big)

labelTopics(fitSpec20, int)

###Look at how content varies in these topics

##change data types
out_20$meta$team<-as.factor(out_20$meta$team)
out_20$meta$days<-as.numeric(out_20$meta$days)

##pick specifcation
prep<-estimateEffect(big ~ team , fitSpec20, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="team", topics=big, model=out_20, method="difference",  cov.value1 = "gov", cov.value2 = "opp",
                    xlab = "More Opp......More Gov", xlim=c(-.1, .1))


##pick specifcation--int
prep<-estimateEffect(int ~ team , fitSpec20, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="team", topics=int, model=out_20, method="difference",  cov.value1 = "gov", cov.value2 = "opp",
                    xlab = "More Opp......More Gov", xlim=c(-.1, .1))




##pick specifcation--over time
prep<-estimateEffect(big ~ s(days) , fitSpec20, meta=out_20$meta)


##plot effects
plot.estimateEffect(prep, covariate="days", topics=big, model=out_20, method="continuous")





###Let's see how the terms used vary within a topic

plot.STM(fitSpec20, type="perspectives", topics = 45)

plot.STM(fitSpec20, type="perspectives", topics = 42)

plot.STM(fitSpec20, type="perspectives", topics = 38)



########################
###### Scaling #########
########################

## 2 WORDFISH

# How is it different from other approaches we've used for scaling?

# 2.1 Read in conservative and labour manifestos (from Recitation 6)
setwd("../Lab/cons_labour_manifestos")

files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))

# Name data
files <- unlist(files)
files <- gsub("./", "", files )
files <- gsub(".txt", "", files )

# Create metadata
year <- unlist(strsplit(files, "[^0-9]+"))
year <- year[year!=""]

party <- unlist(strsplit(files, "[^A-z]+"))
party <- party[party!="a" & party!="b"]

#create data frame
man_df <- data.frame(year = factor(as.numeric(year)),
                   party = factor(party),
                   text = text,
                   stringsAsFactors = FALSE)

lab_con_dfm <- dfm(man_df$text, 
                   stem = T, 
                   remove = stopwords("english"), 
                   remove_punct = T
                   )

# 2.2 fit wordfish

# Setting the index on parties
manifestos_fish <- textmodel_wordfish(lab_con_dfm, c(1,24)) # second parameter corresponds to index texts

# Plot of document positions
plot(year[1:23], manifestos_fish$theta[1:23]) # These are the conservative manifestos
points(year[24:46], manifestos_fish$theta[24:46], pch = 8) # These are the Labour manifestos

plot(as.factor(party), manifestos_fish$theta)

# most important features--word fixed effects
words <- manifestos_fish$psi # values
names(words) <- manifestos_fish$features # the words

sort(words)[1:50]

sort(words, decreasing=T)[1:50]

# Guitar plot
weights <- manifestos_fish$beta

plot(weights, words)

# also check out wordshoal!
