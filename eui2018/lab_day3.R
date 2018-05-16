## 1 Setting up

# 1.1 Workspace

# Clear Global Environment
rm(list = ls())

# Set working directory
setwd(getwd())

# 1.2 Installing quanteda

# Install the latest stable version of quanteda from CRAN
install.packages("quanteda") # run this if you don't have quanteda already installed

library(quanteda)

# What version of quanteda do you have loaded? 
# How many threads (cores) are you using? See the printout in the console

# 1.3 Devtools and the quanteda corpus

# Install the package "devtools" which is used to install packages directly from Github
# install.packages("devtools")
library("devtools")

# Use devtools to install some built-in sample data
devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)

# Read about the data available: https://github.com/quanteda/quanteda.corpora

### Note: Quanteda is still under development so it is changing! New features are being added but sometimes 
### functions or function parameters are deprecated or renamed. 
### This includes the very basic functions in this code demo!
### This means that you may encounter many code examples, StackOverflow questions, 
### and websites with outdated documentation, etc. that include functions or options that have been deprecated or renamed.

# 1.4 Managing dependencies

# If you want to ensure that your code for a project will not break when you update quanteda, 
# I recommend using a dependency manager for R called packrat so that you can specify a dependency 
# on a specific version of quanteda.
# Find out about setting up packrat here: https://rstudio.github.io/packrat/walkthrough.html

# 1.5 Versions of quanteda

# How would you get an older version of quanteda? (For example, 
# if you accidentally installed the dev version from GitHub but 
# you want to go back to the last stable release, or you want a 
# legacy version to support old code.)
# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", repos = "http://cran.us.r-project.org")
# If you want the latest dev version of quanteda, it's on GitHub, but we will use the latest version 
# from CRAN for stability/sanity reasons
# devtools::install_github("quanteda/quanteda") 


## 2 Running basic text analysis

# start with a short character vector
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."

# 2.1 Let's tokenize (break vector into individual words)
tokenized_text <- tokens(sampletxt)
?tokens

tokenized_text <- tokens(sampletxt, remove_punct = TRUE)

# Concept review: Which of these are the same? 
# token (usually words, but can include numbers/punctuation)
# type (a unique word)
# feature 
# word
# term (part of the system's dictionary; related to token, but can include stemmed tokens)

# 2.2 Stemming examples
# SnowballC stemmer is based on the Porter stemmer 

stems <- tokens_wordstem(tokenized_text)
?tokens_wordstem

# 2.3 Loading State of the Union corpus

data("data_corpus_sotu", package = "quanteda.corpora")

# ndoc identifies the number of documents in a corpus

ndocs <- ndoc(data_corpus_sotu)
ntoken(data_corpus_sotu) # number of tokens (total words)
ntype(data_corpus_sotu) # number of types (unique words)
ntype(data_corpus_sotu)/ntoken(data_corpus_sotu) # High TTR indicates high degree lexical variation
nfeature(data_corpus_sotu)

# Here, we grab the text of Obama's 2016 speech (includes Trump's 2018 SOTU)

obama_2016_sotu <- data_corpus_sotu[ndocs -2]

# same as 

obama_2016_sotu <- texts(data_corpus_sotu)[ndocs - 2]

## 2.4 The DFM function creates a Document Feature Matrix from a document, corpus, etc
# in this case, from the 2016 speech

obama_dfm <- dfm(obama_2016_sotu, stem = TRUE)
?dfm

# What pre-processing options were used?

# if we wanted to stem the dfm of Obama's speech

dfm_wordstem(obama_dfm) 

# Question: why 0% sparsity?

# Inspecting the components of a DFM object

str(obama_dfm) # You can see this in the RStudio "Environment" pane as well

obama_dfm[1,1:20]

# The topfeatures function by default shows the most frequently occuring terms in a given DFM

topfeatures(obama_dfm)

# Are all of these features relevant?
# Words?
# Punctuation

## 2.5 Stopwords

# Stopwords are commonly used words that add little understanding to the content of the document by themselves

# The stopwords function takes a language as an input and produces a vector of stopwords compiled from that language

stopwords("english")

# Fun fact: Quanteda also supports stopwords for english, SMART document info retrieval system, danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, italian, portuguese, spanish, and arabic

# Here we compare a DFM from the last SOTU while without English stopwords with one that has them

obama_dfm_no_preprocessing <- dfm(obama_2016_sotu, remove_punct = TRUE)
obama_dfm_pre_processed <- dfm(obama_2016_sotu, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(obama_dfm_no_preprocessing)
topfeatures(obama_dfm_pre_processed)

## 3 Visualization and Weighted DFM

# Now we will create a DFM of all the SOTU speeches

full_dfm <- dfm(data_corpus_sotu, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(full_dfm)

# 3.1 Visualizing the text contained within the DFM(s)

# Dominated by stopwords
textplot_wordcloud(obama_dfm_no_preprocessing, max_words = 200)

# Stopwords removed
textplot_wordcloud(obama_dfm_pre_processed, max_words = 200)

# Over all the SOTUs

textplot_wordcloud(full_dfm, max_words = 200)

# 3.2 tfidf - Frequency weighting

weighted_dfm <- dfm_tfidf(full_dfm) # Uses the absolute frequency of terms in each document

topfeatures(weighted_dfm)
?tfidf

# tfidf - Relative frequency weighting

normalized <- dfm_tfidf(full_dfm, scheme_tf = "prop") 
# Uses feature proportions within documents: divdes each term by the total count of features in the document

topfeatures(normalized)

# What do the different rankings imply?


## 4 Collocations

# bigrams

textstat_collocations(obama_2016_sotu)
?textstat_collocations

# trigrams

textstat_collocations(obama_2016_sotu, size = 3)

# Are there any other terms you all think are interesting?


## 5 Regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex
?grep

s_index <- grep(" s ", texts(data_corpus_sotu))

# this returns every speech that contains " s " -- JUST THE LETTER S BY ITSELF
texts_with_s <- grep(" s ", texts(data_corpus_sotu), value = TRUE)

# Here we create a vector of documents with " s " removed
texts_without_s <- gsub(" s ", "",  data_corpus_sotu[s_index])

# What's the difference between grep and gsub?

## 6 Preprocessing choices

#install.packages("preText")
library("preText")

# Run at home (takes a few minutes to run)

preprocessed_documents <- factorial_preprocessing(
                          data_corpus_sotu,
                          use_ngrams = FALSE,
                          infrequent_term_threshold = 0.2,
                          verbose = FALSE)

preText_results <- preText(
                            preprocessed_documents,
                            dataset_name = "SOTU Speeches",
                            distance_method = "cosine",
                            num_comparisons = 20,
                            verbose = FALSE)

preText_score_plot(preText_results)

#####################
### Text Encoding ###
#####################

## 1 Non English texts

# 1.1 Non-English stopwords

stopwords(language = "spanish")

stopwords(language = "german")

stopwords(language = "zh", source = "misc")


# 1.2 Text encoding
# What is text encoding?
# How do you figure out what kind you have (e.g. scraped text from the Internet)?
# What kind of encoding can R and/or quanteda handle?
# 1.3 Some types of text encoding
# UTF-8
# ASCII (subset of UTF-8)
# Latin-1
# UTF-8 represents characters from European languages (English, Spanish, German, French, etc) 
# and some characters from Chinese/Japanese/Korean, plus emojis.
# Note: Text obtained from Internet sources can be messy. Issues can especially arise when you are 
# working with texts from multiple sources and you end up with a mixture of encodings. This can 
# cause the encoding to be detected incorrectly when you read in the text.

# 1.4 What encoding do you have?

# You can check with this function in base R
validUTF8("This is a sentence")

# You can use the package utf8(), written by Patrick Perry from NYU
# Read about it here: https://github.com/patperry/r-utf8
# install.packages("utf8")
library("utf8")

as_utf8("\xF0\x9F\x98\x81")
print("\xF0\x9F\x98\x81") # There are issues with base R's print() function for Unicode
# any guesses what this is?
utf8_print("\xF0\x9F\x98\x81")

# 1.5 What if you get a weird character and you're not sure?

# install.packages("stringi")
library("stringi")

# Use the encoding guesser to guess what this character is
stri_enc_detect("0x00E3")

# It's only a guess!

# What's ISO-8859-1?
# This is another name for the Latin-1 encoding. 

# 1.6 How do you convert encodings?

test_str <- "São Paulo"
validUTF8(test_str)

converted_str <- iconv("São Paulo", from = "UTF-8", to = "latin1")

converted_str
# Looks the same right?

charToRaw(converted_str) # Latin-1 encoding

charToRaw(test_str) # UTF-8 encoding

# But what about here?
iconv("ã", from = "UTF-8", to = "ASCII")

# In most cases, your text will probably already be in UTF-8. 
# In most cases, you want to convert your text to UTF-8 (with the possible 
# exception of languages that do not use the Latin alphabet)
# The authors of quanteda have also written a package called readtext() 
# that can also deal with encodings in text corpora!



########################################################################
###                 Text Descriptive Statistics                      ###
########################################################################

## Key Words In Context (KWIC) is a good way to summarize info about a topic

kwic(data_corpus_inaugural, "America", 3, case_insensitive = FALSE)
help(kwic)



# Suggested terms?


##########################
## Measuring similarity ##
##########################


# This helps illustrate the value of the vector representation

# Cosine similarity--take the dot product of two vectors
# x * y = |x||y|cos
# cos = x*y/|x||y|

x <- c(1, 2, 3)
y <- c(1, 2, 3)
 
# Define the norm function (to calculate the denominator |x| and |y|)

norm_vec <- function(x) sqrt(sum(x^2))

x %*% y / (norm_vec(x) * norm_vec(y))

# What if they're different

a <- c(1, 2, 3)
b <- c(1, 2, 4000)

a %*% b / (norm_vec(a) *norm_vec(b) )

# Let's do it with texts

last_speech_text <- data_corpus_inaugural[ndoc(data_corpus_inaugural)] # Trump's inaugural speech
first_speech_text <- data_corpus_inaugural[ndoc(data_corpus_inaugural) - 2] # Obama's 2009 inaugural speech

# Make a dfm of these two

first_last_dfm <- dfm(c(last_speech_text, first_speech_text), remove = stopwords("english"), stem = TRUE)

# Calculate similarity

similarity_first_last_with_preprocessing <- textstat_simil(first_last_dfm, margin = "documents", method = "correlation")

as.matrix(similarity_first_last_with_preprocessing)

# 6.2 Let's see how stopwords/stemming affect similarity

first_last_dfm_no_preprocessing <- dfm(c(last_speech_text, first_speech_text))

# Calculate similarity

similarity_first_last_no_preprocessing <- textstat_simil(first_last_dfm_no_preprocessing, margin = "documents", method = "correlation")

as.matrix(similarity_first_last_no_preprocessing)

# Make a dfm of a several documents

several_inaug_dfm <- dfm(corpus_subset(data_corpus_inaugural , Year > 1980), remove = stopwords("english"), stem = TRUE)

# Calculate similarity

similarity_several <- textstat_simil(several_inaug_dfm, margin = "documents", method = "correlation")

View(as.matrix(similarity_several))

# Other options available: Manhattan distance, cosine, etc.
?textstat_simil

# Specific comparisons with Obama's first inauguration speech

textstat_simil(several_inaug_dfm, "2009-Obama", margin = "documents", method = "correlation")


##############################
# Lexical diversity measures #
##############################


# Load in data: Irish budget proposals from 2008-2012
data("data_corpus_irishbudgets", package="quanteda.corpora")

library(dplyr)
irish_budget_texts <- texts(data_corpus_irishbudgets)

# Lexical diversity measures

## TTR 
budget_tokens <- tokens(irish_budget_texts, remove_punct = TRUE) 

# Num tokens per document
num_tokens <- lengths(budget_tokens)
num_types <- ntype(budget_tokens)
irish_budget_TTR <- num_types / num_tokens
head(irish_budget_TTR)
View(irish_budget_TTR)

# Would you expect the budgets to become more or less complex over time?

## Mean per-document TTR scores by year, party

TTR_by_year <- aggregate(irish_budget_TTR, by = list(data_corpus_irishbudgets[["year"]]$year), FUN = mean)
plot(TTR_by_year)
aggregate(irish_budget_TTR, by = list(data_corpus_irishbudgets[["party"]]$party), FUN = mean)
# FF = Fianna Fáil (Centre-Right)
# FG = Fine Gael (Centre-Right)
# Green = Green Party (Centre-Left)
# Indp = Independent Alliance (?)
# LAB = Labour (Centre-Left)
# PBPA = People Before Profit Alliance (Left)
# SF = 	Sinn Féin (Left)
# SOC = Social Democrats (Centre-Left)
# WUAG = Workers and Unemployed Action (Left)


## Calculate TTR score by year, party 

# by year
textstat_lexdiv(dfm(data_corpus_irishbudgets, groups = "year", remove_punct = TRUE, verbose = TRUE), measure = "TTR")

# Sidebar: using the "groups" parameter is how to group documents by a covariate -- note how this changes the ndocs of your corpus

textstat_lexdiv(dfm(data_corpus_irishbudgets, groups = "party", remove_punct = TRUE, verbose = TRUE), measure = "TTR")


#### Readability measure ####

## FRE
View(textstat_readability(data_corpus_irishbudgets, "Flesch"))
textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Flesch")
textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), "Flesch")


## Dale-Chall measure

textstat_readability(data_corpus_irishbudgets, "Dale.Chall.old")
textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Dale.Chall.old")
textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), measure = "Dale.Chall.old")

# 4.3 let's compare each measure

all_readability_measures <- textstat_readability(data_corpus_irishbudgets, c("Flesch", "Dale.Chall", "SMOG", "Coleman.Liau", "Fucks"))
# Note: Fucks refers to German physicist Wilhelm Fucks (see his 1955 work "Mathematical Analysis of Language Elements, Speech Style and languages.")
readability_matrix <- cbind(all_readability_measures$Flesch, all_readability_measures$Dale.Chall, all_readability_measures$SMOG, all_readability_measures$Coleman.Liau, all_readability_measures$Fucks)
readability_cor <- cor(readability_matrix)
rownames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
colnames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
readability_cor

# 5 Bootstrapping

# data prep: remove smaller parties so we're left with the 6 largest
iebudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets, !(party %in% c("WUAG", "SOC", "PBPA" )))

# convert corpus to df for stratified sampling
iebudgets_df <- data.frame(texts = iebudgetsCorpSub[["texts"]]$texts, 
                 party = iebudgetsCorpSub[["party"]]$party,
                 year = as.numeric(iebudgetsCorpSub[["year"]]$year),
                 stringsAsFactors = FALSE)

# Let's filter out empty speeches
iebudgets_df <- na.omit(iebudgets_df)

# We will use a loop to bootstrap a sample of texts and subsequently calculate standard errors

iters <- 10

# initialize data frames to store results
party_FRE <- data.frame(matrix(ncol = length(unique(iebudgets_df$party)), nrow = iters))
colnames(party_FRE) <- names(table(iebudgets_df$party))

# run the bootstrap

for(i in 1:iters) {
  
  iebudgets_grouped <- group_by(iebudgets_df, party)
  
  # take a sample of 20 documents per level (party)
  bootstrap_sample <- sample_n(iebudgets_grouped, 20, replace = TRUE)
  
  readability_results <- textstat_readability(bootstrap_sample$texts, measure = "Flesch")
  
  #store results
  
  readability_grouped <- group_by(readability_results, bootstrap_sample$party)
  readability_means <- summarize(readability_grouped, mean(Flesch))
  
  party_FRE[i, ] <- t(readability_means[, 2])
  
}

# inspect the results
View(party_FRE)

# Define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

# Calculate standard errors and point estimates

party_ses <- apply(party_FRE, 2, std)

party_means <- apply(party_FRE, 2, mean)

# Plot results--party

coefs<-party_means
ses<-party_ses

y.axis <- c(1:6)
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(party_FRE)
adjust <- 0
par(mar=c(2,8,2,2))

plot(coefs, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim=c(min,max),ylim = c(.5,6.5), main = "")
rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,5.5,max,6.5, col = c("grey97"), border="grey90", lty = 2)

axis(1, at = seq(min,max,(max-min)/10), 
     labels = c(round(min+0*((max-min)/10),3),
                round(min+1*((max-min)/10),3),
                round(min+2*((max-min)/10),3),
                round(min+3*((max-min)/10),3),
                round(min+4*((max-min)/10),3),
                round(min+5*((max-min)/10),3),
                round(min+6*((max-min)/10),3),
                round(min+7*((max-min)/10),3),
                round(min+8*((max-min)/10),3),
                round(min+9*((max-min)/10),3),
                round(max,3)),tick = T,cex.axis = .75, mgp = c(2,.7,0))
axis(2, at = y.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "white")
segments(coefs-qnorm(.975)*ses, y.axis+2*adjust, coefs+qnorm(.975)*ses, y.axis+2*adjust, lwd =  1)

segments(coefs-qnorm(.95)*ses, y.axis+2*adjust-.035, coefs-qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
segments(coefs+qnorm(.95)*ses, y.axis+2*adjust-.035, coefs+qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
points(coefs, y.axis+2*adjust,pch=21,cex=.8, bg="white")

# Compare with calculating the per-party mean Flesch statistic

summarize(
          group_by(
                  textstat_readability(iebudgets_df$texts, measure = "Flesch"),
                  iebudgets_df$party
          ),
          mean(Flesch)
)

# How well did we do?

