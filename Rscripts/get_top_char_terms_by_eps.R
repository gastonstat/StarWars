############################################################################
# Title:        get_top_char_terms_by_eps.R
# Description:  Script to get the top-10 terms of the top characters
#               for each episode
# Input files:  top_chars_by_eps.txt
#               SW_EpisodeIV.txt
#               SW_EpisodeV.txt
#               SW_EpisodeVI.txt
# Author:       Gaston Sanchez
#               www.gastonsanchez.com
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
############################################################################

# set your working directory (don't use mine!)
# setwd("/Users/gaston/Documents/Gaston/GoogleSite/StarWars")

# load package tm
require(tm)

# import data tables
Ep4 = read.table("SW_EpisodeIV.txt")
Ep5 = read.table("SW_EpisodeV.txt")
Ep6 = read.table("SW_EpisodeVI.txt")

# select dialogues
diags4 = Ep4[,2]
diags5 = Ep5[,2]
diags6 = Ep6[,2]

# select characters
chars4 = Ep4[,1]
chars5 = Ep5[,1]
chars6 = Ep6[,1]

# import 'top_chars_by_eps.txt'
top_chars_by_eps = read.table("top_chars_by_eps.txt")

# top character names (sorted)
aux_top_chars = rownames(top_chars_by_eps)

# =====================================================================
# Episode IV: get most frequent terms of top characters
# =====================================================================

# identify top characters in episode IV
aux_chars4 = chars4 %in% aux_top_chars
top_chars_eps4 = unique(chars4[aux_chars4])

# empty vector to collect dialogues of top chars
diag4_top_chars = rep("", length(top_chars_eps4))

# collect dialogues for top characters
for (i in 1:length(top_chars_eps4))
{
  diag4_top_chars[i] = paste(diags4[chars4 == top_chars_eps4[i]], collapse=" ")
}
names(diag4_top_chars) = top_chars_eps4

# get corpus for top characters in episode IV
diag4_corpus = Corpus(VectorSource(diag4_top_chars))

# apply text transformatioins
diag4_corpus = tm_map(diag4_corpus, tolower)
diag4_corpus = tm_map(diag4_corpus, removeWords, 
                     c(stopwords("english"),"comlink"))
diag4_corpus = tm_map(diag4_corpus, removeNumbers)
diag4_corpus = tm_map(diag4_corpus, removePunctuation)
diag4_corpus = tm_map(diag4_corpus, stripWhitespace)

# term-document matrix
diag4_tdm = TermDocumentMatrix(diag4_corpus)
dim(diag4_tdm)
diag4_tdm = as.matrix(diag4_tdm)

# get top terms for top characters in episode IV
diag4_top_terms = as.list(1:ncol(diag4_tdm))
for (j in 1:ncol(diag4_tdm))
{
  aux = sort(diag4_tdm[,j], decreasing=TRUE)
  diag4_top_terms[[j]] = head(aux, 10)
}
names(diag4_top_terms) = colnames(diag4_tdm)


# =====================================================================
# Episode V: get most frequent terms of top characters
# =====================================================================

# identify top characters in episode V
aux_chars5 = chars5 %in% aux_top_chars
top_chars_eps5 = unique(chars5[aux_chars5])

# empty vector to collect dialogues of top chars
diag5_top_chars = rep("", length(top_chars_eps5))

# collect dialogues for top characters
for (i in 1:length(top_chars_eps5))
{
  diag5_top_chars[i] = paste(diags5[chars5 == top_chars_eps5[i]], collapse=" ")
}
names(diag5_top_chars) = top_chars_eps5

# get corpus for top characters in episode V
diag5_corpus = Corpus(VectorSource(diag5_top_chars))

# apply text transformations
diag5_corpus = tm_map(diag5_corpus, tolower)
diag5_corpus = tm_map(diag5_corpus, removeWords, 
                      c(stopwords("english"),"comlink"))
diag5_corpus = tm_map(diag5_corpus, removeNumbers)
diag5_corpus = tm_map(diag5_corpus, removePunctuation)
diag5_corpus = tm_map(diag5_corpus, stripWhitespace)

# term-document matrix
diag5_tdm = TermDocumentMatrix(diag5_corpus)
dim(diag5_tdm)
diag5_tdm = as.matrix(diag5_tdm)

# get top terms for top characters in episode V
diag5_top_terms = as.list(1:ncol(diag5_tdm))
for (j in 1:ncol(diag5_tdm))
{
  aux = sort(diag5_tdm[,j], decreasing=TRUE)
  diag5_top_terms[[j]] = head(aux, 10)
}
names(diag5_top_terms) = colnames(diag5_tdm)


# =====================================================================
# Episode VI: get most frequent terms of top characters
# =====================================================================

# identify top characters in episode VI
aux_chars6 = chars6 %in% aux_top_chars
top_chars_eps6 = unique(chars6[aux_chars6])

# empty vector to collect dialogues of top chars
diag6_top_chars = rep("", length(top_chars_eps6))

# collect dialogues for top characters
for (i in 1:length(top_chars_eps6))
{
  diag6_top_chars[i] = paste(diags6[chars6 == top_chars_eps6[i]], collapse=" ")
}
names(diag6_top_chars) = top_chars_eps6

# get corpus for top characters in episode VI
diag6_corpus = Corpus(VectorSource(diag6_top_chars))

# apply text transformations
diag6_corpus = tm_map(diag6_corpus, tolower)
diag6_corpus = tm_map(diag6_corpus, removeWords, 
                      c(stopwords("english"),"comlink"))
diag6_corpus = tm_map(diag6_corpus, removeNumbers)
diag6_corpus = tm_map(diag6_corpus, removePunctuation)
diag6_corpus = tm_map(diag6_corpus, stripWhitespace)

# term-document matrix
diag6_tdm = TermDocumentMatrix(diag6_corpus)
dim(diag6_tdm)
diag6_tdm = as.matrix(diag6_tdm)

# get top terms for top characters in episode VI
diag6_top_terms = as.list(1:ncol(diag6_tdm))
for (j in 1:ncol(diag6_tdm))
{
  aux = sort(diag6_tdm[,j], decreasing=TRUE)
  diag6_top_terms[[j]] = head(aux, 10)
}
names(diag6_top_terms) = colnames(diag6_tdm)


# =====================================================================
# Create data tables from diagX_top_terms
# =====================================================================

# handy function to join terms and their countings
myfun <- function(x) {
  paste(names(x), x, sep="=")
}

# reshape lists into tables
X4 = sapply(diag4_top_terms, myfun)
X5 = sapply(diag5_top_terms, myfun)
X6 = sapply(diag6_top_terms, myfun)
X = rbind(t(X4), t(X5), t(X6))

# create a flag vector indicating the episodes
ntc4 = length(top_chars_eps4)
ntc5 =length(top_chars_eps5)
ntc6 =length(top_chars_eps6)
eps = c(rep(4,ntc4), rep(5,ntc5), rep(6,ntc6))

# add flag vector of episodes
X = cbind(X, eps)
# add column names
colnames(X) = c(paste("x", 1:10, sep=""), "eps")
X


# =====================================================================
# List with data tables of top-chars with top10 terms by episode
# =====================================================================

# how many top characters
nc = length(aux_top_chars)

# create list to store results
top_char_terms10 = as.list(1:nc)

# collect results
for (i in 1:nc)
{
  tmp = which(rownames(X) == aux_top_chars[i])
  # split terms and their count number
  term_vals = strsplit(t(X[tmp,-11]), "=")
  terms = sapply(term_vals, function(x) x[1])
  vals = as.numeric(sapply(term_vals, function(x) x[2]))
  epis = rep(X[tmp,11], each=10)
  # unique terms
  uniq_terms = unique(terms)
  # how many times each unique term
  uniq_vals = rep(0, length(uniq_terms))
  for (j in seq_along(uniq_terms))
  {
    uniq_vals[j] = sum(vals[terms == uniq_terms[j]])
  }
  names(uniq_vals) = uniq_terms
  # get top 10 terms
  top10_terms = head(sort(uniq_vals, decreasing=TRUE), 10)
  top10_words = names(top10_terms)
  # distribution of top10 terms by episode
  auxdf = data.frame(terms, vals, epis=as.numeric(epis))
  DF = data.frame(char=rep(aux_top_chars[i], 10), term=top10_words, 
             epIV=rep(0,10), epV=rep(0,10), epVI=rep(0,10),
             stringsAsFactors = FALSE)
  for (k in 1:10)
  {
    where_word <- auxdf[,1] == top10_words[k]
    DF[k,auxdf[where_word,3]-1] = auxdf[where_word,2]
  }
  top_char_terms10[[i]] = DF
}


# =====================================================================
# Export results in table format to file "top_char_terms.txt"
# =====================================================================

# save list of tables in file "top_char_terms.txt" as table format
cat(c("character","term","epIV","epV","epVI"), file="top_char_terms.txt", sep=",")
# write data to outfile file
for (i in 1:nc)
{
  for (j in 1:10)
  {
    cat("\n", file="top_char_terms.txt", append=TRUE)
    cat(unlist(top_char_terms10[[i]][j,]), file="top_char_terms.txt", 
        sep=",", append=TRUE)    
  }
}


