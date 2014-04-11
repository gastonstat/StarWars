############################################################################
# Title:        get_top_chars_network.R
# Description:  Script to get a network of the top talkative characters
# Input files:  top_chars_by_eps.txt
#               SW_EpisodeIV.txt
#               SW_EpisodeV.txt
#               SW_EpisodeVI.txt
# R functions:  arcDiagram.R
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
require(igraph)

# import data tables
Ep4 = read.table("SW_EpisodeIV.txt", stringsAsFactors = FALSE)
Ep5 = read.table("SW_EpisodeV.txt", stringsAsFactors = FALSE)
Ep6 = read.table("SW_EpisodeVI.txt", stringsAsFactors = FALSE)

# select dialogues
diags4 = Ep4[,2]
diags5 = Ep5[,2]
diags6 = Ep6[,2]

# select characters
chars4 = Ep4[,1]
chars5 = Ep5[,1]
chars6 = Ep6[,1]

# how many dialogues in each episode
n4 = length(diags4)
n5 = length(diags5)
n6 = length(diags6)

# import 'top_chars_by_eps.txt'
top_chars_by_eps = read.table("top_chars_by_eps.txt")


# ==================================================================
# Get dialogues of top characters
# ==================================================================

# join characters names
chars = c(chars4, chars5, chars6)

# join dialogues in one vector
diags = c(diags4, diags5, diags6)

# how many top characters
ntop = nrow(top_chars_by_eps)
# top character names (sorted)
aux_top_chars = rownames(top_chars_by_eps)

# create empty vector to collect dialogues of top characters
top_char_diags = rep("", ntop)
# collect dialogues for top characters
for (i in 1:ntop) {
  top_char_diags[i] = paste(diags[chars == aux_top_chars[i]], collapse=" ")
}
names(top_char_diags) = aux_top_chars


# ==================================================================
# Text mining
# ==================================================================

# get corpus
diag_corpus = Corpus(VectorSource(top_char_diags))

# apply some text transformations
diag_corpus = tm_map(diag_corpus, tolower)
diag_corpus = tm_map(diag_corpus, removeWords, 
                     c(stopwords("english"),"comlink"))
diag_corpus = tm_map(diag_corpus, removeNumbers)
diag_corpus = tm_map(diag_corpus, removePunctuation)
diag_corpus = tm_map(diag_corpus, stripWhitespace)

# get document-term matrix
diag_dtm = DocumentTermMatrix(diag_corpus)
# inspect diag_dtm
# (90% sparsity, which means a lot of empty cells)
diag_dtm
dim(diag_dtm)

# convert as matrix
diag_mat = as.matrix(diag_dtm)

# get word count
count_terms = colSums(diag_mat)
hist(count_terms, col="gray80")

# to simplify, we need to get the less sparsed terms
# for instance, let's get terms >= 90% quantile frequency
which_mfw <- count_terms >= quantile(count_terms, probs=0.90)
sum(which_mfw)
# top 30 terms
mfw = count_terms[which_mfw]
barplot(head(sort(mfw, decreasing=TRUE), 30), 
        border=NA, las=2)
title(c("Most frequent terms in dialogues",
        "(from top characters)"), cex.main=0.9)


# ==================================================================
# Network between top characters
# ==================================================================
# In order to get a network between top characters, we first need to
# obtain an adjacency matrix based on the most frequent terms
# (we'll use this matrix to make the graph for the arc-diagram)
# Although we can get an adjacency matrix in several ways, I'm using
# two slightly different approaches (they provide the same graph)
# The difference in the two approaches are in the nodes weights
# which we'll use for plotting the arcs in the arc-diagram

# Option 1
# matrix with most frequent terms
mft_mat = diag_mat[,which_mfw]
# first adjacency matrix
adj_mat1 = diag_mat %*% t(diag_mat)
# set zeros in diagonal
diag(adj_mat1) = 0
# create graph from adjacency matrix
graph_chars1 = graph.adjacency(adj_mat1, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist 1
edges1 = get.edgelist(graph_chars1)

# Option 2
# transform mft_mat to a binary (ie boolean) matrix
bin_mat = mft_mat
bin_mat[mft_mat != 0] = 1
# second adjacency matrix using binary distances
adj_mat2 = dist(bin_mat, method="binary", diag=TRUE, upper=TRUE)
adj_mat2 = 1 - as.matrix(adj_mat2)
# set zeros in the diagonal
diag(adj_mat2) = 0
# create graph from adjacency matrix
graph_chars2 = graph.adjacency(adj_mat2, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist 2
edges2 = get.edgelist(graph_chars2)

# the difference between the two graphs is in the weights
a = E(graph_chars1)$weight
b = E(graph_chars2)$weight
summary(a); summary(b)

# save network in file "top_chars_network.txt"
write.table(edges1, "top_chars_network.txt")
write.table(E(graph_chars1)$weight, "weight_edges_graph1.txt")
write.table(E(graph_chars2)$weight, "weight_edges_graph2.txt")

# ==================================================================
# Preliminary arc-diagram plots
# ==================================================================

# let's take a first look to the arc-diagram network
# using the arcDiagram function

# arc-diagram using default parameters (not very interesting)
arcDiagram(edges1, cex=0.8, mar=c(7,1,4,1))
title("Preliminary arc-diagram", cex.main=0.9)

# let's try to improve the arc-diagram tweaking some parameters
# arc widths based on graph_chars1
w1 = E(graph_chars1)$weight
lwds = sqrt(w1/20)
# arc colors based on graph-chars2
w2 = E(graph_chars2)$weight
cols = hsv(h=0, s=w2/max(w2), v=0, alpha=0.5*w2/max(w2))

# The width of the arcs connecting two characters reflect the number 
# of words in common. In turn, the color of the arcs reflect the 
# association between two characters depending on the similarity 
# of the words in their dialogues.
# arc-diagram
arcDiagram(edges1, lwd=lwds, col=cols, cex=0.8, mar=c(7,1,4,1))
title("Preliminary arc-diagram", cex.main=0.9)


# ==================================================================
# Arc-diagram with pie charts
# ==================================================================

# now let's add some pie-charts in each node to reflect the
# dialogues participation proportion of each character by episode

# arc-diagram with pie charts on nodes
# color bands (of episodes VI, V, IV)
#col.pies = c("#C7F464", "#4ECDC4", "#556270")
col.pies = c("#C7F464", "#4ECDC4", "#6d849c")

# arc-diagram with pie charts
arcPies(edges1, top_chars_by_eps, cex=1, col.pies=col.pies, 
         lwd=lwds, col=cols, mar=c(7,1,4,1))
title("Arc-diagram with pie charts", cex.main=0.9)
legend(x=0.9, y=0.5, title="Episodes", text.col="gray65", cex=0.8,
       legend=c("IV","V","VI"), pch=19, col=col.pies, bty="n")


# ==================================================================
# Arc-diagram with bands
# ==================================================================

# We can add more visual effects by making the size of the nodes
# proportional to the importance of each character. In addition, 
# we can substitute the pie-charts for band-charts

# arc-diagram with bands on nodes
# color bands (of episodes VI, V, IV)
#col.bands = c("#C7F464", "#4ECDC4", "#556270")
col.bands = c("#C7F464", "#4ECDC4", "#6d849c")
# arc-diagram with bands
arcBands(edges1, top_chars_by_eps, col.bands=col.bands, 
         lwd=lwds, col=cols, mar=c(4,1,4,1))
title("Arc-diagram with bands", cex.main=0.9)
legend(x=0.9, y=0.5, title="Episodes", text.col="gray65", cex=0.8,
       legend=c("IV","V","VI"), pch=19, col=col.bands, bty="n")
