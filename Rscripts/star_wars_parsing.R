setwd("/Users/gaston/Documents/Gaston/GoogleSite/StarWars")

# load packages
library(ggplot2)
library(plyr)
library(stringr)
library(tm)
library(igraph)

# reading cleaned scripts
Ep4 = read.table("SW_EpisodeIV.txt")
Ep5 = read.table("SW_EpisodeV.txt")
Ep6 = read.table("SW_EpisodeVI.txt")

# select dialogues
diags4 = Ep4[,2]
diags5 = Ep5[,2]
diags6 = Ep6[,2]

# characters
chr4 = Ep4[,1]
chr5 = Ep5[,1]
chr6 = Ep6[,1]

# how many dialogues in each episode
n4 = length(chr4)
n5 = length(chr5)
n6 = length(chr6)

# ====================================================
# Find most talkative characters
# and their dialogue frequencies per episode
# ====================================================

# join characters names
chrs = c(chr4, chr5, chr6)
eps = c(rep("IV",n4), rep("V",n5), rep("VI",n6))
# remove strings between parenthesis
#chrs = gsub("\\((.*?)\\)", "", chrs)
# strip whitespaces
chrs = gsub("\\s$", "", chrs)
# get absolute frequencies
chr_names = table(chrs)
# get number
y = as.vector(chr_names)
# sort by frequency
yord = order(y, decreasing=TRUE)
# inspect freqs
head(sort(y, decreasing=TRUE))
head(names(chr_names)[yord], 20)
tail(names(chr_names)[yord], 20)
# top 20 characters (most talkative) barplot
par(mar = c(7, 4, 4, 1))
top_chrs = head(chr_names[yord], 20)
barplot(top_chrs, las=2, cex.names=0.7, 
        border=NA, ylim=c(0,500), ylab="num of dialogues")
title(c("Star Wars (Episodes IV, V, VI)", "top 20 most talkative characters"),
      cex.main=0.9)
# get names
who_top_chrs = names(top_chrs)


# data frame
chr_eps = data.frame(characters=chrs, episodes=eps)
# number of dialogues per episode
chr_by_eps = ddply(subset(chr_eps, characters %in% who_top_chrs),
   .(characters, episodes), "nrow")
# not exactly in the shape I want it
chr_by_eps

# I want results in a matrix format
top_chrs_by_eps = matrix(0, nrow=length(top_chrs), ncol=3)
aux_top_chrs = sort(who_top_chrs)
for (i in 1:nrow(chr_by_eps))
{
  irow = which(chr_by_eps[i,1] == aux_top_chrs)
  icol = which(chr_by_eps[i,2] == c("IV","V","VI"))
  top_chrs_by_eps[irow, icol] = chr_by_eps[i,3]
}
rownames(top_chrs_by_eps) = aux_top_chrs
colnames(top_chrs_by_eps) = c("IV","V","VI")
# we'll use this to draw the bands
top_chrs_by_eps


# ====================================================
# Network between top20 characters
# ====================================================

# concatenate dialogues
diags = c(diags4, diags5, diags6)
# empty vector to collect dialogues of top chars
diag_top_chrs = rep("", 20)
# collect dialogues for top20 characters
for (i in 1:20)
{
  diag_top_chrs[i] = paste(diags[chrs == aux_top_chrs[i]], collapse=" ")
}
names(diag_top_chrs) = aux_top_chrs

# get corpus
diag_corpus = Corpus(VectorSource(diag_top_chrs))
# transforming
diag_corpus = tm_map(diag_corpus, tolower)
diag_corpus = tm_map(diag_corpus, removeWords, 
    c(stopwords("english"),"comlink"))
diag_corpus = tm_map(diag_corpus, removeNumbers)
diag_corpus = tm_map(diag_corpus, removePunctuation)
diag_corpus = tm_map(diag_corpus, stripWhitespace)

# dtm
diag_dtm = DocumentTermMatrix(diag_corpus)
dim(diag_dtm)

# as matrix
diag_mat = as.matrix(diag_dtm)

# get word count
count_terms = colSums(diag_mat)
# get less sparse terms
# terms >= 90% quantile frequency
which_mfw <- count_terms >= quantile(count_terms, probs=0.90)
sum(which_mfw)
# top 30 terms
mfw = count_terms[which_mfw]
barplot(head(sort(mfw, decreasing=TRUE), 30), 
        border=NA, las=2)


# adjacency matrix with most freq terms
mft_mat = diag_mat[,which_mfw]
adj_mat = diag_mat %*% t(diag_mat)
# zeors in diagonal
diag(adj_mat) = 0
# create graph from adjacency matrix
graph_chrs = graph.adjacency(adj_mat, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist
edges = get.edgelist(graph_chrs)


#adj_mat = abs(cor(t(mft_mat)))
bin_mat = mft_mat
bin_mat[mft_mat != 0] = 1
#adj_mat = dist(bin_mat, method="binary", diag=TRUE, upper=TRUE)
#adj_mat = 1 - as.matrix(adj_mat)
#adj_mat = abs(cor(t(bin_mat)))

adj_mat = bin_mat %*% t(bin_mat)

diag(adj_mat) = 0
# create graph from adjacency matrix
graph_chrs = graph.adjacency(adj_mat, mode="undirected", weighted=TRUE, diag=FALSE)
# get edgelist
edges = get.edgelist(graph_chrs)




# ====================================================
# top terms for each character
# ====================================================

# how many characters
nc = nrow(top_chrs_by_eps)
# list to store top terms
top_terms_by_chr = as.list(1:nc)
for (i in 1:nc)
{
  terms_by_chr = sort(diag_mat[i,], decreasing=TRUE)
  top_terms_by_chr[[i]] = head(terms_by_chr, 10)
}
names(top_terms_by_chr) = rownames(diag_mat)
# maximum num of times of frequent term pronounced by each char
maxi = sapply(top_terms_by_chr, function(x) max(x))
maxi


# ====================================================
# plot characters with bands
# ====================================================

# characters frequency
cf = rowSums(top_chrs_by_eps) / sum(top_chrs_by_eps)
# words center coordinates
fin = cumsum(cf)
ini = c(0, cumsum(cf)[-nc])
centers = (ini + fin) / 2
names(centers) = names(cf)
# word radiums
crads = cf / 2
# how many docs
ndocs = ncol(top_chrs_by_eps)

# colors of episodes (VI, V, IV)
#cols = c("#556270", "#4ECDC4", "#C7F464")
cols = c("#C7F464", "#4ECDC4", "#556270")

# function to get pie segments
t2xy <- function(x1, y1, u, rad)
{
  t2p <- pi * u + 0 * pi/180
  list(x2 = x1 + rad * cos(t2p), y2 = y1 + rad * sin(t2p))
}

# ennumerate
inds = sort(unique(as.vector(edges)))
nums = order(inds)
# matrix with numeric indices
e_num = matrix(0, nrow(edges), ncol(edges))
for (i in 1:nrow(edges))
{
  e_num[i,1] = centers[which(inds == edges[i,1])]
  e_num[i,2] = centers[which(inds == edges[i,2])]
}
# max arc
radios = abs(e_num[,1] - e_num[,2]) / 2
max_radios = which(radios == max(radios))
max_rad = unique(radios[max_radios] / 2)
# circle locations
locs = rowSums(e_num) / 2
# arc line width
#lwds = log10(E(graph_chrs)$weight) + 0.2 #/ max(E(graph_chrs)$weight)
#lwds = (log2(E(graph_chrs)$weight)+0.2) / 2
summary(E(graph_chrs)$weight)
hist(E(graph_chrs)$weight, breaks=seq(0, 3500, length=21))
boxplot(E(graph_chrs)$weight)
lwds = sqrt(E(graph_chrs)$weight/10)
summary(15*E(graph_chrs)$weight/max(E(graph_chrs)$weight))
#lwds = 20*E(graph_chrs)$weight/max(E(graph_chrs)$weight)
lwds = 10*E(graph_chrs)$weight/max(E(graph_chrs)$weight)

lwds = 10 * (E(graph_chrs)$weight - 0.9*min(E(graph_chrs)$weight)) / max(E(graph_chrs)$weight)
summary(lwds)

# gray colors for arcs
# arc_cols = hsv(0,0.2,0,0.10)
# arc_cols = hsv(h=0, s=lwds/max(lwds), v=0, alpha=0.5*lwds/max(lwds))
arc_cols = hsv(h=0, s=E(graph_chrs)$weight/max(E(graph_chrs)$weight),
               v=0, alpha=0.5*lwds/max(lwds))

#pdf("myplot.pdf", width=7, height=6)
# plot
par(mar = c(3,1,3,1))
plot.new()
plot.window(xlim=c(-0.025, 1.025), ylim=c(-0.5*max_rad, 1.1*max_rad*2))
# plot connecting arcs
z = seq(0, pi, l=100)
for (i in 1:nrow(edges))
{
  radio = radios[i]
  x = locs[i] + radio * cos(z)
  y = radio * sin(z)
  lines(x, y, col=arc_cols[i], lwd=lwds[i], 
        lend=1, ljoin=2, lmitre=1)
}
abline(h=0, col="white", lwd=5)
# add legend
legend(x=0.9, y=2*max_rad, title="Episodes", text.col="gray65",
       legend=c("IV","V","VI"), pch=19, col=cols, bty="n")

# plot word arcs
for (i in 1:nc)
{
  radius = crads[i]
  p = c(0, cumsum(top_chrs_by_eps[i,] / sum(top_chrs_by_eps[i,])))
  dp = diff(p)
  np = length(dp)
  angle <- rep(45, length.out = np)
  for (k in 1:np)
  {
    n <- max(2, floor(200 * dp[k]))
    P <- t2xy(centers[i], 0, seq.int(p[k], p[k+1], length.out=n), rad=radius)
    polygon(c(P$x2, centers[i]), c(P$y2, 0), angle=angle[i], 
            border=NA, col=cols[k], lty=0)
  }
  # draw white circles
  theta = seq(0, pi, length=100)
  x3 = centers[i] + 0.7*crads[i] * cos(theta)
  y3 = 0 + 0.7*crads[i] * sin(theta)
  polygon(x3, y3, col="white", border="white", lty=1, lwd=2)
  
}
# add word names
cex_names = cf
cex_names[cf < 0.01] = 0.01
cex_names = cex_names * 5
text(centers, 0, rownames(top_chrs_by_eps), cex=cex_names, 
     adj=c(0.5,0), col="gray50")
# close pdf
dev.off()



# ====================================================
# Find most frequent terms
# ====================================================

# collapse dialogues into single vectors
d4 = paste(diags4, collapse=" ")
d5 = paste(diags5, collapse=" ")
d6 = paste(diags6, collapse=" ")

# join collapsed dialogues vectors
all = c(d4, d5, d6)

# corpus
corpus = Corpus(VectorSource(all))

# transforming
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, 
    c(stopwords("english"),"comlink"))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)

# tdm
tdm = TermDocumentMatrix(corpus)
dim(tdm)
# convert as matrix
m = as.matrix(tdm)
# 
a = rowSums(m)
head(sort(a, decreasing=TRUE), 20)


# ====================================================
# Find most talking characters
# ====================================================

# concatenate dialogues
dialogues = c(diags4, diags5, diags6)
# get corpus
diag_corpus = Corpus(VectorSource(dialogues))
# transforming
diag_corpus = tm_map(diag_corpus, tolower)
diag_corpus = tm_map(diag_corpus, removeWords, 
    c(stopwords("english"),"comlink"))
diag_corpus = tm_map(diag_corpus, removeNumbers)
diag_corpus = tm_map(diag_corpus, removePunctuation)
diag_corpus = tm_map(diag_corpus, stripWhitespace)

# tdm
diag_tdm = TermDocumentMatrix(diag_corpus)
dim(diag_tdm)

diag_tdm = TermDocumentMatrix(diag_corpus,
    control=list(wordLengths=c(4, Inf), minDocFreq=5))

# as matrix
diag_mat = as.matrix(diag_tdm)
colnames(diag_mat) = chrs
# 
b = rowSums(diag_mat)
head(sort(b, decreasing=TRUE), 20)

# most frequent terms
most_freq = findFreqTerms(diag_mat, lowfreq=30)
mft = which(rownames(diag_mat) %in% most_freq)
mft_mat = diag_mat[mft,]
z = colSums(mft_mat)
head(sort(z, decreasing=TRUE), 20)
