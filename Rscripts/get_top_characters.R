############################################################################
# Title:        get_top_characters.R
# Description:  Script to identify the most talkative characters
# Input files:  SW_EpisodeIV.txt
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

# load package plyr
require(plyr)

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

# how many dialogues in each episode
n4 = length(diags4)
n5 = length(diags5)
n6 = length(diags6)

# =======================================================
# Find most talkative characters
# =======================================================
# Keep in mind that Artoo and Chewie are not included 
# since they don't really have dialogues in english

# join characters names
chars = c(chars4, chars5, chars6)
# vector with episode number
eps = c(rep("IV", n4), rep("V", n5), rep("VI", n6))

# get absolute frequencies
char_names = table(chars)
# get quantity number of names
y = as.vector(char_names)
# sort by frequency
yord = order(y, decreasing=TRUE)

# inspect frequencies of names
head(sort(y, decreasing=TRUE))
head(names(char_names)[yord], 20)
tail(names(char_names)[yord], 20)

# barplot with the top 20 characters (most talkative)
par(mar = c(7, 4, 4, 1))
top_chars = head(char_names[yord], 20)
barplot(top_chars, las=2, cex.names=0.7, 
        border=NA, ylim=c(0,500), ylab="num of dialogues")
title(c("Star Wars (Episodes IV, V, VI)", "top 20 most talkative characters"),
      cex.main=0.9)
# get top names
who_top_chars = names(top_chars)


# =======================================================
# Find dialogue frequencies of top characters by episode
# =======================================================

# data frame with character names and episodes
char_eps = data.frame(characters=chars, episodes=eps)
# number of dialogues per episode
char_by_eps = ddply(subset(char_eps, characters %in% who_top_chars),
                   .(characters, episodes), "nrow")
# not exactly in the shape I want it
char_by_eps

# I want the results in a matrix format
top_chars_by_eps = matrix(0, nrow=length(top_chars), ncol=3)
aux_top_chars = sort(who_top_chars)
for (i in 1:nrow(char_by_eps))
{
  irow = which(char_by_eps[i,1] == aux_top_chars)
  icol = which(char_by_eps[i,2] == c("IV","V","VI"))
  top_chars_by_eps[irow, icol] = char_by_eps[i,3]
}
rownames(top_chars_by_eps) = aux_top_chars
colnames(top_chars_by_eps) = c("IV","V","VI")
# this is one of the table for the next steps in the analysis process
top_chars_by_eps

# export matrix 'top_chars_by_eps'
write.table(top_chars_by_eps, "top_chars_by_eps.txt")
