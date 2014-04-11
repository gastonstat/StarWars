############################################################################
# Title:        ultimate_arc_diagram.R
# Description:  Script to get the customized arc-diagram plot
# Input files:  top_chars_by_eps.txt
#               top_chars_network.txt
#               top_char_terms.txt
#               weight_edges_graph1.txt
#               weight_edges_graph2.txt
# R functions:  arcBandBars.R
# Author:       Gaston Sanchez
#               www.gastonsanchez.com
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
############################################################################

# ==================================================================
# Input files
# ==================================================================

# set your working directory (don't use mine!)
# setwd("/Users/gaston/Documents/Gaston/GoogleSite/StarWars")

# import 'top_chars_by_eps.txt'
top_chars_by_eps = read.table("top_chars_by_eps.txt")

# top character names (sorted)
aux_top_chars = rownames(top_chars_by_eps)

# how many top characters
nc = length(aux_top_chars)

# import top_chars_network.txt
top_chars_network = read.table("top_chars_network.txt")
edges1 = as.matrix(top_chars_network)

# import table top_char_terms.txt
top_char_terms = read.table("top_char_terms.txt", header=TRUE, sep=",")

# get a list of terms frequency by character 
# we'll use these in the barcharts below the arcs
top_chars_freq_terms = as.list(1:nc)
sec = seq(1, 200, by=10)
for (i in 1:20)
{
  Baux = as.matrix(top_char_terms[sec[i]:(sec[i]+9),3:5])
  rownames(Baux) = top_char_terms[sec[i]:(sec[i]+9),2]
  # greater than 4
  gt4 <- rowSums(Baux) >= 4
  # select those terms with counts greater than 4
  if (sum(gt4) > 1) {
    Baux = Baux[gt4,]
  } else {
    Baux = Baux[1:2,]
  }
  Bp = prop.table(Baux, margin=1)
  top_chars_freq_terms[[i]] = Bp
}

# import table top_char_terms.txt
w1 = read.table("weight_edges_graph1.txt", header=TRUE)
w1 = as.vector(t(w1))
w2 = read.table("weight_edges_graph2.txt")
w2 = as.vector(t(w2))


# ==================================================================
# Prepare parameters for plotting the arc-diagram
# ==================================================================

# arcs width parameter based on edge weights from graph_chars1
lwds = sqrt(w1/20)
summary(10 * w1 / max(w1))
lwds = 20 * w1 / max(w1)

# arc colors based on graph_chars2
cols = hsv(h=0, s=w2/max(w2), v=0, alpha=0.6*w2/max(w2))

# color bands (of episodes VI, V, IV)
# col.bands = c("#4EA3CD", "#4E70CD", "#5E4ECD")
col.bands = c("#C7F464", "#4ECDC4", "#6d849c")

# magnification factor of terms in bar-charts
# (I went through a large trial-and-error process in order
# to get the right size for the labels)
cex.terms = rowSums(top_chars_by_eps) / sum(top_chars_by_eps)
cex.terms[cex.terms < 0.1] = max(cex.terms) / 4
cex.terms = 3 * cex.terms


# ==================================================================
# Arc-diagram with bands
# ==================================================================
# This diagram is obtained with the function arcBandBars
# The function is pretty much tuned for the StarWars data tables
# although it can be modified to fit the user's needs

# save the image in pdf format so you can zoom-in
# and see all the interesting details 
pdf("StarWars_Arc_Diagram.pdf", width=14, height=9)
arcBandBars(edges1, top_chars_by_eps, top_chars_freq_terms,
            col.bands=col.bands, lwd=lwds, col=cols, cex.terms=cex.terms,
            col.terms="white", mar=c(1,1,3,1), bg="gray80")
title(c("Star Wars: Character Dialogues", "Arc-diagram"), 
      cex.main=0.9, col.main="gray50")
# add legend
legend(x=0.9, y=0.5, title="Episodes", text.col="gray25", cex=0.8,
       legend=c("IV","V","VI"), pch=19, col=col.bands, bty="n")
dev.off()
