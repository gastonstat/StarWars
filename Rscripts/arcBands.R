############################################################################
# Title:        arcBands.R
# Description:  function to plot a basic arc-diagram with bands
# Author:       Gaston Sanchez
#               www.gastonsanchez.com
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
############################################################################


arcBands <- function(
  edgelist, bands, col.bands=NULL,
  sorted=TRUE, decreasing=FALSE, lwd=NULL,
  col=NULL, cex=NULL, col.nodes=NULL, lend=1, ljoin=2, lmitre=1,
  las=2, bg=NULL, mar=c(4,1,3,1))
{
  # ARGUMENTS
  # edgelist:   two-column matrix with edges
  # bands:      numeric matrix with rows=nodes and columns=numbers
  # sorted:     logical to indicate if nodes should be sorted
  # decreasing: logical to indicate type of sorting (used only when sorted=TRUE)
  # lwd:        widths for the arcs (default 1)
  # col:        color for the arcs (default "gray50")
  # cex:        magnification of the nodes labels (default 1)
  # col.nodes:  color of the nodes labels (default "gray50")
  # lend:       the line end style for the arcs (see par)
  # ljoin:      the line join style for the arcs (see par)
  # lmitre:     the line mitre limit fort the arcs (see par)
  # las:        numeric in {0,1,2,3}; the style of axis labels (see par)
  # bg:         background color (default "white")
  # mar:        numeric vector for margins (see par)
  
  # make sure edgelist is a two-col matrix
  if (!is.matrix(edgelist) || ncol(edgelist)!=2)
    stop("argument 'edgelist' must be a two column matrix")
  edges = edgelist
  # how many edges
  ne = nrow(edges)
  # get nodes
  nodes = unique(as.vector(edges))
  nums = seq_along(nodes)
  # how many nodes
  nn = length(nodes)  
  # ennumerate
  if (sorted) {
    nodes = sort(nodes, decreasing=decreasing)
    nums = order(nodes, decreasing=decreasing)
  }
  # make sure bands is correct
  if (!is.matrix(bands) && !is.data.frame(bands))
    stop("argument 'bands' must be a numeric matrix or data frame")
  if (is.data.frame(bands))
    bands = as.matrix(bands)
  if (nrow(bands) != nn)
    stop("number of rows in 'bands' is different from number of nodes")
  
  # check default argument values
  if (is.null(lwd)) lwd = rep(1, ne)
  if (length(lwd) != ne) lwd = rep(lwd, length=ne)
  if (is.null(col)) col = rep("gray50", ne)
  if (length(col) != ne) col = rep(col, length=ne)
  if (is.null(col.nodes)) col.nodes = rep("gray50", nn)
  if (length(col.nodes) != nn) col.nodes = rep(col.nodes, length=nn)
  if (!is.null(cex) && length(cex) != nn) cex = rep(cex, length=nn)
  if (is.null(bg)) bg = "white"
  
  # nodes frequency from bands
  nf = rowSums(bands) / sum(bands)
  # words center coordinates
  fin = cumsum(nf)
  ini = c(0, cumsum(nf)[-nn])
  centers = (ini + fin) / 2
  names(centers) = nodes
  # node radiums
  nrads = nf / 2
  
  # arcs coordinates
  # matrix with numeric indices
  e_num = matrix(0, nrow(edges), ncol(edges))
  for (i in 1:nrow(edges))
  {
    e_num[i,1] = centers[which(nodes == edges[i,1])]
    e_num[i,2] = centers[which(nodes == edges[i,2])]
  }
  # max arc radius
  radios = abs(e_num[,1] - e_num[,2]) / 2
  max_radios = which(radios == max(radios))
  max_rad = unique(radios[max_radios] / 2)
  # arc locations
  locs = rowSums(e_num) / 2

  # function to get pie segments
  t2xy <- function(x1, y1, u, rad)
  {
    t2p <- pi * u + 0 * pi/180
    list(x2 = x1 + rad * cos(t2p), y2 = y1 + rad * sin(t2p))
  }
  
  # plot
  par(mar = mar, bg = bg)
  plot.new()
  plot.window(xlim=c(-0.025, 1.025), ylim=c(0, 1*max_rad*2))
  # plot connecting arcs
  z = seq(0, pi, l=100)
  for (i in 1:ne)
  {
    radio = radios[i]
    x = locs[i] + radio * cos(z)
    y = radio * sin(z)
    lines(x, y, col=col[i], lwd=lwd[i], 
          lend=lend, ljoin=ljoin, lmitre=lmitre)
  }
  # plot node bands
  for (i in 1:nn)
  {
    radius = nrads[i]
    p = c(0, cumsum(bands[i,] / sum(bands[i,])))
    dp = diff(p)
    np = length(dp)
    angle <- rep(45, length.out = np)
    for (k in 1:np)
    {
      n <- max(2, floor(200 * dp[k]))
      P <- t2xy(centers[i], 0, seq.int(p[k], p[k+1], length.out=n), rad=radius)
      polygon(c(P$x2, centers[i]), c(P$y2, 0), angle=angle[i], 
              border=NA, col=col.bands[k], lty=0)
    }
    # draw white circles
    theta = seq(0, pi, length=100)
    x3 = centers[i] + 0.7*nrads[i] * cos(theta)
    y3 = 0 + 0.7*nrads[i] * sin(theta)
    polygon(x3, y3, col=bg, border=bg, lty=1, lwd=2)    
  }
  # add node names
  if (is.null(cex)) {
    cex = nf
    cex[nf < 0.01] = 0.01
    cex = cex * 10
  }
  # add node names
  text(centers, 0, nodes, cex=cex, adj=c(0.5,0), col=col.nodes)
}
