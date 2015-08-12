# Tutorial From
# https://trinkerrstuff.wordpress.com/2012/06/29/igraph-and-structured-text-exploration/
require(igraph)

set.seed(10)
X <- matrix(rpois(100, 1), 10, 10)
colnames(X) <- paste0("Guy_", 1:10)
rownames(X) <- c('The', 'quick', 'brown', 'fox', 'jumps',
                 'over', 'a', 'bot', 'named', 'Dason')
X
#word frequency matrix
Y <- X >= 1
Y
Y <- apply(Y, 2, as, "numeric") #boolean matrix
Y
rownames(Y) <- rownames(X)
Y
Z <- t(Y) %*% Y  #adjacency matrix
Z
g <- graph.adjacency(Z, weighted=TRUE, mode ='undirected')
# remove loops
require(igraph)
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)


#Plot a Graph
set.seed(3952)
layout1 <- layout.auto(g)
#for more on layout see:
browseURL("http://finzi.psych.upenn.edu/R/library/igraph/html/layout.html")
opar <- par()$mar; par(mar=rep(0, 4)) #Give the graph lots of room
plot(g, layout=layout1)


#adjust the widths of the edges and add distance measure labels
#use 1 - binary (?dist) a proportion distance of two vectors
#1 is perfect and 0 is no overlap (using 1 - binary)

edge.weight <- 7  #a maximizing thickness constant
z1 <- edge.weight*(1-dist(t(X), method="binary"))
E(g)$width <- c(z1)[c(z1) != 0] #remove 0s: these won't have an edge
z2 <- round(1-dist(t(X), method="binary"), 2)
E(g)$label <- c(z2)[c(z2) != 0]
plot(g, layout=layout1) #check it out!

SUMS <- diag(Z) #frequency (same as colSums(X))
label.size <- .5 #a maximizing label size constant
V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + label.size
plot(g, layout=layout1) #check it out!

#add factor information via vertex color
set.seed(15)
V(g)$gender <- rbinom(10, 1, .4)
V(g)$color <- ifelse(V(g)$gender==0, "pink", "lightblue")

plot(g, layout=layout1) #check it out!
plot(g, layout=layout1, edge.curved = TRUE) #curve it up

par(mar=opar) #reset margins

#interactive version
tkplot(g)  #an interactive version of the graph
tkplot(g, edge.curved =TRUE)

