# Tutorial on graphs from https://solomonmessing.wordpress.com/2012/09/30/working-with-bipartiteaffiliation-network-data-in-r/
require(igraph);
require("Matrix");
df <- data.frame( person =
                      c('Sam','Sam','Sam','Greg','Tom','Tom','Tom','Mary','Mary'), group =
                      c('a','b','c','a','b','c','d','b','d'), stringsAsFactors = F)

df;
#   person group
# 1    Sam     a
# 2    Sam     b
# 3    Sam     c
# 4   Greg     a
# 5    Tom     b
# 6    Tom     c
# 7    Tom     d
# 8   Mary     b
# 9   Mary     d
m <- table( df );
M <- as.matrix( m );
# library('Matrix');
A <- spMatrix(nrow=length(unique(df$person)),
              ncol=length(unique(df$group)),
              i = as.numeric(factor(df$person)),
              j = as.numeric(factor(df$group)),
              x = rep(1, length(as.numeric(df$person))) );
row.names(A) <- levels(factor(df$person));
colnames(A) <- levels(factor(df$group));
A;
# 4 x 4 sparse Matrix of class "dgTMatrix"
#       a b c d
# Greg 1 . . .
# Mary . 1 . 1
# Sam  1 1 1 .
# Tom  . 1 1 1
Arow <- tcrossprod(A);
Arow;
M1 <- M[,1]
M2 <- M[,2]
M3 <- M[,3]
M4 <- M[,4]
# 4 x 4 sparse Matrix of class "dsCMatrix"
#       Greg Mary Sam Tom
# Greg    1    .   1   .
# Mary    .    2   1   2
# Sam     1    1   3   2
# Tom     .    2   2   3
Acol <- tcrossprod(t(A));
Acol;
# 4 x 4 sparse Matrix of class "dsCMatrix"
#   a b c d
# a 2 1 1 .
# b 1 3 2 2
# c 1 2 2 1
# d . 2 1 2
g1 <- graph.adjacency(tcrossprod(M1), weighted=TRUE, mode ='undirected');
g1 <- simplify(g1);
E(g1)$label <- "a";
E(g1)$color <- "red";
V(g1)$label <- row.names(A);
g2 <- graph.adjacency(tcrossprod(M2), weighted=TRUE, mode ='undirected');
g2 <- simplify(g2);
E(g2)$label <- "b";
E(g2)$color <- "blue"
V(g2)$label <- row.names(A);
g3 <- graph.adjacency(tcrossprod(M3), weighted=TRUE, mode ='undirected');
g3 <- simplify(g3);
E(g3)$label <- "c";
E(g3)$color <- "light blue"
V(g3)$label <- row.names(A);
g4 <- graph.adjacency(tcrossprod(M4), weighted=TRUE, mode ='undirected');
g4 <- simplify(g4);
E(g4)$label <- "d";
E(g4)$color <- "dark green"
V(g4)$label <- row.names(A);

plot(g1)
plot(g2)
plot(g3)
plot(g4)
community.newman <- function(g) {
    deg <- degree(g)
    ec <- ecount(g)
    B <- get.adjacency(g) - outer(deg, deg, function(x,y) x*y/2/ec)
    diag(B) <- 0
    eigen(B)$vectors[,1]
}

V(g1)$size <- scale(abs(mem), 15, 25)

g <- graph.empty() + vertices(V(g1)) + edges(E(g2))

plot(g)
# g3 <- graph_from_adjacency_matrix(Arow, mode = "undirected", diag = FALSE)
plot(g3)


# As <- as.data.frame(as.matrix(A)); ### THIS IS HOW TO MAKE IT WORK!!!
# g1 <- graph.data.frame(A, directed = FALSE);
# plot(g1)
# g1
#
#
# for (i in V(A)) {
#     for (j in names(df)) {
#         A <- set.vertex.attribute(A,
#                                            j,
#                                            index = i,
#                                            attributes[i + 1, j])
#     }
# }
