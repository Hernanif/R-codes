
#Script for calculating and comparing modularity between different algorithms
#Written by:Hernani Oliveira
#E-mail: oliveira.hfm@gmail.com
#Date: 11/03/2017


library(igraph)
library(igraphdata)

Name                               Directed            Weighted        Components 
Edge-betweenness                      T                   T                 T  
Leading eigenvector                   F                   F                 T
Fast-greedy                           F                   T                 T
Louvain                               F                   T                 T
Walktrap                              F                   T                 F
Label propagation                     F                   T                 F
Infomap                               T                   T                 T
Spinglass                             F                   T                 F
Optimal                               F                   T                 T

#Loads the datas
dieta<-read.table("Elninodry92withoutzeros.txt", h=T)

#Transforms dataset into an igraph object
d1<-graph_from_incidence_matrix(dieta, weight=TRUE)


#Calculates modularity using different algorithms
cw<-cluster_walktrap(d1)
modularity(cw)
membership(cw)

ceb<-cluster_edge_betweenness(d1)
modularity(ceb)
membership(ceb)

cs<-cluster_spinglass(d1)
modularity(cs)
membership(cs)

cfg<-cluster_fast_greedy(d1)
modularity(cfg)
membership(cfg)

cle<-cluster_leading_eigen(d1)
modularity(cle)
membership(cle)

cl<-cluster_louvain(d1)
modularity(cl)
membership(cl)

co<-cluster_optimal(d1)
modularity(co)
membership(co)

#Compares the found modularity algorithms against one another
compare(cw, ceb, method="adjusted.rand")
compare(cw, cs, method="adjusted.rand")
compare(cw, cfg, method="adjusted.rand")

#Plots the modules of interaction using the different algorithms for comparison
op<-par(mfrow=c(4,2), mar=c(1,0,1,0))
plot(ceb, d1, vertex.label=V(d1), main="Edge betweenness")
plot(cfg, d1, vertex.label=V(d1), main="Fast greedy")
plot(cle, d1, vertex.label=V(d1), main="Leading Eigenvector")
plot(cs, d1, vertex.label=V(d1), main="Spinglass")
plot(cw, d1, vertex.label=V(d1), main="Walktrap")
plot(cl, d1, vertex.label=V(d1), main="Louvain")
plot(co, d1, vertex.label=V(d1), main="Cluster Optimal")
