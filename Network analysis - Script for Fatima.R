##### Network analysis for Fatima Data
#Written by: Hernani Oliveira
#Date: 24/01/2017

#Converts a given bipartite web into a graph object
library(bipartite)
library(igraph)
library(statnet)

par(mfrow=c(1,1))
data<-read.table("guanawetnet2.11.2016.txt", h=T)
d1<-graph_from_incidence_matrix(data, weight=TRUE)
attributes<-read.table("attr.guanawetnet2.11.2016.txt", h=T)

V(d1)$ID=as.character(attributes$ID[match(V(d1)$name,attributes$Node.ID)]) # This code says to create a vertex attribute called "Sex" by extracting the value of the column "Sex" in the attributes file when the Bird ID number matches the vertex name.
V(d1)$ID

V(d1)$color=V(d1)$ID #assign the "Sex" attribute as the vertex color
V(d1)$color=gsub("Plant","green",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("Bat","black",V(d1)$color) #Bats will be black

V(d1)$shape=V(d1)$ID #assign the "Sex" attribute as the vertex color
V(d1)$shape=gsub("Plant","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Bat","circle",V(d1)$shape) #Bats will be black

#PLot the network
plot.igraph(d1, axes=FALSE, layout=layout.kamada.kawai, edge.width=E(d1)$weight/2, edge.color="grey", vertex.size=5)
title("Full mutualistic network - Bat-Plants - Kamada.Kawai")

legend(-1.5, -0.5, legend=c("Bat", "Plant"), 
       col=c("black", "green"), pch=c(16,15),
       pt.cex=1.0,cex=1.2, bty="n", title="Full network of Interactions")

fc <- fastgreedy.community(d1)
cw <- cluster_walktrap(d1)
membership(fc)
membership(cw)
modularity(fc)
modularity(cw)
plot(fc, d1, vertex.size=2)
plot(cw, d1, vertex.size=2)
plot_dendrogram(fc)
plot_dendrogram(cw)
