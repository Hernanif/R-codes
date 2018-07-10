#R script for the analysis of the third chapter: nestedness and modularity versus phylogeny and bite force
#Written by: Hernani Oliveira
#Date: 09/05/2017

library(bipartite)
library(igraph)

####Guanacaste network
##Network analysis
#Loads the network matrix
guanacaste<-read.table("guananet2.11.2016.txt", h=T)

#Converts the matrix to a graph object
guanagraph<-graph_from_incidence_matrix(guanacaste)

#Calculates modularity according to a fast greedy algorithm
guana_fg <- cluster_fast_greedy(guanagraph)

#Calculates the membership of the nodes based on the fast greedy algorithm for modularity
membership(guana_fg)

#Calculate closeness centrality values for all the nodes in the network
closeness(guanagraph)

#Calculates nested rank for all nodes in the network
nestedrank(guanacaste)

##Phylogenetic analysis


####La Selva network
##Network analysis
#Loads the network matrix
laselva<-read.table("laselvanet9.12.2016.txt", h=T)

#Transform the matrix to an 
laselvagraph<-graph_from_incidence_matrix(laselva)

#Calculates modularity according to the fast greedy algorithm
laselva_fg <- cluster_fast_greedy(laselvagraph)

#Calculates the membership of the nodes according to modularity by the fast greedy algr
membership(laselva_fg)

#Calculate closeness centrality values for all the nodes in the network
closeness(laselvagraph)

#Calculates nested rank for all nodes in the network
nestlaselva<-nestedrank(laselva)
nestlaselva



