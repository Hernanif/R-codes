
library(statnet)
library(igraph)

#Creates and plots a random bipartite graph with 80 nodes and connectivity of 0.1
prandom<-bipartite.random.game(40, 40, p=0.5, directed = FALSE)
plot(prandom, vertex.size=2, vertex.label=NA)


#Calculates a diverse array of metrics for the network
plot(degree_distribution(prandom))

shapiro.test(x)
x<-degree_distribution(prandom)

hist(degree_distribution(prandom))


edge_density(prandom)


hist(shortest.paths(prandom))

mean_distance(prandom)

diameter(prandom)

assortativity.degree(prandom)

#Calculates and plot modularity from a given graph using the Cluster Walktrap algorithm
cwd1 <- cluster_walktrap(prandom)
modularity(cwd1)
md1<-membership(cwd1)
plot(cwd1, prandom, vertex.label=NA, vertex.size=2)

#Add two isolates to a graph
g<-add.isolates(g,2) 

prandom<-bipartite.random.game(7, 7, p=0.5, directed = FALSE)
tkplot(prandom)


library(sna)

###Calculates the clusters from the networks 
#gclust.boxstats simply takes the hclust object in h, applies cutree to form k groups, 
#and then uses boxplot on the distribution of meas by group. This can be quite handy 
#for assessing graph clusters.
###Example from the cluster calculation
#Create some random graphs
g<-rgraph(10,20,tprob=c(rbeta(10,15,2),rbeta(10,2,15)))
#Find the Hamming distances between them
g.h<-hdist(g)
#Cluster the graphs via their Hamming distances
g.c<-hclust(as.dist(g.h))
#Now display boxplots of density by cluster for a two cluster solution
gclust.boxstats(g.c,2,gden(g))


#Generate random graphs with varying density
g<-rgraph(10,20,tprob=runif(20,0,1))
#Get Hamming distances between graphs
g.h<-hdist(g)
#Plot the association of distance, density, and reciprocity
gdist.plotstats(g.h,cbind(gden(g),grecip(g)))

