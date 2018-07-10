library(igraph)
library(tigerstats)

############

#JB1

dieta1<-read.table("JB1-CHUVA.txt", h=T)
dieta2<-read.table("JB1-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium<-bipartite.random.game(13, 51, p=edge_density(d1), directed = FALSE) 
  cfg<-cluster_fast_greedy(pmedium)
  modularity(cfg)
  pmedium2<-bipartite.random.game(7, 36, p=edge_density(d2), directed = FALSE) 
  cfg1<-cluster_fast_greedy(pmedium2)
  modularity(cfg1)
  x[i] <- abs(modularity(cfg1) - modularity(cfg))
  #print(plow)
}
mean(x)
sd(x)

shapiro.test(x)

hist(x)
abline(v=(modularity(fg1) - modularity(fg2)), col="red", lwd=2, xlab="")

count(x>0.186)

fg1<-cluster_fast_greedy(d1)
fg2<-cluster_fast_greedy(d2)
modularity(fg1) - modularity(fg2)

pnormGC(0.186, region="below", mean=0.113, sd=0.073)


#####################################

#JB2

dieta1<-read.table("JB2-CHUVA-2.txt", h=T)
dieta2<-read.table("JB2-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium<-bipartite.random.game(13, 58, p=edge_density(d1), directed = FALSE) 
  cfg<-cluster_fast_greedy(pmedium)
  modularity(cfg)
  pmedium2<-bipartite.random.game(7, 48, p=edge_density(d2), directed = FALSE) 
  cfg1<-cluster_fast_greedy(pmedium2)
  modularity(cfg1)
  x[i] <- abs(modularity(cfg1) - modularity(cfg))
  #print(plow)
}
mean(x)
sd(x)

count(x>0.186)

hist(x)
abline(v=(modularity(fg1) - modularity(fg2)), col="red", lwd=2, xlab="")

fg1<-cluster_fast_greedy(d1)
fg2<-cluster_fast_greedy(d2)
modularity(fg1) - modularity(fg2)

pnormGC(0.197, region="below", mean=0.088, sd=0.061)


#############
#JB3

dieta1<-read.table("JB3-CHUVA.txt", h=T)
dieta2<-read.table("JB3-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium<-bipartite.random.game(12, 27, p=edge_density(d1), directed = FALSE) 
  cfg<-cluster_fast_greedy(pmedium)
  modularity(cfg)
  pmedium2<-bipartite.random.game(10, 37, p=edge_density(d2), directed = FALSE) 
  cfg1<-cluster_fast_greedy(pmedium2)
  modularity(cfg1)
  x[i] <- abs(modularity(cfg1) - modularity(cfg))
  #print(plow)
}
mean(x)
sd(x)

count(x>0.141)

hist(x)
abline(v=(modularity(fg1) - modularity(fg2)), col="red", lwd=2, xlab="")

fg1<-cluster_fast_greedy(d1)
fg2<-cluster_fast_greedy(d2)
fg1
fg2
modularity(fg1) - modularity(fg2)

pnormGC(0.141, region="below", mean=0.081, sd=0.060)

#######################

#FAL comparison

dieta1<-read.table("FAL-CHUVA.txt", h=T)
dieta2<-read.table("FAL-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium<-bipartite.random.game(14, 72, p=edge_density(d1), directed = FALSE) 
  cfg<-cluster_fast_greedy(pmedium)
  pmedium2<-bipartite.random.game(12, 44, p=edge_density(d2), directed = FALSE) 
  cfg1<-cluster_fast_greedy(pmedium2)
  modularity(cfg1)
  x[i] <- abs(modularity(cfg1) - modularity(cfg))
  #print(plow)
}
mean(x)
sd(x)

hist(x)
abline(v=(modularity(fg1) - modularity(fg2)), col="red", lwd=2, xlab="")

count(x>0.186)

fg1<-cluster_fast_greedy(d1)
fg2<-cluster_fast_greedy(d2)
modularity(fg1) - modularity(fg2)

pnormGC(0.157, region="below", mean=0.145, sd=0.070)







