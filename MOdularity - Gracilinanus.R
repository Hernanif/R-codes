library(igraph)
library(tigerstats)


a <- 681.30   #valor a ser testado
s <- 162.07  #desvio-padrao
n <- 57  #sample size
xbar <- 290.0 #average
z <- (xbar-a)/(s/sqrt(n)) #test for value of z-score
z #z-score value
2*pnorm(-abs(z)) #calculates the p-value


dieta1<-read.table("FAL-CHUVA.txt", h=T)
dieta2<-read.table("FAL-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

cfg1<-cluster_fast_greedy(d1)
cfg2<-cluster_fast_greedy(d2)
abs(modularity(cfg1) - modularity(cfg2))
modularity(cfg1)
modularity(cfg2)


#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium1<-sample_bipartite(14, 72, p=edge_density(d1), type="Gnp" )
  pmedium2<-sample_bipartite(12, 44, p=edge_density(d2), type="Gnp")
  cfg1<-cluster_fast_greedy(pmedium1)
  cfg2<-cluster_fast_greedy(pmedium2)
  x[i] <-  abs(modularity(cfg1) - modularity(cfg2))
  #print(plow)
}

centr_clo(d1)

#Calculates the mean of the random genreated networks
mean(x)

#Calculates the standard deviation of the randomgenerated networks
sd(x)

pnormGC(0.1574, region="below", mean=0.1490, sd=0.0704)


###################

dieta1<-read.table("JB1-CHUVA.txt", h=T)
dieta2<-read.table("JB1-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

cfg1<-cluster_fast_greedy(d1)
cfg2<-cluster_fast_greedy(d2)
abs(modularity(cfg1) - modularity(cfg2))
modularity(cfg1)
modularity(cfg2)


#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium1<-sample_bipartite(13, 52, p=edge_density(d1), type="Gnp" )
  pmedium2<-sample_bipartite(7, 36, p=edge_density(d2), type="Gnp")
  cfg1<-cluster_fast_greedy(pmedium1)
  cfg2<-cluster_fast_greedy(pmedium2)
  x[i] <-  abs(modularity(cfg1) - modularity(cfg2))
  #print(plow)
}

#Calculates the mean of the random genreated networks
mean(x)

#Calculates the standard deviation of the randomgenerated networks
sd(x)

pnormGC(0.1862, region="below", mean=0.1119, sd=0.0700)


#############

a <- 681.30   #valor a ser testado
s <- 162.07  #desvio-padrao
n <- 57  #sample size
xbar <- 290.0 #average
z <- (xbar-a)/(s/sqrt(n)) #test for value of z-score
z #z-score value
2*pnorm(-abs(z)) #calculates the p-value


dieta1<-read.table("JB2-CHUVA-2.txt", h=T)
dieta2<-read.table("JB2-SECA.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)
d2<-graph_from_incidence_matrix(dieta2, weight=TRUE)

cfg1<-cluster_fast_greedy(d1)
cfg2<-cluster_fast_greedy(d2)
abs(modularity(cfg1) - modularity(cfg2))
modularity(cfg1)
modularity(cfg2)


#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium1<-sample_bipartite(13, 58, p=edge_density(d1), type="Gnp" )
  pmedium2<-sample_bipartite(7, 48, p=edge_density(d2), type="Gnp")
  cfg1<-cluster_fast_greedy(pmedium1)
  cfg2<-cluster_fast_greedy(pmedium2)
  x[i] <-  abs(modularity(cfg1) - modularity(cfg2))
  #print(plow)
}

#Calculates the mean of the random genreated networks
mean(x)

#Calculates the standard deviation of the randomgenerated networks
sd(x)

pnormGC(0.1975, region="below", mean=0.0881, sd=0.0615)




#########
