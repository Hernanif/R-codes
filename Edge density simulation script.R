library(igraph)

dieta1<-read.table("laselvanet9.12.2016.txt", h=T)
d1<-graph_from_incidence_matrix(dieta1, weight=TRUE)

#Creates a loop for the generation of 100 random bipartite networks according to the formula
x <- vector()
for(i in 1:1000){
  pmedium<-bipartite.random.game(7, 16, p=edge_density(d1), directed = FALSE) 
  x[i] <- mean_distance(pmedium)
  #print(plow)
}


#Gives the summary for the distribution of the random networks
summary(x)

#Calculates the mean of the random genreated networks
mean(x)

#Calculates the standard deviation of the randomgenerated networks
sd(x)

#Calculates the value of the observed network
mean_distance(pmedium)

library(tigerstats)

a <- 681.30   #valor a ser testado
s <- 162.07  #desvio-padrao
n <- 57  #sample size
xbar <- 290.0 #average
z <- (xbar-a)/(s/sqrt(n)) #test for value of z-score
z #z-score value
2*pnorm(-abs(z)) #calculates the p-value


pnormGC(273.50  , region="above", mean=290.0, sd=162.07)
