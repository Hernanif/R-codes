
library(igraph)
library(bipartite)
library(tigerstats)

#Carregue a rede original ponderada
dados<-read.table("JB3-CHUVA.txt", head=TRUE)
dados1<-graph_from_incidence_matrix(dados)

#Rode o algoritmo QuanBiMo
cfg<-cluster_fast_greedy(dados1)
modularity(cfg)

#Estime a significancia de M com o algoritmo swap
#Modelo nulo de Patefiled (1981)
for (i in 1:1000) {
nulls2<- nullmodel(dados, N=1, method=5) #Pode trocar por quantas permutacoes quiser
output <- matrix(unlist(nulls2), ncol = ncol(dados), byrow = TRUE)
nulls1<-graph_from_incidence_matrix(output)
                modules.nulls<-cluster_fast_greedy(nulls1)
                x[i]<-modularity(modules.nulls)
                }

summary(x)
sd(x)
modularity(cfg)
pnormGC(modularity(cfg)  , region="below", mean=mean(x), sd=sd(x))
pnormGC(modularity(cfg)  , region="above", mean=mean(x), sd=sd(x))
JB3-CHUVA
