library(betalink)
library(bipartite)
library(igraph)

cerradototal<-read.table("fruit.flower.for.exclusions.txt", h=T)
cerradominusforests<-read.table("fruit.flower.minus.forests..txt", h=T)
cerradominussavannas<-read.table("fruit.flower.minus.savannas.txt", h=T)
cerradominuscampos<-read.table("fruit.flower.minus.campos.txt", h=T)

d1<-graph_from_incidence_matrix(cerradototal, weight=TRUE)
d2<-graph_from_incidence_matrix(cerradominusforests, weight=TRUE)
d3<-graph_from_incidence_matrix(cerradominussavannas, weight=TRUE)
d4<-graph_from_incidence_matrix(cerradominuscampos, weight=TRUE)

betalink(d1, d2, bf = B01)
betalink(d1, d3, bf = B01)
betalink(d1, d4, bf = B01)

plot(d1)
plot(d2)
plot(d3)
plot(d4)
