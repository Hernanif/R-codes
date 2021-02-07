#R Script for Calculating Differences between Pairs of Weighted Networks using the Patefield null model
#Author: Hernani F. M. Oliveira
#Date: 07/02/2021

#Install packages needed fot the analysis
install.packages("bipartite")
install.packages("tigerstats")
install.packages("spaa")

#Loads libraries required for the analysis
library(bipartite)
library(tigerstats)
library(spaa)

#Loads the networks that will be compared
dados<-read.table("rede1.txt", head=TRUE)
dados
dados2<-read.table("rede2.txt", head=TRUE)
dados2
dados_neo=as.matrix(dados)
dados_neo
dados2_neo=as.matrix(dados2)
dados2_neo

#Calculate the differences between the observed networks regarding the chosen metric. Obs: you can chose Shannon or Levins
orig=abs(sum(niche.width(dados_neo, method = "levins")[1, ])/ncol(niche.width(dados_neo, method = "levins")) - sum(niche.width(dados2_neo, method = "levins")[1, ])/ncol(niche.width(dados2_neo, method = "levins")))
orig

#Creates a loop for the generation of 1000 random bipartite networks according to the formula. Obs: you can change the null model
x <- vector()
for(i in 1:1000){
  pmedium1<-nullmodel(dados_neo, N=1, method=2)
  pmedium2<-nullmodel(dados2_neo, N=1, method=2)
  x[i]<-abs(sum(niche.width(pmedium1, method = "levins")[1, ])/ncol(niche.width(pmedium1, method = "levins")) - sum(niche.width(pmedium2, method = "levins")[1, ])/ncol(niche.width(pmedium2, method = "levins")))
}
x

#Calculates the significance of the observed difference in relation to the randomized differences. Obs: you can change to below or above depending on the direction of the question you aim to ask
pnormGC(orig, region="below", mean(x), sd(x))

