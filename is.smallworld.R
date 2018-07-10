## Function to estimate the presence of a small-world organization
## by Borja Esteve-Altava, April 2015
## Requires package "igraph"
        # Input: an igraph object
        # Outputs: TRUE/FALSE presence of small-workd and score

is.smallworld = function(graph, rep=1000){
        
        # function that compares the C and L of empirical and random equivalent networks
        test.sw = function(graph){
                deg <- degree(graph)
                Crand <- matrix(0,nrow=1, ncol=rep)
                Lrand <- matrix(0,nrow=1, ncol=rep)
                for (i in 1:rep){
                        # generate random equivalent network
                        Grand <- degree.sequence.game(deg, method="simple")
                        # calculate C and L
                        Crand[i] <- transitivity(Grand, type="average", isolates="NaN")
                        Lrand[i] <- average.path.length(Grand)
                }
                # C and L empirical and the average of the random ones
                C <- transitivity(graph, type="average", isolates="NaN")
                L <- average.path.length(graph)
                CR <- mean(Crand, na.rm=TRUE)
                LR <- mean(Lrand, na.rm=TRUE)
                smallwordness <- (C/CR)/(L/LR)
                smallwordness
        }
        
        # check if the graph is connected
        check1 <- is.connected(graph)
          # if connected: calculate small-world
        if (check1==TRUE){
                print("This graph is connected")
                print(paste("Is small-world?", test.sw(graph)>=0.012*vcount(graph)^1.11))
                print(paste("score =", round(test.sw(graph), 3)))
          # if not connected: calculate small-world for each cluster 
        } else{
                print("This graph is not connected")
                for (i in 1:no.clusters(graph)){
                        subgraph <- induced.subgraph(graph, v=clusters(graph)$membership==i)
                        print(paste("Cluster", i))
                        print(paste("Is small-world?", test.sw(subgraph)>=0.012*vcount(subgraph)^1.11))
                        print(paste("score =", round(test.sw(subgraph), 3)))
                }
        }
}