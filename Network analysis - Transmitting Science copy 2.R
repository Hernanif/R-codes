##### Script wrote from the Course of Transmitting Science with Network Analysis
#25/10/2016 
#Hernani Oliveira - oliveira.hfm@gmail.com


###Community detection
#It is important to stress that the identification of struc- tural clusters is possible
#only if graphs are sparse, i. e. if the number of edges m is of the order of the number 
#of nodes n of the graph. If m ??? n, the distribution of edges among the nodes is too 
#homogeneous for communities to make sense2. In this case the problem turns into some- 
#thing rather different, close to data clustering (Gan et al., 2007), which requires 
#concepts and methods of a different nature. The main difference is that, while 
#communities in graphs are related, explicitly or implicitly, to the concept of edge 
#density (inside versus outside the community), in data clustering communities are sets 
#of points which are ???close??? to each other, with respect to a measure of dis- tance or 
#similarity, defined for each pair of points. Some classical techniques for data clustering, 
#like hierarchical, partitional and spectral clustering will be discussed later in the 
#review (Sections IV.B, IV.C and IV.D), as they are sometimes adopted for graph clustering 
#too. Other standard procedures for data clustering include neural network clustering 
#techniques like, e. g., self-organizing maps and multi-dimensional scaling techniques 
#like, e. g., singular value decomposition and principal component analysis (Gan et al., 2007).

#A required property of a community is connectedness. We expect that for C to be a community 
#there must be a path between each pair of its vertices, running only through vertices of C. 
#This feature simplifies the task of community detection on disconnected graphs, as in this 
#case one just analyzes each connected component separately, unless special constraints are 
#imposed on the resulting clusters.

###Cliques
#In graph terms, this corresponds to a clique, i. e. a subset whose vertices are all adjacent 
#to each other. In social network analysis, a clique is a maximal sub- graph, whereas in graph 
#theory it is common to call cliques also non-maximal subgraphs. Triangles are the simplest cliques,
#and are frequent in real networks. But larger cliques are less frequent. Moreover, the condition 
#is really too strict: a subgraph with all possible internal edges except one would be an extremely 
#cohesive sub- group, but it would not be considered a community un- der this recipe. Another problem 
#is that all vertices of a clique are absolutely symmetric, with no differentia- tion between them. 
#In many practical examples, instead, we expect that within a community there is a whole hi- erarchy 
#of roles for the vertices, with core vertices co- existing with peripheral ones. We remark that 
#vertices may belong to more cliques simultaneously, a property which is at the basis of the Clique 
#Percolation Method of Palla et al. (Palla et al., 2005) (see Section XI.A). From a practical point 
#of view, finding cliques in a graph is an NP-complete problem (Bomze et al., 1999). The Bron- Kerbosch 
#method (Bron and Kerbosch, 1973) runs in a time growing exponentially with the size of the graph.


#It is however possible to relax the notion of clique, defining subgroups which are still 
#clique-like objects. A possibility is to use properties related to reachability, i. e. 
#to the existence (and length) of paths between vertices. An n-clique is a maximal subgraph 
#such that the distance of each pair of its vertices is not larger than n (Alba, 1973; 
#Luce, 1950). For n = 1 one recovers the definition of clique, as all vertices are adjacent, 
#so each geodesic path between any pair of vertices has length 1. This def- inition, more 
#flexible than that of clique, still has some limitations, deriving from the fact that 
#the geodesic paths need not run on the vertices of the subgraph at study, but may run 
#on vertices outside the subgraph. In this way, there may be two disturbing consequences. 
#First, the diameter of the subgraph may exceed n, even if in princi- ple each vertex of 
#the subgraph is less than n steps away from any of the others. Second, the subgraph may 
#be disconnected, which is not consistent with the notion of cohesion one tries to enforce

#To avoid these problems, Mokken (Mokken, 1979) has suggested two possible al
#ternatives, the n-clan and the n-club. An n-clan is an n-clique whose diameter is not 
#larger than n, i. e. a sub- graph such that the distance between any two of its ver- 
#tices, computed over shortest paths within the subgraph, does not exceed n. An n-club, 
#instead, is a maximal subgraph of diameter n. The two definitions are quite close: the 
#difference is that an n-clan is maximal under the constraint of being an n-clique, whereas 
#an n-club is maximal under the constraint imposed by the length of the diameter.

#Another measure related to structural equivalence is the Pearson correlation between 
#columns or rows of the ad- jacency matrix,
#Cij= k
#??i =  j(Aij ?????i)2/n.
#An alternative measure is the number of edge- (or vertex-) independent paths between 
#two vertices. Inde- pendent paths do not share any edge (vertex), and their number is 
#related to the maximum flow that can be con- veyed between the two vertices under the 
#constraint that each edge can carry only one unit of flow (max-flow/min- cut theorem (Elias et al., 1956)).

#A division of a graph into over- lapping (or fuzzy) communities is called cover.

install.packages("igraph")
library(igraph)
g<-make_ring(8)
str(g)
plot(g, vertex.size=30, vertex.label.cex=2)
?plot

#for details see?help
net1<-make_empty_graph(10)
net2<-make_full_graph(10, loops=FALSE)
net3<-make_ring(10)
net4<-make_lattice(c(2,5,1))
net5<-make_lattice(length=3, dim=3)
net6<-make_tree(10,2)
net7<-make_star(10, mode="out")

par(mfrow=c(2,4))
plot(net1)
plot(net2)
plot(net3)
plot(net4)
plot(net5)
plot(net6)
plot(net7)

#Examining attributes
graph_attr(graph)
graph_attr_names(graph)
graph_attr(graph, name)

#Modifying and removing attributes
set_graph_attr(graph, name, value)
delete_graph_attr()

#Check the arguments of the network
graph_attr(net7)

#Changing the attributes of the networks
net7<-set_graph_attr(net7, "name", "My Start Network")
net7

net7 <- delete_graph_attr(net7, "name")
net7

#Direactly examine and set attributes
net7$name <- "Out-star" #I like more maths
net7$name

#Directly create new attributes
#color for nodes
V(net7)$color <- c("pink", "skyblue")

#Weights for links
E(net7)$weight <- seq(0.2, length=9, by=0.5)

par(mar=c(1,1,1,1))
plot(net7, vertex.size=30, vertex.label.cex=3, edge.width=E(net7)$weight)


V(net1)$color<-c("pink", "black")
plot(net1, vertex.size=30)


Vcount(igraph object) #gives the number of nodes
Ecount(igraph object) #gives the number of links

#Random example of how to create a new attribute to the graph
V(net1)$size<-runif(vcount(net1), 20, 60)
par(mar=c(1,1,1,1))
plot(net1)
plot(net1, vertex.label=NA, vertex.size=V(net1)$size)

#Sets attributes to the graphs
V(graph)$attribute
E(graph)$attribute

###Types of network data
#Edge list, adjacency matrix and data frmae

#Creates an edge list 
el<-matrix(c(1,2,3,4,5,2,4,1,3,1), ncol=2)
el2net<-graph_from_edgelist(el, directed=TRUE)
el2net
plot(el2net)

#Creates 7 nodes and 12 links
el<-matrix(NA, ncol=2, nrow=12)
el[,1] <-c("Pepe", "Kuba", "John", "Piere")
el[,2] <-c("Alice", "Elisa", "Sara")
el2net<-graph_from_edgelist(el, directed=FALSE)
el2net$name <- "My Network"
str(el2net)
plot(el2net)

#Analysing skull morphology
library(gdata)
skull <- read.xls("homo.skull.xls", sheet=1)
A <- data.matrix(skull, rownames.force=TRUE)
library



data.matrix(data, rownames.force=TRUE)
str(A)
Skull <-graph_from_adjacency_matrux(A, mode="undirected")

#create network
netD <- graph_from_data_frame(relations) #finish typing the command





elexample<-matrix(c(1,2,3,4,5,6,7,7,6,5,4,3,2,1), ncol=2)
el2net<-graph_from_edgelist(elexample, directed=TRUE)
plot(el2net)



degree(g, v=V(g), mode=c("all", "out", "in"), loops=TRUE, normalized=FALSE)

degree(delete_vertex_attr(homoU, "name"))
degree(homoU)

degree.distribution(g, v=V(g), cumulative=FALSE, mode=c("all", "out", "in"))
degree_distribution(homoU, v=1:5)

plot(degree_distribution(homoU, mode=c("out") ))
plot(degree_distribution(homoU, mode=c("in") ))
plot(degree_distribution(homoU, mode=c("all") ))

edge_density(el2net, loops=FALSE)

plot(degree_distribution(homoU, v=1:5))

degree_distribution(g, V=which(V(g)$sex=="male"))


shortest_paths(homoU, 1, 21)

distance_table(graph, directed=TRUE)
mean_distance(graph, directed=TRUE, unconnected=TRUE) #main function interesting for the small world distribution
mean_distance(homoU)
barplot(distance_table(homoU)$res, names.arg=c("k=1", "k=2", "k=3"))
shortest_paths

#Calculate C and L
C <- transitivity(homoU, type="average")
L <- mean_distance(homoU)


transitivity(g, v=V(g), type=c("local", "weighted", "global", ))




####Observation: take a look into the ape package and function to convert an igraph object
l<-sample_k_regular(10, 1, directed=FALSE)
m<-sample_k_regular(10, 2, directed=FALSE)
n<-sample_k_regular(10, 3, directed=FALSE)

plot(l)
plot(m)
plot(n)

###Random network (The Erdos & Renyi Model)
sample_gnp(n, p, directed=FALSE)
sample_gnm(n, m, directed=FALSE)

set.seed(1) #it will be used to set the same amount of nodes to have a fixed value for the rando networks created

ER <- sample_gnp(1000, 0.1)
plot(degree.distribution(ER))

#The Barabasi & Albert Model
sample_pa(n, power=1, m=newlinks, zero.appeal=1, directed=TRUE)
BA <- sample_pa(500, m=1, directed=FALSE)
plot(BA, vertex.size=2, vertex.label=NA)

#The Proximity model - its has its connections determined by a geometric constraint
sample_grg(nodes, radius, torus=FALSE, coords=FALSE)

set.seed(20)
PG <- sample_grg(20, 0.3, coords=TRUE)
plot(PG)




#Plot function uses coords if TRUE
plot(PG, vertex.size=10, vertex.label=NA)



library(gdata)
skull <-read.xls("homo.skull.xls", sheet=1)
str(skull[,1:6], vec.len=1) 
skulls$Occipita
skull$Frontal

#####Null models
library(igraph)
sample_k_regular(no.of.nodes, k, directed=FALSE)
sample_k_regular(11, 3, directed=FALSE)

# jackknife of modularity Q value #How to calculate a value to test for statiscal significance of modular values
jackknife_Q = function(graph, membership){
  Qi <- vector()
  for (j in 1:ecount(graph)){
    g <- delete_edges(graph, j)
    Qi[j] <- modularity(g, membership)
  }
  ss <- sum((Qi-mean(Qi))^2)
  n <- (ecount(graph)-1)/ecount(graph)
  Q.error <- sqrt(n*ss)
  return(Q.error)
}


cw <- cluster_walktrap(d1)
d<-membership(cw)
modularity(cw)
plot(cw, d1, vertex.size=2, vertex.label=NA)
title("Membership Rainforest - El Nino Year")

jackknife_Q(d1, d)


#Heuristic models
cluster_walktrap(d1, steps=4)
modular<-cluster_walktrap(d1, steps=4)
plot(as.dendrogram(modular))

#Compare values from the networks
compare(comm1, comm2, method = c("vi", "nmi", "split.join", "rand", "adjusted.rand")) #Borja usually uses the NMI that actually means the normalized value

cw <- cluster_walktrap(d1)
cd <- fastgreedy.community(d1)
compare(cw, cd, method = c("nmi"))

# Function: Wilcox test to evaluate modules 
community.significance.test <- function(graph, vids, ...) {
  subgraph <- induced_subgraph(graph, vids)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vids) - in.degrees
  wilcox.test(in.degrees, out.degrees, alternative="greater")
}

#define vertice ids that you are testing to be part of the same modules - This formula is useful for the formula above
vids = which(M$membership==2) 


# Function: Wilcox test to evaluate modules => it is used to test if the modules are actually statistically significantly different from random of from other connections in the network
community.significance.test <- function(d1, vids, ...) {
  subgraph <- induced_subgraph(d1, vids)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(d1, vids) - in.degrees
  wilcox.test(in.degrees, out.degrees, alternative="greater")
}

#define vertice ids that you are testing to be part of the same modules - This formula is useful for the formula above
vids = which(cw$membership==1) 

community.significance.test(d1, vids)




####### Transplanted from the igraph website of the Igraph creator
1 Clique percolation

Clique percolation is a community detection method developed by Gergely Palla and his co-workers, see Palla, Gergely, Imre Der??nyi, Ill??s Farkas, and Tam??s Vicsek. 2005. Uncovering the overlapping community structure of complex networks in nature and society. Nature 435(7043):814-8. Pubmed Arxiv.
This algorithm is not implemented in igraph, but here is a quick (and rather inefficient) version to do it:
  
  clique.community <- function(graph, k) {
    clq <- cliques(graph, min=k, max=k)
    edges <- c()
    for (i in seq_along(clq)) {
      for (j in seq_along(clq)) {
        if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
          edges <- c(edges, c(i,j)-1)
        }
      }
    }
    clq.graph <- simplify(graph(edges))
    V(clq.graph)$name <- seq_len(vcount(clq.graph))
    comps <- decompose.graph(clq.graph)
    
    lapply(comps, function(x) {
      unique(unlist(clq[ V(x)$name ]))
    })
  }
2 Label propagation algorithm by Raghavan et al.

Usha Nandini Raghavan, R??ka Albert and Soundar Kumara. 2007. Near linear time algorithm to detect community structures in large-scale networks, Phys. Rev. E 76, 036106 Arxiv

A quick implementation by Peter McMahan, he sent this to the igraph-help mailing list.

largeScaleCommunity <- function(g,mode="all"){
  V(g)$group <- as.character(V(g))
  thisOrder <- sample(vcount(g),vcount(g))-1
  t <- 0
  done <- FALSE
  while(!done){
    t <- t+1
    cat("\rtick:",t)
    done <- TRUE ## change to FALSE whenever a node changes groups              
    for(i in thisOrder){
      ## get the neighbor group frequencies:                                    
      groupFreq <- table(V(g)[neighbors(g,i,mode=mode)]$group)
      ## pick one of the most frequent:                                         
      newGroup <- sample(names(groupFreq) [groupFreq==max(groupFreq)],1)
      if(done){done <- newGroup==V(g)[i]$group}
      V(g)[i]$group <- newGroup
    }
  }
  ## now fix any distinct groups with same labels:                              
  for(i in unique(V(g)$group)){
    ## only bother for connected groups                                         
    if(!is.connected(subgraph(g,V(g)[group==i]))){
      theseNodes <- V(g)[group==i]
      theseClusters <- clusters(subgraph(g,theseNodes))
      ## iterate through the clusters and append their names                    
      for(j in unique(theseClusters$membership)){
        V(g)[theseNodes[theseClusters$membership==j]]$group <- paste(i,j,sep=".")
      }
    }
  }
  return(g)
}
3 How to use the community detection algorithms?

3.1 Code

G??bor wrote this in the mailing-list.

memberships <- list()

### edge.betweenness.community
ebc <- edge.betweenness.community(G)
mods <- sapply(0:ecount(G), function(i) {
  g2 <- delete.edges(G, ebc$removed.edges[seq(length=i)])
  cl <- clusters(g2)$membership
  modularity(G, cl)
})

g2 <- delete.edges(G, ebc$removed.edges[1:(which.max(mods)-1)])
memberships$`Edge betweenness` <- clusters(g2)$membership

### fastgreedy.community
fc <- fastgreedy.community(G)
memb <- community.to.membership(G, fc$merges,
                                steps=which.max(fc$modularity)-1)
memberships$`Fast greedy` <- memb$membership

### leading.eigenvector.community
lec <- leading.eigenvector.community(G)
memberships$`Leading eigenvector` <- lec$membership

### spinglass.community
sc <- spinglass.community(G, spins=10)
memberships$`Spinglass` <- sc$membership

### walktrap.community
wt <- walktrap.community(G, modularity=TRUE)
wmemb <- community.to.membership(G, wt$merges,
                                 steps=which.max(wt$modularity)-1)
memberships$`Walktrap` <- wmemb$membership

### label.propagation.community
memberships$`Label propagation` <- label.propagation.community(G)
3.2 Example

G <- graph.disjoint.union(graph.atlas(1000),graph.atlas(1001),graph.atlas(1002))
G <- add.edges(G,c(2,10,11,15,16,0))                                
G$layout <- layout.kamada.kawai                                    
V(G)$color <- rainbow(3)[memberships$'Edge betweenness'+1]                
plot(G)
4 Testing the significance of a community

The following code snippet performs a Wilcoxon rank-sum test on the "internal" and "external" degrees of a community in order to quantify its significance. Let us call the edges within a community "internal" and the edges connecting the vertices of a community with the rest of the graph "external". The null hypothesis of the test is that there is no difference between the number of "internal" and "external" edges incident to a vertex of the community. More internal than external edges show that the community is significant; less internal than external edges show that the community is in fact an "anti-community". The p-value of the test performed by this function will be close to zero in both cases; the value of the test statistic tells us whether we have a community or an anti-community.

community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}




