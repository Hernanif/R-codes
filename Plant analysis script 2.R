### Script for the analysis of Costa Rica networks

Wrote by Hernani Oliveira
E-mail: oliveiradebioh@gmail.com


##Creating and editing networks
#Changing a network from directed to undirected and vice-versa 
    #Mutual: Two directed edges are created for each undirected edge, one in each direction.
    #Arbitrary: The number of edges in the graph stays the same, an arbitrarily directed edge is created for each undirected edge.
    #Each: The number of edges remains constant, an undirected edge is created for each directed one, this version might create graphs with multiple edges.
    #Collapse: One undirected edge will be created for each pair of vertices which are connected with at least one directed edge, no multiple edges will be created.

as.directed(graph, mode = c("mutual", "arbitrary"))
g<-as.undirected(graph, mode = c("collapse", "each", "mutual"),
              edge.attr.comb = igraph_opt("edge.attr.comb"))

##Complete list of plotting arguments
plot(size= , size2=, color=, shape=, label=, label.family=, label.font=, label.cex=, label.dist=, label.degree=, label.color=, color=, width=, arrow.size=, arrow.width=, Ity=, label=, label.family=, label.font=, label.cex=, label.color=, label.x=, label.y=, curved=, arrow.mode=, loop.angle=, loop.angle2=, layout=, margin=, palette=, rescale=, asp=, )

##Generates 1 random number between the values 5.0 and 7.5 
x1 <- runif(1, 5.0, 7.5)

library(UserNetR)
library(statnet)
library(igraph)
library(rgl)
library(igraphdata)

#### Different ways of plotting the data
#Plotting an Edge list with associated data and nodes with different collors
 V1 V2 Weight   
X C O 10
Y D P 11
Z E Q 12 
W J A 22
U T G 3

el<-read.table("networkcosta.txt", h=T)
el<-read.table("Insects.txt", h=T)

el[,1]=as.character(el[,1])        #Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems (see page on data import)
el[,2]=as.character(el[,2])
el=as.matrix(el)                   #igraph needs the edgelist to be in matrix format
g=graph.edgelist(el[,1:2])         #We first greate a network from the first two columns, which has the list of vertices
E(g)$weight=as.numeric(el[,3])     #We then add the edge weights to this network by assigning an edge attribute called 'weight'
V(g)$size=degree(g)*3              #Define the size of the node by the number of edges that it receives
plot(g,layout=layout.fruchterman.reingold,edge.width=E(g)$weight/2, edge.curved=FALSE)

plot(g, layout=layout.circle)


plot(g,layout=layout.kamada.kawai,edge.width=E(g)$weight/2, edge.curved=FALSE)

tkplot(g)

#Summary
summary(g)

#Calculates multiple measures from the network
degree(g)
closeness(g)
betweenness(g)
bonpow(g) #Power (Bonacih, 1987)
evcent(g) #eigenvector centrality
alpha.centrality(g)

#List of attributes with different colors
attributes<-read.table("attributes.txt", h=T)

Node.ID ID 
Carollia.perspicillata Bat
Carollia.subrufa Bat
Piper.aduncum Plant
Piper.reticulatum Plant 

V(g)$ID=as.character(attributes$ID[match(V(g)$name,attributes$Node.ID)]) # This code says to create a vertex attribute called "Sex" by extracting the value of the column "Sex" in the attributes file when the Bird ID number matches the vertex name.
V(g)$ID

V(g)$color=V(g)$ID #assign the "Sex" attribute as the vertex color
V(g)$color=gsub("Plant","green",V(g)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(g)$color=gsub("Bat","black",V(g)$color) #Bats will be black

V(g)$shape=V(g)$ID
V(g)$shape=gsub("Plant","square",V(g)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(g)$shape=gsub("Bat","circle",V(g)$shape) #Bats will be black

#Choose the shape of the vertices in the network
V(g)$shape <-"circle"

#Change the shape of specific vertices in the network
V(g)[c("Salicaceae")]$shape<-"rectangle"

V(g)$size <- 4*sqrt(graph.strength(g))

#PLot the network
plot.igraph(g, layout=layout.fruchterman.reingold, edge.width=E(g)$weight/2, edge.color="grey", vertex.size=5)
plot.igraph(g, layout=layout.kamada.kawai, edge.width=E(g)$weight/2, edge.color="grey", vertex.size=5)

plot.igraph(g, layout=layout.circle, edge.width=E(g)$weight/2, edge.color="grey", vertex.label=NA)

#How to re-scale the graph
l <- layout_with_fr(g)
l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
par(mfrow=c(2,2), mar=c(0,0,0,0)) 
plot(g, rescale=F, layout=l*0.4, vertex.size=5) 
plot(g, rescale=F, layout=l*0.6, vertex.size=5) 
plot(g, rescale=F, layout=l*0.8, vertex.size=5)
plot(g, rescale=F, layout=l*1.0, vertex.size=5)
plot(g, rescale=F, layout=l*2.0, vertex.size=5)

par(mfrow=c(1,1)) 

########## Network analysis - Random graphs
#Creating an Erdos-Renyi Random Graph MOdel
### For a large n, the network will have a POisson degree distribution
library(igraph)
g<-erdos.renyi.game(n=12, 10, type='gnm')
g
graph.density(g)
plot(g)

op<-par(mar=c(0,1,3,1), mfrow=c(1,2))
plot(erdos.renyi.game(n=12,10,type='gnm'), vertex.color=2, main="First random graph")
plot(erdos.renyi.game(n=12,10,type='gnm'), vertex.color=4, main="Second random graph")
par(op)

##Plots the degree distribution of the graph
#Random graphs are connected even with a low values of average degree #That means tha across the range of network sizes tupically seen in social network analysis, the average degree required to have a completely connected network will be less than aproximately 12.
#Another property of large networks is that the network will stay completely or almost completely connected for average degrees higher than 4 or 5
#Another property is that the connected random graphs are quite compact. Even for large networks
g<-erdos.renyi.game(n=1000,.005, type='gnp')
plot(degree.distribution(g))
plot(g)

##Demonstration about how increasing the average degree impacts on the connectance of the network
#For average degrees of 4 or more, the network is almost fully connected
crnd <- runif(500,1,8)
cmp_prp<-sapply(crnd,function(x) max(clusters(erdos.renyi.game(n=1000, p=x/999))$csize)/1000)
smoothingSpline<-smooth.spline(crnd, cmp_prp, spar=0.25)
plot(crnd, cmp_prp, col='grey60', xlab="Avg. Degree", ylab="Largest Component Proportion")
lines(smoothingSpline, lwd=1.5)

##Runs 250 simulations producing random graphs from 50 to 5000 nodes
n_vect<-rep(c(50,100,500,1000,5000), each=50)
g_diam<-sapply(n_vect, function(x) diameter(erdos.renyi.game(n=x, p=6/(x-1))))
library(lattice)
bwplot(g_diam ~ factor(n_vect), panel  = panel.violin, xlab="Network Size", ylab="Diameter")

##Small-world model
#The Erdos-Renyi random graph model has one major limitation in that it does not describe
#the properties of many real-world social networks. One type of model, called the small-world model 
# by Watts and Strotgatz (1998) produces random networks that are somewhat more realistic than 
#Erdos- Renyi 
#Small-world model starts with a circle of nodes, where each node is connected to its c immediate
#neighbors (forming a formal lattice structure). Then, a small number of edges are rewired, where hey are 
#removed and then replaced with another tie that connects two random nodes. If the rewiring probability
#is 0, then we end up with the original lattice network. When p is 1, then we have an Erdos-Renyi
#random graph. The main interesting discovery of Watts and Strotgatz (and others), is that only
#a small fraction of ties needs to be rewired to dramatically reduce the diameter of the network

#This functions calculates and produces different networks according to the Watts Strogatz model
#and different rewiring probabilities
g1<-watts.strogatz.game(dim=1, size=100, nei=2, p=0)
g2<-watts.strogatz.game(dim=1, size=100, nei=2, p=0.5)
g3<-watts.strogatz.game(dim=1, size=100, nei=2, p=0.20)
g4<-watts.strogatz.game(dim=1, size=100, nei=2, p=1)

op<-par(mar=c(2,1,3,1), mfrow=c(2,2))

plot(g1, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=0")))
plot(g2, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=0.05")))
plot(g3, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=0.20")))
plot(g4, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=1")))

#This simulation show how quickle the rewiring reduces the diameter of the networks
library(igraph)
g100 <-watts.strogatz.game(dim=1, size=100, nei=2, p=0)
g100
diameter(g100)
p_vect<-rep(1:30, each=10)
g_diam<-sapply(p_vect, function(x)
        diameter(watts.strogatz.game(dim=1, size=100,
                                     nei=2, p=x/200)))
smoothingSpline = smooth.spline(p_vect, g_diam, spar=0.35)
plot(jitter(p_vect,1), g_diam, col='grey60',
     xlab="Number of Rewired Edges",
     ylab="Diameter")
lines(smoothingSpline, lwd=1.5)



#PLots a distribution of the strength against the the degree distribution of the graph
degree(g)
strength(g)
plot(jitter(strength(g)), degree(g), col='grey60',
     xlab="Degree",
     ylab="Strength")
lines(smoothingSpline, lwd=1.5)






##Scale free models
#These are networks that follow a power law distribution. Most of it can be explained by a 
#preferential attachment. As network grows, new nodes are more likely to form ties with other 
#nodes that already have many ties. The "rich gets richer" phenomena.
#Preferantial attachment model of Barabasi and Albert 

g<-barabasi.game(500, directed=FALSE)
V(g)$color<-"lightblue"
V(g)[degree(g) >9]$color<-"red"
node_size<-rescale(node_char=degree(g), low=2,
                   high=8)
plot(g, vertex.label= NA, vertex.size=node_size)

median(degree(g))
mean(degree(g))
table(degree(g))

op<-par(mfrow=c(1,2))
plot(degree.distribution(g), xlab="Degree", ylab="Proportion")
plot(degree.distribution(g), log='xy', xlab="Degree", ylab="Proportion")

par(op)

#Shows how a network that functions according to the Barabasi game grows by adding nodes
g1<-barabasi.game(10,m=1,directed=FALSE)
g2<-barabasi.game(25,m=1,directed=FALSE)
g3<-barabasi.game(50,m=1,directed=FALSE)
g4<-barabasi.game(100,m=1,directed=FALSE)

op<-par(mfrow=c(2,2), mar=c(4,0,1,0))

plot(g1, vertex.label=NA, vertex.size=3, xlab="n=10")
plot(g2, vertex.label=NA, vertex.size=3, xlab="n=10")
plot(g3, vertex.label=NA, vertex.size=3, xlab="n=10")
plot(g4, vertex.label=NA, vertex.size=3, xlab="n=10")

#Comparing Random models to empirical networks
data(lhds)
ilhds<-asIgraph(lhds)
ilhds
graph.density(ilhds)
mean(degree(ilhds))
g_rnd<-erdos.renyi.game(1283, .0033, type='gnp')
g_smwrld<-watts.strogatz.game(dim=1, size=1283, nei=2, p=.25)
g_prfatt<-barabasi.game(1283, out.dist=c(.15,.6,.25), directed=FALSE, zero.appeal=2)


plot(g, layout=layout.circle, vertex.label=NA)

plot(g, layout=layout.circle)



legend(x, y = NULL, legend, fill = NULL, col = par("col"),
       border = "black", lty, lwd, pch,
       angle = 45, density = NULL, bty = "o", bg = par("bg"),
       box.lwd = par("lwd"), box.lty = par("lty"), box.col = par("fg"),
       pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
       xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1,
       adj = c(0, 0.5), text.width = NULL, text.col = par("col"),
       text.font = NULL, merge = do.lines && has.pch, trace = FALSE,
       plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
       inset = 0, xpd, title.col = text.col, title.adj = 0.5,
       seg.len = 2)

networkcosta

colrs <- c("black", "green") V(g)$color <- colrs[V(g)$ID.numbr]
colrs <- c("gray50", "tomato", "gold") V(g)$color <- colrs[V(g)$ID]



colrs <- c("gray50", "tomato", "gold") V(net)$color <- colrs[V(net)$media.type]

legend(x=-1.5, y=-1.1, V(g)$color=V(g)$ID, pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


#### Main types of plots
Layout = Fruchterman Reingold
Layout = Kamada Kawai
Layout = Circle
Layout = Reingold Tilford

#Makes a heatmap of the interactions
netm <- get.adjacency(g, attr="weight", sparse=F)
colnames(netm) <- V(g)$V1
rownames(netm) <- V(g)$V2

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,67:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )

plot(g, layout=layout.bipartite) 

#PLots the degree distribution
dd <- degree.distribution(g, cumulative=T, mode="all")
plot(dd, pch=19, cex=1, col="orange", xlab="Degree", ylab="Cumulative Frequency")


plot.igraph(g,vertex.label=V(g)$name,vertex.size=15, vertex.label.color=V(g)$color, 
            
V(g)$name,vertex.size=30,,vertex.label.color="yellow", vertex.label.font=2,vertex.color="darkblue",edge.color="black"

plot.igraph(g,vertex.label=V(g)$name,vertex.size=30,,vertex.label.color="yellow", vertex.label.font=2,vertex.color="darkblue",edge.color="black")

V(g)$size=degree(g)*2 #because 1 is a small size for a node, I'm just multiplying it by 5
plot.igraph(g,vertex.label=NA,layout=layout.fruchterman.reingold, edge.width=E(g)$weight)









#Data for the network
dfh<-data.frame(A=c("C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.perspicillata", "C.subrufa", "C.subrufa", "C.subrufa", "C.subrufa", "C.subrufa", 
"A.lituratus", "A.lituratus", "A.lituratus", "A.lituratus", "C.villosum", "C.villosum", "C.villosum", "C.villosum", "Glossophagasp", "Glossophagasp", "Glossophagasp", "Glossophagasp", "Glossophagasp", "A.jamaicensis", "A.jamaicensis", "A.jamaicensis", "A.jamaicensis", "A.jamaicensis", "A.jamaicensis", "A.jamaicensis", "Dermanurasp", "Dermanurasp", "Dermanurasp", "P.helleri", "A.toltecus", "M.microtis", "L.thomasi", "L.thomasi", "L.thomasi"), B = c("B. ungulata",
"P. multiplinervium", "Moraceae", "Moraceae", 
"Moraceae", "Moraceae", "Moraceae", "Moraceae", "Moraceae", "Moraceae", "Moraceae", "Moraceae", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.marginatum", "P.auritum", "P.tuberculatum", "Moraceae", "Moraceae", "Moraceae", "M.chicle", "Moraceae", "Moraceae", "Moraceae", "F.trigonata", "P.marginatum", "F.paraensis", "P.amalago", "P.amalago", "P.amalago", "Moraceae", 
"Moraceae", "Moraceae", "Moraceae", "M.chicle", "M.chicle", "F.trigonata", "Moraceae", "M.chicle", "M.chicle", "P.marginatum", "Moraceae", "Salicaceae", "P.amalago", "P.amalago", "P.amalago"))  

#Transforms the network into a graph
df.h<-graph.data.frame(d = dfh, directed = TRUE)

#Plots the network
plot(df.h)

#PLots a 3D version of the network
rglplot(df.h)


######## How to create a co-membership relation between individuals of the network
bm
Group1 Group2 Group3 Group4
A      1      1      0      0
B      1      0      1      0
C      1      0      1      0
D      0      1      0      1
E      0      0      1      1

#Converts the matrix into an igraph object
bg=graph.incidence(bm)

#Converts into a bipartite projection
pr=bipartite.projection(bg) 


get.adjacency(pr$proj1,sparse=FALSE,attr="weight")

#Plot the projection of the membership
plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="black",vertex.label=V(pr$proj1)$name)



############ Exploratory analysis

#Check to see if all nodes in the network are connected
is.connected (graph object)

#Is it strongly connected
is.connected(g, mode=c("strong"))

#Is it weakly connected
is.connected(g, mode=c("weak"))

#Average path length between two nodes
average.path.length(g)

#Network diameter
diameter(g)

#Transitivity
transitivity(g)

#Censuses of all components within a graph
comps<-decompose.graph(yeast)
table(sapply(comps, vcount))

comps<-decompose.graph(g)
table(sapply(comps, vcount))


#Visualizes the ditribution of vertex degree in the network
hist(degree(g), col="lightblue", xlim=c(0,50), xlab="Vertex Degree", ylab="Frequency", main="")

#Visualizes vertex strength  / Vertex strength is obtained by summing up the weigths of edges incident to a given vertex. The distribution of strength - sometimes called the weighted degree distribution - is defined in analogy to the ordinary degree distribution
hist(graph.strength(g), col="pink", xlab="Vertex Strength", ylab="Frequency", main="Vertex strength distribution")

#Counts the number of edges and vertices
library(igraphdata)
ecount(g)
vcount(g)

#Calculates the degree distribution of the network
d.df.h <- degree(g)
hist(d.df.h, col="blue", xlab="Degree", ylab="FRequency", main="Degree Distribution")

d.df.h <- degree(g)
hist(d.df.h, col="blue", xlab="Degree", ylab="FRequency", main="Degree Distribution")


#Calculates the logarithmical distribution of degree
dd.df.h <- degree.distribution(g)
d <- 1:max(d.df.h)-1
ind <- (dd.df.h !=0)
plot(d[ind], dd.df.h[ind], log="xy", col="blue", xlab=c("Log-Degree"), ylab=c("Log-Intensity"), main="Log-log Degree Distribution")


#Visualizes degree distribution of the vertices versus the degree distribution of the neighbour verticies
a.nn.deg.df.h <- graph.knn(df.h,V(df.h))$knn
plot(d.df.h, a.nn.deg.df.h, log="xy", col="goldenrod", xlab=c("Log Vertex Degree"), ylab=c("Log Average NEighbor Degree"))





###################################################################################################

par(mfrow=c(1,2))

plot(df.h, layout=layout.kamada.kawai, vertex.size=7,  edge.width=1)
#The algorithm of Kamada and Kawai [7], on the other hand, associates springs between all vertices, with the ideal length of a spring proportional to the graph distance of the vertices. In a force-directed algorithm, the energy of the system is typically minimized iteratively by moving the vertices along the direction of the force. This amount may be large initially, but reduces gradually based on a "cooling schedule."

plot(df.h, layout=layout.fruchterman.reingold) 
#The Fruchterman-Reingold Algorithm is a force-directed layout algorithm. The idea of a force directed layout algorithm is to consider a force between any two nodes. In this algorithm, the nodes are represented by steel rings and the edges are springs between them. The attractive force is analogous to the spring force and the repulsive force is analogous to the electrical force. The basic idea is to minimize the energy of the system by moving the nodes and changing the forces between them. For more details refer to the Force Directed algorithm. In this algorithm, the sum of the force vectors determines which direction a node should move. The step width, which is a constant determines how far a node moves in a single step. When the energy of the system is minimized, the nodes stop moving and the system reaches it's equilibrium state. The drawback of this is that if we define a constant step width, there is no guarantee that the system will reach equilibrium at all. T.M.J. Fruchterman and E.M. Reingold introduced a "global temperature" that controls the step width of node movements and the algorithm's termination. The step width is proportional to the temperature, so if the temperature is hot, the nodes move faster (i.e, a larger distance in each single step). This temperature is the same for all nodes, and cools down at each iteration. Once the nodes stop moving, the system terminates.


title("ITS2 Network - Guanacaste")
plot(df.h, layout=layout.circle)
title("ITS2 Network - Guanacaste")

articulation.points(df.h)

assortativity (df.h, types1, types2 = NULL, directed = TRUE)

bipartite.mapping(df.h)

plot(df.h)

library(sand)
data(karate)
nv<-vcount(df.h)
ne<-ecount(df.h)
degs<-degree(df.h)

ntrials <- 1000


## Looking for patterns of agregations in the community

library(sand) #I`m going to use in this example the matrix created in the interaction above
kc<-fastgreedy.community(g1) #cluster the community in a hierarchical agglomerative way

#The complete function is fastgreedy.community(graph, merges=TRUE, modularity=TRUE,
membership=TRUE, weights=E(g1)$weight)

length(kc)
sizes(kc)
par(mfrow=c(1, 2))
plot(g1, vertex.color=membership(kc))
plot(kc, g, vertex.size=5, vertex.label=NA)
library(ape)
dendPlot(kc, mode="phylo")

#Community structure detection based on edge betweenness
edge.betweenness.community (df.h, weights = E(df.h)$weight, directed = TRUE, edge.betweenness = TRUE, merges = TRUE, 
bridges = TRUE, modularity = TRUE, membership = TRUE)

#Calculates the alpha centrality of the graphic on a matrix
alpha.centrality(df.h, nodes=V(df.h), alpha=1, loops=FALSE,
exo=1, weights=NULL, tol=1e-7, sparse=TRUE)





 