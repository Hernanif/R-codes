##Network Analysis

rm(list = ls()) #clear workspace

#########Bipartite package
#general analysis of network
plotweb(flowers, method="normal", y.lim=c(-0.2,2.0), text.rot="90", labsize=1.5, col.low="black", col.high="black", col.interaction="grey")

#Slope of extinction
d<-second.extinct(flowers, participant = "higher", method = "abun", nrep = 10, details = FALSE, ext.row=NULL, ext.col=NULL)
slope.bipartite(d, plot.it = TRUE)

#To get specific values from the command newtworklevel
networklevel(web, "generality") #the name of the measure that you want


#####################Plotting bipartite networks in Igraph

# Read data matrices.
# Read a network
# Creating the objects. Example input from the clipboard. 

mymat <- read.delim("data.txt", row.names=1)

# Where data.txt has a weighted adjacency matrix, e.g.,:

	Aa	Ab	Ac	Ba	Bb	Bc	Bd	Ca	Cb	Cc	Da
P1	139	112	 9	73	4	14	20	14	5	2	1
P2	184	26	6	19	31	21	17	11	7	5	0
P3	281	74	33	36	18	29	5	9	5	3	0
P4	87	40	38	21	13	12	9	1	1	0	0
P5	100	93	27	12	8	11	32	0	0	0	0
P6	51	25	13	17	9	1	0	0	0	0	0
P7	42	26	16	0	2	0	0	0	0	0	0
P8	31	18	14	15	12	9	7	1	0	0	0
P9	46	12	6	0	3	0	0	0	0	0	0
P10	136	0	0	12	0	2	2	0	0	0	0
P11	130	8	0	5	0	0	0	0	0	0	0

# Use this to copy from the clipboard, after select/copy the above block.

mymat <- read.table(pipe("pbpaste"), header=T, sep= "\t",row.names=1)


#############Igraph package

#Creates an igraph object
library(igraph)
ug4 <- graph.formula(a -- b:c, c--b:d, e -- a:d:t) #the first letter introduces the vertex and the next ones after the -- defines to which other nodes that node is connected
plot(ug4)
ug4


#Another way of creating the same graph
ug4.2 <- graph.empty(n=5, directed=FALSE)
V(ug4.2)$name <- V(ug4.2)$label <- letters[1:5]
ug4.2 <- add.edges(ug4.2, c(0,1, 0,2, 0,4, 1,2, 2,3, 3,4))
ug4.2


#Importing an adjacency matrix into a usable format in either statnet or igraph using these sample codes 
(note, this is for an unweighted matrix):

library(igraph) # This loads the igraph package
dat=read.csv(file.choose(),header=TRUE,row.names=1,check.names=FALSE) # choose an adjacency matrix from a .csv file
m=as.matrix(dat) # coerces the data set as a matrix
g=graph.adjacency(m,mode="undirected",weighted=NULL) # this will create an 'igraph object'
g 


#Importing an edge list 
library(igraph)
dat=read.csv(file.choose(),header=TRUE) # choose an edgelist in .csv file format
el=as.matrix(dat) # coerces the data into a two-column matrix format that igraph likes
el[,1]=as.character(el[,1])
el[,2]=as.character(el[,2])
g=graph.edgelist(el,directed=FALSE) # turns the edgelist into a 'graph object'

#A much easier way to import an edge list in igraph is to use 
dat=read.csv(file.choose(),header=TRUE) # choose an edgelist in .csv file format
g=graph.data.frame(dat,directed=FALSE)



##Plotting a network with the weigths #the values are the weights of the network and when it`s zero means that there are no connection
m <- read.table(row.names=1, header=TRUE, text=
"           A          B          C          D           E         F
A 0.00000000  0.0000000  0.0000000  0.0000000  0.05119703 1.3431599
B 0.00000000  0.0000000 -0.6088082  0.4016954  0.00000000 0.6132168
C 0.00000000 -0.6088082  0.0000000  0.0000000 -0.63295415 0.0000000
D 0.00000000  0.4016954  0.0000000  0.0000000 -0.29831267 0.0000000
E 0.05119703  0.0000000 -0.6329541 -0.2983127  0.00000000 0.1562458
F 1.34315990  0.6132168  0.0000000  0.0000000  0.15624584 0.0000000")
m <- as.matrix(m)
library(igraph)
ig <- graph.adjacency(m, mode="undirected", weighted=TRUE)
plot(ig, layout= layout.kamada.kawai, edge.label=round(E(ig)$weight, 3)) #the last number represents the number of decimal places after the comma
qgraph(m,edge.labels=TRUE)


plot.igraph(ig, vertex.shape= "circle")
vertex.shape= “circle”, “square”, “csquare”, “rectangle”, “crectangle”, “vrectangle”, “pie” 


#Get edges from an Igraph
get.data.frame(g, what="edges")

#Get vertices from an Igraph
get.data.frame(g, what="vertices")


######Ploting a graph in Igraph
library(sand)
fblog.c<-contract.vertices(fblog, party.nums)
l<-layout.drl(fblog) ##dlr is an algorithm designed for the visualization of big networks
E(fblog.c)$weight<-l
fblog.c<-simplify(fblog.c)
plot(fblog, layout=layout.kamada.kawai(fblog), vertex.label=V(fblog)$PolParty, vertex.color=as.numeric(party.nums.f), vertex.label.dist=1.5, edge.width=sqrt(E(fblog.c)$weight), vertex.size=3)

### fblog= igraph data 
### layout= layout format of the graphic
### vertex.label= labels of the vertices
### vertex.color= color of the vertices
### vertex.size= size of the vertices
### vertex.label.dist= distance between the vertex and the label
### edge.width= width of the lines connecting the vertices

#E(fblog.c)$weight) => plot the edges based on the weight of the interaction ###Very very useful!!!!
#vertex.label=V(net)$name => plot labels based on the names of each vertex


plot(fblog, layout=l, vertex.size=5, vertex.label=3, vertex.color=party.nums, edge.arrow.size=1000)

d<-graph.adjacency(fruits)

fblog.c<-contract.vertices(fblog, party.nums)
E(fblog.c)$weight<-1
fblog.c<-simplify(fblog.c)
party.size<-as.vector(table(V(fblog)$PolParty))
plot(fblog.c, vertex.size=5*sqrt(party.size), vertex.label=party.names, vertex.label= party.names, vertex.color=V(fblog.c), edge.width=sqrt(E(fblog.c)$weight), vertex.label.dist=1.5, edge.arrow.size=0)


### Using Igraph, Qgraph plot
str(object) #gives the vertex of the objects

title("title") # puts the title in the graphic
plot(data, layout=layout.circle) #plots the data in a circle layout
plot(data, layout=layout.fruchterman.reingold) 

## Description of how the algorithm is made
#The Fruchterman-Reingold Algorithm is a force-directed layout algorithm. The idea of a force directed layout algorithm is to consider a force between any two nodes. In this algorithm, the nodes are represented by steel rings and the edges are springs between them. The attractive force is analogous to the spring force and the repulsive force is analogous to the electrical force. The basic idea is to minimize the energy of the system by moving the nodes and changing the forces between them. For more details refer to the Force Directed algorithm.
In this algorithm, the sum of the force vectors determines which direction a node should move. The step width, which is a constant determines how far a node moves in a single step. When the energy of the system is minimized, the nodes stop moving and the system reaches it's equilibrium state. The drawback of this is that if we define a constant step width, there is no guarantee that the system will reach equilibrium at all. T.M.J. Fruchterman and E.M. Reingold introduced a "global temperature" that controls the step width of node movements and the algorithm's termination. The step width is proportional to the temperature, so if the temperature is hot, the nodes move faster (i.e, a larger distance in each single step). This temperature is the same for all nodes, and cools down at each iteration. Once the nodes stop moving, the system terminates.

plot(data, layout=layout.kamada.kawai)

##Description of how the algorithm is made
#The algorithm of Kamada and Kawai [7], on the other hand, associates springs between all vertices, with the ideal length of a spring proportional to the graph distance of the vertices. In a force-directed algorithm, the energy of the system is typically minimized iteratively by moving the vertices along the direction of the force. This amount may be large initially, but reduces gradually based on a "cooling schedule."

plot(data, layout=layout.reingold.tilford(data, circular=T))

plot(data, layout=layout.reingold.tilford)

#Plot bipartite network
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1], vertex.size=30, vertex.shape=ifelse(V(g.bip)$type, "rectangle", "circle"), vertex.color=ifelse(V(g.bip)$type, "red", "cyan"))

 
#############Decorating Graph Layouts

tkplot(igraph) #allows the manual edition of an igraph

tkplot.getcoords(igraph) #gets coordinates of a graph
plot(ug4, layout=xy)
layout.spring(ug4)


#######Example
library(igraphdata)
data(karate)
#Reproducible layout
set.seed(42)
l<-layout.kamada.kawai(karate)

#Plot undecorated first
igraph.options(vertex.size=10)
par(mfrow=c(1,1))
plot(karate, layout=l, vertex.label=V(karate))

#Now decorate, starting with labels
V(karate)$label <- sub("Actor", "", V(karate)$name)

#Two leaders get shapes different from club members
V(karate)$shape<-"circle"
V(karate)[c("Mr Hi", "John A")]$shape<-"rectangle"

#Differantiate two factions by color
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"

#Vertex area proportional to vertex strength
#(i.e., total weight of incident edges)
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2<-V(karate)$size* .5

#Weight edges by number of common activities
E(karate)$width <- E(karate)$weight

#Color edges by within/between faction
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[F1 %--% F1]$color <- "pink"
E(karate) [F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"

#Offset vertex labels for smaller points (default=0)
V(karate)$label.dist <-ifelse(V(karate)$size >= 10, 0, 0.75)

#Plot decorated graph, using same layout
plot(karate, layout=l)



library(sand)
data(lazega)

#Office location indicated by color
colbar<-c("red", "dodgerblue", "goldenrod")
v.colors<-colbar[V(lazega)$Office]

#Type of practice indicated by vertex shape
v.shapes<-c("circle", "square")[V(lazega)$Practice]

#Vertex size proportional to years with firm
v.size <-3.5*sqrt(V(lazega)$Years)

#Label vertices according to seniority
v.label<-V(lazega)$Seniority

#Reproducible layout
set.seed(42)
l <- layout.fruchterman.reingold(lazega)
plot(lazega, layout=l, vertex.color=v.colors, vertex.shape=v.shapes, vertex.size=v.size, vertex.label=v.label)



#Algorithms
Kamada-Kawai
Input matrix: all-pairs-shortest-path
Force model: springs between all pairs which relax to edge length
Optimization: each node has an "energy" according to "spring tension", node with highest energy is moved to optimal position using a Newton-Raphson steepest descent. Energy of network is minimized.

Fruchtermen-Riengold
Input matrix: raw distance matrix
Force model: electrostatic repulsion between all, attraction to connected nodes, force minima is at desired edge length
Optimization: reposition nodes according to the force vector they "feel", the distance nodes are allowed to move is gradually decreased until graph settles.  

Moody's Peer-Influence
Input matrix: raw similarity matrix
Model: nodes are repositioned to the weighted average of their peers' coordinates
Optimization: repeated iteration

MDS (metric)
Input matrix: all-pairs-shortest-path matrix or alternate measure of distances/similarities between nodes.
Model: 2D projection of high-dimensional space of the network using matrix algebra (generally  SVD) to determine Eigenvectors or principal components which will display a large amount of variance.
Optimization: exact solution

MDS (non-metric)
Input matrix:  all-pairs-shortest-path matrix  or alternate measure of distances /similarities between nodes
Model: search for a low-stress projection from 2D projection of high-dimensional space of the network 
Optimization:  there are many different techniques, I don't know enough about them yet.


###Creating graphs
df <- data.frame(A = c("Berlin", "Amsterdam", "New York") , B = c("Munich", "Utrecht", "Chicago"))
df.g <- graph.data.frame(d = df, directed = FALSE)
plot(df.g, vertex.label = V(df.g)$name)


## Ploting a matrix of bat interactions
library(igraph)
df<-data.frame(A = c("A. lituratus", "A. jamaicensis", "A. caudifer", "A. caudifer", "A. caudifer", "A. caudifer", "A. caudifer", "G. soricina", "G. soricina", "G. soricina", "G. soricina", "G. soricina", "G. soricina", "G. soricina", "G. soricina", "L. mordax", "A. geoffroyi", "A. geoffroyi", "P. discolor", "P. discolor", "P. discolor", "P. discolor", "P. discolor", "P. lineatus", "P. lineatus", "P. lineatus", "P. lineatus", "P. hastatus", "P. hastatus", "C. perspicillata",  "C. perspicillata", "C. perspicillata", "S. lilium", "C. doriae", "A. planirostris",  "A. planirostris",  "A. planirostris", "L. thomasi"), B = c("M. fistulifera", "L. pacari", "V. longicaudis", "S. sulfureus", "P. prasinata", "A. rufinerve", "P. mucronata", "C. brasiliensis", "C. villosum", "M. fistulifera", "P. prasinata", "P. corynocephalus", "V. chamissonis", "P. galbana", "P. mucronata", "C. jacobinae", "C. brasiliensis", "L. pacari", "C. brasiliensis", "C. coriaceum", "C. villosum", "M. fistulifera", "P. corynocephalus", "C. brasiliensis", "M. fistulifera", "P. corynocephalus", "L. pacari", "M. fistulifera", "P. corynocephalus", "C. brasiliensis", "P. corynocephalus", "P. mucronata", "C. brasiliensis", "M. fistulifera", "C. brasiliensis", "L. pacari", "V. chamissonis", "C. villosum")) 
df.g<-graph.data.frame(d = df, directed = FALSE)
par(mfrow=c(1,3))
plot(df.g, layout=layout.kamada.kawai, vertex.size=1,  edge.width=1)
title("Bat-plants pollinated interactions")
plot(df.g, layout=layout.circle)
title("Bat-plants pollinated interactions")


## Looking for patterns of agregations in the community

library(sand) #I`m going to use in this example the matrix created in the interaction above
kc<-fastgreedy.community(df.g) #cluster the community in a hierarchical agglomerative way

#The complete function is fastgreedy.community(graph, merges=TRUE, modularity=TRUE,
membership=TRUE, weights=E(graph)$weight)

length(kc)
sizes(kc)
par(mfrow=c(1, 2))
plot(df.g, vertex.color=membership(kc))
plot(kc, df.g)
library(ape)
dendPlot(kc, mode="phylo")

#Community structure detection based on edge betweenness
edge.betweenness.community (df.g, weights = E(df.g)$weight, directed = TRUE, edge.betweenness = TRUE, merges = TRUE, 
bridges = TRUE, modularity = TRUE, membership = TRUE)

#Calculates the alpha centrality of the graphic on a matrix
alpha.centrality(df.g, nodes=V(df.g), alpha=1, loops=FALSE,
exo=1, weights=NULL, tol=1e-7, sparse=TRUE)

articulation.points(df.g)

assortativity (df.g, types1, types2 = NULL, directed = TRUE)

bipartite.mapping(df.g)


plot(df.g)


## Testing to see if this pattern is different from one encountered for 
a randomly generated network created with the same data 

library(sand)
data(karate)
nv<-vcount(df.g)
ne<-ecount(df.g)
degs<-degree(df.g)

ntrials <- 1000

#Generate classical random graphs of this same order and size and, 
for each one, we use the same community detection algorithm to determine the
number of communities

num.comm.rg<-numeric(ntrials)
for(i in (1:ntrials)){
   g.rg<-erdos.renyi.game(nv, ne, type="gnm")
   c.rg<-fastgreedy.community(g.rg)
   num.comm.rg[i]<-length(c.rg)
}


#The results may be summarized and compared using side by side bar plots. 
This is used to test if the number of communities found in the network is 
different from the number of communities found in a random network created.
Fixed size means that the communities created have the same size of the random
networks that it`s creating to try to detect the number of communities. Fixed degree
sequence means that the networks created have the same degree sequence as the
random networks created.   

rslts<-c(num.comm.rg,num.comm.grg)
indx<-c(rep(0, ntrials), rep(1, ntrials))
counts<-table(indx, rslts)/ntrials
par(mfrow=c(1, 2))
barplot(counts, beside=TRUE, col=c("blue", "red"), xlab="Number of Communities", ylab= "Relative Frequency", legend=c("Fixed Size", "Fixed Degree Sequence"))
plot(kc, df.g)


##### Calculating and comparing the clustering of a network with a randomly crated
network 

#Calculation made by the following function
library(igraphdata)
data(macaque)
summary(macaque)
clust.coef.dir<-function(graph) {
A<-as.matrix(get.adjacency(graph))
S<-A+t(A)
deg<-degree(graph, mode=c("total"))
num<-diag(S%*% S %*% S)
denom<-diag(A %*% A)
denom<-2*(deg*(deg - 1) - 2* denom)
cl<-mean(num/denom)
return(cl)
}
#Simulating draws 
ntrials<-1000
nv<-vcount(macaque)
ne<-ecount(macaque)
cl.rg<-numeric(ntrials)
apl.rg<-numeric(ntrials)
for (i in (1:ntrials)) {
g.rg<-erdos.renyi.game(nv, ne, type="gnm", directed=TRUE)
cl.rg[i] <- clust.coef.dir(g.rg)
apl.rg[i]<-average.path.length(g.rg)
}

#Summaryzing the results for the distribution of the clustering coefficient 
summary(cl.rg)

#Summaryzing the results for the average path length
summary(apl.rg)

#Comparing these distributions against the values for the macaque network 
#By comparing these results with the distribution of the clustering in the random
created networks we can see the pattern of clustering
 
clust.coef.dir(macaque)

average.path.length(macaque)


#Similarly, we do the same using generalized random graphs constrained to have
the required degree sequence

num.comm.grg<-numeric(ntrials)
for(i in (1:ntrials)){
    g.rg<-erdos.renyi.game(nv, ne, type="gnm")
    c.rg<-fastgreedy.community(g.rg)
    num.comm.rg[i]<-length(c.rg)
}


#### Different functions for looking into agregations patterns in communities
#edge.betweenness.community 
Is a hierarchical decomposition process where edges are removed 
in the decreasing order of their edge betweenness scores 
(i.e. the number of shortest paths that pass through a given edge). 
This is motivated by the fact that edges connecting different groups 
are more likely to be contained in multiple shortest paths simply 
because in many cases they are the only option to go 
from one group to another. This method yields good results but is 
very slow because of the computational complexity of edge betweenness calculations and because the betweenness scores have to be re-calculated after every edge removal. Your graphs with ~700 vertices and ~3500 edges are around the upper size limit of graphs that are feasible to be analyzed with this approach. Another disadvantage is that edge.betweenness.community builds a full dendrogram and does not give you any guidance about where to cut the dendrogram to obtain the final groups, so you'll have to use some other measure to decide that (e.g., the modularity score of the partitions at each level of the dendrogram).

#fastgreedy.community 
Is another hierarchical approach, but it is bottom-up 
instead of top-down. It tries to optimize a quality function called 
modularity in a greedy manner. Initially, every vertex belongs to a 
separate community, and communities are merged iteratively such that 
each merge is locally optimal (i.e. yields the largest increase in the 
current value of modularity). The algorithm stops when it is not possible 
to increase the modularity any more, so it gives you a grouping as well 
as a dendrogram. The method is fast and it is the method that is usually 
tried as a first approximation because it has no parameters to tune. 
However, it is known to suffer from a resolution limit, i.e. communities 
below a given size threshold (depending on the number of nodes and edges 
if I remember correctly) will always be merged with neighboring communities.

#walktrap.community 
is an approach based on random walks. The general idea is that if you 
perform random walks on the graph, then the walks are more likely to stay 
within the same community because there are only a few edges that lead 
outside a given community. Walktrap runs short random walks of 3-4-5 steps 
(depending on one of its parameters) and uses the results of these random 
walks to merge separate communities in a bottom-up manner like 
fastgreedy.community. Again, you can use the modularity score to select 
where to cut the dendrogram. It is a bit slower than the fast greedy approach 
but also a bit more accurate (according to the original publication).

#spinglass.community 
is an approach from statistical physics, based on the so-called Potts model. 
In this model, each particle (i.e. vertex) can be in one of c spin states, 
and the interactions between the particles (i.e. the edges of the graph) 
specify which pairs of vertices would prefer to stay in the same spin state 
and which ones prefer to have different spin states. The model is then 
simulated for a given number of steps, and the spin states of the particles 
in the end define the communities. The consequences are as follows: 1) There 
will never be more than c communities in the end, although you can set c to 
as high as 200, which is likely to be enough for your purposes. 2) There may 
be less than c communities in the end as some of the spin states may become 
empty. 3) It is not guaranteed that nodes in completely remote (or disconencted)
parts of the networks have different spin states. This is more likely to be 
a problem for disconnected graphs only, so I would not worry about that. 
The method is not particularly fast and not deterministic (because of the 
simulation itself), but has a tunable resolution parameter that determines 
the cluster sizes. A variant of the spinglass method can also take into 
account negative links (i.e. links whose endpoints prefer to be in different 
communities).

#leading.eigenvector.community 
is a top-down hierarchical approach that optimizes the modularity function 
again. In each step, the graph is split into two parts in a way that the 
separation itself yields a significant increase in the modularity. The split 
is determined by evaluating the leading eigenvector of the so-called 
modularity matrix, and there is also a stopping condition which prevents 
tightly connected groups to be split further. Due to the eigenvector 
calculations involved, it might not work on degenerate graphs where the 
ARPACK eigenvector solver is unstable. On non-degenerate graphs, it is 
likely to yield a higher modularity score than the fast greedy method, 
although it is a bit slower.

#label.propagation.community 
is a simple approach in which every node is assigned one of k labels. 
The method then proceeds iteratively and re-assigns labels to nodes in a 
way that each node takes the most frequent label of its neighbors in a 
synchronous manner. The method stops when the label of each node is one of 
the most frequent labels in its neighborhood. It is very fast but yields 
different results based on the initial configuration (which is decided 
randomly), therefore one should run the method a large number of times 
(say, 1000 times for a graph) and then build a consensus labeling, which 
could be tedious.


###############################################################################
##Exponential Random Graph Models

#Separating the network into adjacency matrix and attributes
library(sand)
data(lazega)
A<-get.adjacency(lazega)
v.attrs<-get.data.frame(lazega, what="vertices")

#Creating an analogous network object for ergm
library(ergm)
library(mixer)
lazega.s<-network::as.network(as.matrix(A), directed=FALSE)
network::set.vertex.attribute(lazega.s, "Office", v.attrs$Office)
network::set.vertex.attribute(lazega.s, "Practice", v.attrs$Practice)
network::set.vertex.attribute(lazega.s, "Gender", v.attrs$Gender)
network::set.vertex.attribute(lazega.s, "Seniority", v.attrs$Seniority)

#Specifying a model #The strength of ERGMs lies in our ability to specify 
decidedly more nuanced models than that below. Doing so properly and effectively,
however, requires some thought and care.

my.ergm.bern <- formula(df ~ edges)
my.ergm.bern
lazega.s~edges
summary.statistics(my.ergm.bern)
edges

my.ergm <- formula(df ~ edges + kstar(2) + kstar(3) + triangle)
summary.statistics(my.ergm)

my.ergm<-formula(lazega.s ~ edges + gwesp(1, fixed=TRUE))
summary.statistics(my.ergm)

lazega.ergm <- formula(lazega.s ~ edges + gwesp(log(3), fixed=TRUE) + nodemain("Seniority") + nodemain ("Practice") + match ("Practice") + match("Gender") + match("Office")) 
 # This specification allow us to control for the density of the network and some effects of transitivity. In addition, it allows us to asses th effect on the formation of collaboratives ties among lawyers that is had by seniority, the type of practice (i.e., corporate or ligation), and commonality of practice, gender, and office location.

###Model fitting
#In standard settings, with independent and identically distributed realizations, exponential family models like that in (6.1) are generally fit using the method of maximum likelihood. In the context of the ERGMs in (6.2), the maximum likelihood estimators (MLEs) Oh of the parameters Oh are well defined, assuming and appropriatelyt-specific model, but their calculation is non-trivial.
#In ergm, models are fit using the function ergm, which implements a version 
#of Markov chain Monte Carlo maximum likelihood estimation, deriving from the 
#fundamental work of Geyer and Thompson [62]. The model in (6.13), for example, 
#is fit as: 

set.seed(42)
lazega.ergm.fit <- ergm(lazega.ergm)

anova.ergm(lazega.ergm.fit)

summary.ergm(lazega.ergm.fit)


##Goodness-of-fit
plot(fblog.sbm, classes=as.factor(V(fblog)$PolParty))


#To assess the godness-of-fit of our model in (6.13), as fit by ergm, 
#the funtcion gfhof in ergm runs the necessary MOnte Carlo simulation and 
#calculates comparisons with the original network graph in terms of the 
#distribution of degree, geodesic length, and edge-wise shared partners 
#(i.e., the numbver of neighbors shared by a pair of vertices defining an edge)

gof.lazega.ergm <- gof(lazega.ergm.fit)

par(mfrow=c(1, 3))
plot(gof.lazega.ergm)

##Network block models
#The structure of an ERGM closely parallels that of a standard regression model in statistics.
The presence or absence of network edges (i.e., the Yij) is taken to be the response
variable, while the role of the predictor variables is played by some combination
of network summary statistics (i.e., endogenous variables) and functions of vertex
and edge attirbutes (i.e., incorporating exogenous effects). In this section, we examine
the class of network block models, which are instead analogous to classical mixture models.

** This seems a very important function for the PhD as it calculates network position

##Model specification

##Model fiting

library(igraph)
library(mixer)
setSeed(42)
fblog.sbm<-mixer(as.matrix(get.adjacency(fblog)), qmin=2, qmax=15)

fblog.sbm.output <- getModel(fblog.sbm)
names(fblog.sbm.output)

fblog.sbm.output$q

fblog.sbm.output$alphas

fblog.sbm.output$Taus[, 1:3]

my.ent <- function(x) {-sum(x*log(x, 2))}
apply(fblog.sbm.output$Taus[, 1:3], 2, my.ent)

log(fblog.sbm.output$q, 2)

summary(apply(fblog.sbm.output$Taus, 2, my.ent))

##Goodness-of-fit
plot(fblog.sbm, class=as.factor(V(fblog)$PolParty))

####Latent Network Models
# From the perspective of statistical modeling, one key innovation 

summary(lazega)
library(eigenmodel)
set.seed(42)
A<-get.adjacency(lazega, sparse=FALSE)
lazega.leig.fitl<-eigenmodel_mcmc(A, R=2, S=11000, burn=10000)
same.prac.op<- v.attr.lazega$Practice %0% v.attr.lazega$Practice





###################################################################################

## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David", "Esmeralda"), age=c(48,33,45,34,21),  gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David", "David", "Esmeralda"), to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"), same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),  friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph.data.frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)
plot(g, layout=layout.kamada.kawai)
par(mfrow=c(1,2))

###Vertex and edge characteristics
#Vertex degree and vertex strength
par(mfrow=c(1,2))
hist(degree(igraph), col="lightblue", xlim=c(0, 50), xlab="Vertex Degree", ylab="Frequency", main="Vertex degree")
hist(graph.strength(igraph), col="pink", xlab="Vertex Strength", ylab="Frequency", main="Vertex Strength")
table(sapply(cliques(igraph), length)) ## 1=> number of nodes (cliques of one seize); 2=> number of edges (cliques of size two); 3=> number of triangles (cliques of size three)
table(sapply(maximal.cliques(igraph), length)) ##number of maximum cliques
clique.number(igraph) #defines the number of cliques 
cores<-graph.coreness(igraph) #show the number of cores
transitivity(igraph) #gives a measure of the clustering of the network

#Calculates vertex cuts (pag.58)
articulation.points(igraph)
igraph.cut.vertices<-articulation.points(igraph)
length(igraph.cut.vertices)

#Tests the level of connectivity between edges of a graph
is.connected(d, c("strong"))
is.connected(d, c("weak"))


library(sand)
data(karate)
kc<-fastgreedy.community(karate) #cluster the community in a hierarchical agglomerative way
length(kc)
sizes(kc)
par(mfrow=c(1, 2))
plot(kc, karate)
library(ape)
dendPlot(kc, mode="phylo")


#Produces a graph partitioning to see how the network is divided (page 63)
library(sand)
data(katare)
k.lap<-graph.laplacian(karate)
eig.anal<-eigen(k.lap)
plot(eig.anal$values, col="blue", ylab="Eigenvalues of Graph Laplacian")
f.vec<-eig.anal$vectors[, 33]
faction<-get.vertex.attribute(karate, "Faction")
f.colors<-as.character(length(faction))
f.colors[faction == 1] <- "red"
f.colors[faction == 2] <- "cyan"
plot(f.vec, pch=16, xlab="Actor Number", ylab= "Fiedler Vector Entry", col=f.colors)
abline(0, 0, lwd=2, col= "lightray")

#Validation of Graph Partitioning (page 64)
func.class<-get.vertex.attribute(yeast.gc, "Class")
table(func.class)
func.class

library(sand)
set.seed(42)
g.er<-erdos.renyi.game(100, 0.02)
par(mfrow=c(1, 2))
plot(g.er, layout=layout.circle, vertex.label=NA)
is.connected(g.er)
table(sapply(decompose.graph(g.er), vcount))
mean(degree(g.er))
hist(degree(g.er), col="lightblue", xlab="Degree", ylab="Frequency", main="")
average.path.length(g.er)
diameter(g.er)
transitivity(g.er)

#Generalized Random Graph Models
degs<-c(2,2,2,2,3,3,3,3)
g1<-degree.sequence.game(degs, method="vl")
g2<-degree.sequence.game(degs, method="vl")
par(mfrow=c(1, 2))
plot(g1, vertex.label=NA)
plot(g2, vertex.label=NA)
graph.isomorphic(g1, g2) #see if graphs are isomorphic
c(ecount(g1), ecount(g2))

#Generates a graph with the same degree sequence as our netowrk of protein-protein interaction in yeast
data(yeast)
degs <- degree(yeast)
fake.yeast<-degree.sequence.game(degs, method=c("vl"))
all(degree(yeast) == degree(fake.yeast))

#Generates a small world network graph
g.ws<-watts.strogatz.game(1, 25, 5, 0.05)
plot(g.ws, layout=layout.circle, vertex.label= NA)
g.lat100<-watts.strogatz.game(1, 100, 5, 0)
transitivity(g.lat100)
diameter(g.lat100)
average.path.length(g.lat100)
g.ws100<-watts.strogatz.game(1, 100, 5, 0.05)
diameter(g.ws100)
average.path.length(g.ws100)
transitivity(g.ws100)

##Link prediction
library(sand)
nv<-vcount(df.g)
ncn<-numeric()
A<-get.adjacency(df.g)
for(i in (1:(nv-1))){
ni<-neighborhood(fblog, 1, i)
nj<-neighborhood(fblog, 1, (i+1) :nv)
nbhd.ij<-mapply(intersect, ni, nj, SIMPLIFY=FALSE)
temp<-unlist(lapply(nbhd.ij, length)) - 2*A[i, (i+1) :nv]
ncn<-c(ncn, temp)
}

library(vioplot)
Avec<-A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1],
names=c("No Edge", "Edge"))
title(ylab="Number of COmmon Neighbors")

library (ROCR)
pred <- prediction(ncn, Avec)
perf<-performance(pred, "auc")
slot(perf, "y.values")
  
##Association Network Inference
rm(list=ls())
data(Ecoli.data)
ls()
heatmap(scale(Ecoli.expr), Rowv=NA)

library(igraph)
g.regDB<-graph.adjacency(regDB.adj, "undirected")
summary(g.regDB)
plot(g.regDB, vertex.size=3, vertex.label=NA)

mycorr<-cor(Ecoli.expr)

##############################################################
#######MOdeling and prediction for processes on Network Graphs

##Nearest Neighbor Methods

set.seed(42)
library(sand)
library(igraphdata)
data(ppi.CC)
summary(ppi.CC)
plot(ppi.CC, layout=layout.circle)
V(ppi.CC)$ICSC[1:10]
V(ppi.CC)[ICSC == 1]$color <- "yellow"
V(ppi.CC)[ICSC == 0]$color <- "blue"
plot(ppi.CC, vertex.size=5, vertex.label=NA)

clu <- clusters(ppi.CC)
ppi.CC.gc <- induced.subgraph(ppi.CC, clu$membership==which.max(clu$csize))
nn.ave <- sapply (V(ppi.CC.gc), function(x) mean(V(ppi.CC.gc)[nei(x)]$ICSC))

#Ploting histograms of the resulting values, separated according to the status of the vertex definig each neighborhood, i.e., according to the status of the `ego` vertex, in the terminology of social networks
par(mfrow=c(2,1))
hist(nn.ave[V(ppi.CC.gc)$ICSC == 1], col="yellow", ylim=c(0, 30), xlab="Proportion Neighbors w/ ICSC", main="Egos w/ ICSC")
hist(nn.ave[V(ppi.CC.gc)$ICSC == 0], col="blue", ylim=c(0, 30), xlab="Proportion Neighbors w/ ICSC", main= "Egos w/out ICSC") 













####Food web plotting function
### Author: Edmund Hart (edmund.m.hart@gmail.com)
### Description: A function to create grahps of trophic networks using ggplot2
### Plots food webs in a circular graph

library(ggplot2)
 
########## support function create.xy
########## returns regularly spaced circular coordinates for the size 
########## of your web
 
create.xy <- function(po){
  degs <- seq(0,2*pi,by=(2*pi/(po)))
	return(cbind(cos(degs),sin(degs)))
}
 

############Plotting function##############
# plot.webgg() requires two arguments:
# ARGUMENTS: web - first is a square S x S matrix of 0's and 1's where S is the total
# number of species in the web
# columns are consumers and rows are species that are consumed
# therefore a 1 in row 5 and column 8 means that species 8 eats species 5
#  Use the function t() if your matrices are oppositely arranged
#labels - is a vector of length S containing string values for the color of each consumer
# species link.
#
# RETURNED VALUES:  The function creates a list with two values.  The first is
# a ggplot2 object accessed as object$plot  
# The next is a dataframe of raw x-y coordinates to connect accessed as object$rawdat
plot.webgg <- function(web,labels){
  	xy <- create.xy(dim(web)[1])
		xy <-data.frame(xy)
		cons <- vector()
		my.plot<- ggplot(xy,aes(x=X1,y=X2))+geom_point()+xlab("")+ylab("")
  	###Suppress warnings
    options(warn=-1)
    ####Create output frame
    p.df <- data.frame(matrix(NA,ncol=4,nrow=0))
		
    ###Create label index
    label.in <- vector()
    
    for(i in 1:dim(web)[1]){
			
      #select the consumer links
			cons <- which(web[,i]==1)
      #####Now I need to have an if statement if there are no links
  if(length(cons)>0){
      #Now create repeat the one set of points that is being drawn
      #ordering with odd numbers
      tmp1 <-data.frame(cbind(rep(xy[i,1],length(cons)),rep(xy[i,2],length(cons)),seq(1,2*length(cons),by=2)))
      #Next select the nodes to connect to
      tmp2 <- data.frame(cbind(xy[cons,]))
      #add an index of sequenced even numbers 
      tmp2$X3 <- seq(2,2*length(cons),by=2)
      tmp <- rbind(tmp1,tmp2)
      #now sort them to get the order correct
      tmp <- tmp[order(tmp$X3),]
      #Finally add a grouping variable
      tmp$color <- rep(paste(labels[i],i),dim(tmp)[1])
      p.df <- rbind(p.df,tmp)
     # A tricky thing can be that your colors will be all wrong if 
     # I don't adjust the label vector, shortening it for columns
     # that are all 0's so instead I build up a label index
    label.in <- c(label.in,i)
      }
	
    
  }
      my.plot<-my.plot+geom_path(data=p.df,aes(x=X1,y=X2,group=color,colour=color))+scale_colour_manual(values=labels[label.in],legend=F)
		my.plot <-   my.plot+opts(axis.ticks = theme_blank(), axis.title.y = theme_blank(), axis.text.y =  theme_blank(), axis.title.x = theme_blank(), axis.text.x =  theme_blank()) 
	return(list(plot=my.plot,rawdat=p.df))
}
 
 
 
##############Sample Data Generation######
# Data inputs:  In this formulation the species that is consumed are the rows
# and the consumers are the columns
# Data example: Here I use a random network example
 
 
######Large random matrix
rand.mat <- matrix(rbinom(1600,1,.2),ncol=40,nrow=40)
diag(rand.mat)<- 0
lab <- rep("Black",40)
test<- plot.webgg(rand.mat,lab)
test$plot
 
####Small random matrix with colored links
rand.mat <- matrix(rbinom(100,1,.3),ncol=10,nrow=10)
diag(rand.mat)<- 0
lab <- c(rep("blue",3),rep("red",7))
test<- plot.webgg(rand.mat,lab)
test$plot
##############Note that overlapping colors
# can cause other colors (in this example it would be purple)
# this will normally not happen unless you have reciprocal links
# if anyone really wants that fixed I could figure it out 
# lastly here is an example using a real food web from 
# Bascompte et al. 10.1073/pnas.0501562102 at http://www.pnas.org/cgi/content/full/0501562102/DC1
# found at http://knb.ecoinformatics.org/knb/metacat?action=read&qformat=nceas&docid=bowdish.272 
#
basc.web <- as.matrix(read.csv("basc_web.csv",header=F))
####Note that I cleaned it up by removing the row and column
#labels in the original files and saved it as a CSV
 
#now replace values greater than 0 but less than 1 with 1's
for(i in 1:249){basc.web[which(basc.web[,i]>0),i]<-1 }
labs <- rep("Black",249)
basc.plot <- plot.webgg(basc.web,labs)
basc.plot$plot
 
##########Finally an example generated with a niche-foodweb simulation
# from Williamns and Martinez: http://www.nature.com/nature/journal/v404/n6774/abs/404180a0.html
#  I have a brief undocumented function below to generate the web, it 
# requires S (# of species) and C (connectivity)
 
niche.model <- function(S,C){
  new.mat <- matrix(0,nrow=S,ncol=S)
	ci <- vector()
	niche <- runif(S,0,1)
	r <- rbeta(S,1,((1/(2*C))-1)) * niche
	for(i in 1:S){ci[i]<-runif(1,r[i]/2,niche[i])}
	
	#now set the smallest species niche value to have an n of 0
	r[which(niche==min(niche))] <- .00000001
  for(i in 1:S){
		for(j in 1:S){
		   if(niche[j] > (ci[i]-(.5*r[i])) && niche[j]< (ci[i]+.5*r[i])){new.mat[j,i]<-1}
			}
    }
new.mat <- new.mat[,order(apply(new.mat,2,sum))]
return(new.mat)
}
###Plot
niche.web <- niche.model(20,.25)
labs <- rep("Black",20)
niche.plot <- plot.webgg(niche.web,labs)
niche.plot$plot

#######################################################################

###Temporal networks
#A evolução temporal das redes nos leva a outra perspectiva da estrutura social e, em alguns casos, agrega dados em uma janela de tempo que pode destacar difusões de estruturas temporais de informações importantes, comunidades, formações de opinião, etc. Não muitas opções para visualização de redes dinâmicas em softwares de SNA, a maioria trata somente redes estáticas. E em alguns casos, eles são bem limitados em layouts dinâmicos. O Gephi tem um plugin bem interessante de Timeline, mas nenhum ainda possui uma possibilidade de transformar a animação em vídeo. Veremos neste post como fazer isso usando um script em R e usar o ffmpeg para tornar snapshots de grafos em vídeo. A idéia é simples: 1- Gerar snapshots da rede em tempos diferentes usando R e sua biblioteca igraph; 2- Juntar todos em um vídeo usando o ffmpeg. Para o passo 1, nós precisamos “desenhar” a rede para fazermos o cada snapshot. Precisamos organizar em um layout os nós e as arestas específicas de acordo com o tempo. O pacote igraph tem vários layouts, por exemplo, o Kamada-Kawai e o Fruchterman-Reingold. Mas temos complicações no meio do caminho: Os algoritmos de layout geralmente, em suas iterações visam minimizar a energia das forças físicas entre os nós, e começam de uma    configuração inicial que tipicamente é de uma condição, inicialmente, aleatória. Isso significa que chamadas sucessivas do algoritmo resultaria em diferentes resultados, caso sua rede não evoluísse continuamente. Em nossa situação de rede temporal, temos que o layout de um snapshot para o outro seria diferente e geraria uma descontinuidade de layouts de um snapshot para o outro. Para nossa sorte, no igraph 0.6, podemos especificar as posições iniciais dos nós. Podemos usar isso nos dois algoritmos citados acima. O plano é usar o layout do snapshot anterior como condição inicial do próximo snapshot. A implementação seria assim:

library(igraph) 
par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0)) 
g <- watts.strogatz.game(1,20,3,0.4) 
layout.old <- layout.fruchterman.reingold(g)
for(i in 1:4){ 
layout.new <- layout.fruchterman.reingold(g,params=list(niter=10,maxdelta=2,start=layout.old)) 
plot(g,layout=layout.new) 
layout.old <- layout.new 
}

#Percebe-se que há dois parâmetros passados para a função de layout: niter=10 que especifica o número de iterações de minimização de energia no algoritmo. Esse número deveria ser pequeno, caso contrário o resultado final seria bem diferente da condição inicial. O mesmo se faz com o outro parâmetro maxdelta=2 que controla o máximo de mudança de posiçãoque os nós sofrem no processo de minimização. O outro problema é que em uma rede temporal, os nós e/ou as arestas aparecem e desaparecem dinamicamente. Assim a rede que depende do tempo pode ter números diferentes de nós e/ou arestas de um snapshot para o outro. Isso significa que a rede anterior não pode ser usada como condição inicial para a próxima rede. A solução para esse problema é considerar todos (passado/presente/futuro) nós e arestas para calcular o layout mas só mostrar os nós presentes no grafo, fazendo com que os outros se tornem transparentes. Esse truque permite a reutilização dos layouts entre os passos, mas também produzirá uma visualização mais ou menos constante, onde o layout em seu tempo específico não estaria relacionado com a estrutura instantânea do grafo temporal. Para superar esse problema, tiramos vantagem de uma propriedade dos algoritmos: nós que são conectados são atraídos entre si por suas arestas. Em determinado instante, nós podemos modificar a atração entre os nós dependendo se o nó está presente ou não. Só podemos, no igraph 0.6, usar isso no algoritmo Fruchterman-Reingold, que tem o parâmetro weights, um vetor que diz os pesos das arestas que é usado para medir a atração entre os nós conectados. Por exemplo, usaremos isso para ser 1 o peso de um nó presente e 0(zero) o peso dos outros nós. Isso produzirá um layout onde os nós presentes são bem conectados enquanto os outros, passado/futuro, são repelidos deles. Esse efeito enfatiza o aparecimento e o desaparecimento dos nós, mas pode criar muita confusão caso isso aconteça muito. Para testar essas idéias, trabalharemos com um exemplo importante na Teoria de Grafos Complexos: um algoritmo para gerar redes sem escala de forma aleatória, Barabási-Albert Model. Em nossa implementação, mantemos o mecanismo simples: começar com um certo número de nós, e a cada passo adicionar um nó para se conectar ao nós existentes, que são selecionados proporcionalmente ao número de links, que eles os nós presentes, já tem. Esse mecanismo, nós leva, a ter nós bem fortemente conectados juntos, “hubs”, com uma grande quantidade de nós fracamente conectados. Um modelo para esse nós, é encontrado no seguinte link: Cada linha na forma id1 | id2 | tempo, indica que há um link entre id1 e id2 e ele aparece no tempo determinado. Dependendo do contexto, isso pode representar que a conexão foi ativada em um instante específico(por exemplo quando se dá RT em um tweet) ou que foi o instante exato em que a aresta surgiu(como o modelo de barabási-Albert, aqui mostrado). O código que gera os snapshots e produz figuras PNG para cada um, vem a seguir:

library(igraph)

#load the edges with time stamp
#there are three columns in edges: id1,id2,time
edges <- read.table("gsv.txt",header=T)

#generate the full graph
g <- graph.edgelist(as.matrix(edges[,c(1,2)]),directed=F)
E(g)$time <- edges[,3]

#generate a cool palette for the graph
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
YlOrBr.Lab <- colorRampPalette(YlOrBr, space = "Lab")
#colors for the nodes are chosen from the very beginning
vcolor <- rev(YlOrBr.Lab(vcount(g)))

#time in the edges goes from 1 to 300. We kick off at time 3
ti <- 3
#weights of edges formed up to time ti is 1. Future edges are weighted 0
E(g)$weight <- ifelse(E(g)$time < ti,1,0)
#generate first layout using weights.
layout.old <- layout.fruchterman.reingold(g,params=list(weights=E(g)$weight))

#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10 
#of the time (i.e. 10 snapshots) between adding two consecutive nodes 
dt <- 0.1
#Output for each frame will be a png with HD size 1600x900 : )
png(file="example%03d.png", width=1600,height=900)
nsteps <- max(E(g)$time)
#Time loop starts
for(ti in seq(3,total_time,dt)){
  #define weight for edges present up to time ti.
  E(g)$weight <- ifelse(E(g)$time < ti,1,0) 
  #Edges with non-zero weight are in gray. The rest are transparent
  E(g)$color <- ifelse(E(g)$time < ti,"gray",rgb(0,0,0,0))
  #Nodes with at least a non-zero weighted edge are in color. The rest are transparent
  V(g)$color <- ifelse(graph.strength(g)==0,rgb(0,0,0,0),vcolor)
  #given the new weights, we update the layout a little bit
  layout.new <- layout.fruchterman.reingold(g,params=list(niter=10,start=layout.old,weights=E(g)$weight,maxdelta=1))
  #plot the new graph
  plot(g,layout=layout.new,vertex.label="",vertex.size=1+2*log(graph.strength(g)),vertex.frame.color=V(g)$color,edge.width=1.5,asp=9/16,margin=-0.15)
  #use the new layout in the next round
  layout.old <- layout.new 
}
dev.off()

#######################################################################

### Plotting interaction acumulation curves (http://pedroj.github.io/interaccum)

Estimating frugivore species richness. A “phytocentric” sampling

#Species (taxa) are in rows. Census samples are the columns.  Code
#variables are: cla - Class: Aves, Mammalia ord - Order fam - Family gen
#- Genus sp - Species code - species labels totvis - visits recorded
#totbic - number of fruits pecked sde - effectiveness Then columns cec18,
#cec02... inidicate individual plants.  The adjacency matrix entries hold
#the number of records.
# 
#We may eventually need these libraries.
require(vegan)
require(ade4)
#Data input. First a dataset with group codes and labels.
accum <- read.table("data/cecropia.txt", header = TRUE, 
                       sep = "\t", dec = ",", 
                       na.strings = "NA")
#I transpose the dataset (needed for accumulation curves).
mat <- data.frame(t(accum[, 10:37]))  # Just the adjacency matrix, and we add rownames
colnames(mat) <- accum$code
head(mat[, 1:6])

##Thr_orna Bro_tiri Eup_chal Pyr_fron Coe_flav Cis_leve
## cec18      999      400        0      120        6        0
## cec02     1113        0       20        1        0        0
## cec03      742        0       20        4        1        1
## cec25       49        0        0        0        4        0
## cec22      315      256        4        0        0        0
## cec06       99       16        0        0        0       16

specpool(mat)  # This is the species richness estimates

##     Species chao chao.se jack1 jack1.se jack2  boot boot.se  n
## All      38 50.5   8.457 52.46    5.052 58.35 44.71   2.873 28

# Now, plot the species accumulation curves.
all <- specaccum(mat, method = "random")
plot(all, ci.type = "poly", col = "blue", lwd = 2, ci.lty = 0, ylim = c(0, 45), 
    ci.col = "lightblue", main = "Cecropia glaziouvi", xlab = "Number of trees", 
    ylab = "Number of frugivore species")
boxplot(all, col = "yellow", add = TRUE, pch = "+")



Estimating fruit species richness. A “zoocentric” sampling

# Reading the dataset, from a matrix of 1054 samples of three warbler
# species: Sylvia atricapilla, Sylvia borin, and Sylvia melanocephala.
# Data input. First a dataset with group codes and labels.
sylvia <- read.table("data/hr_sylvia.txt", header = TRUE, sep = "\t", dec = ".", 
    na.strings = "NA")
# By species
satr <- sylvia[sylvia$species == "SATR", ][, 4:20]
sbor <- sylvia[sylvia$species == "SBOR", ][, 4:20]
smel <- sylvia[sylvia$species == "SMEL", ][, 4:20]
#
specpool(satr)  # Fruit species richness estimates

##     Species chao chao.se jack1 jack1.se jack2  boot boot.se   n
## All      15   15       0    16   0.9984    17 15.44  0.5453 643

specpool(sbor)

##     Species  chao chao.se jack1 jack1.se jack2  boot boot.se   n
## All      13 13.25  0.7289 13.99   0.9942 13.02 13.71  0.7319 173

specpool(smel)

##     Species chao chao.se jack1 jack1.se jack2  boot boot.se   n
## All      13   15   3.742 14.99    1.408 15.99 13.93  0.8036 238

# Now, plot the species accumulation curves.  Function to estimate
# accumulation curves and plot
spacc <- function(data, thetitle) {
    spaccum <- specaccum(data, method = "random")
    plot(spaccum, ci.type = "poly", col = "blue", lwd = 2, ci.lty = 0, ylim = c(0, 
        20), ci.col = "lightblue", main = thetitle, xlab = "Number of samples", 
        ylab = "Number of fruit species")
    # NOT RUN: boxplot(spaccum, col='yellow', add=TRUE, pch='+')
}
par(mfrow = c(3, 1))
spacc(satr, "Sylvia atricapilla")
spacc(sbor, "Sylvia borin")
spacc(smel, "Sylvia mlanocephala")



Accumulation curves for interactions

# Create dummy datasets with pairwise interactions recorded in each
# sampling.  List of the sampled matrices.  Day 1- getting the pairwise
# interaction labels.
source("vectorize.R")
source("matfills.R")  # This creates a randomly-filled matrix
M1 <- randommat(5, 8)
colnames(M1) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")
rownames(M1) <- c("A1", "A2", "A3", "A4", "A5")
MM <- vectorize(M1)
colnames(MM) <- c("A", "P", "I")
head(MM)

##    A  P I
## 1 A1 P1 0
## 2 A1 P2 0
## 3 A1 P3 0
## 4 A1 P4 0
## 5 A1 P5 0
## 6 A1 P6 0

lab <- paste(MM$A, "-", MM$P)
MM <- data.frame(lab)
# Generate the additional matrices
m <- function() {
    mat <- randommat(5, 8)
    colnames(mat) <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8")
    rownames(mat) <- c("A1", "A2", "A3", "A4", "A5")
    return(mat)
}
# List of matrices (50 samples)
mlist <- list(m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), 
    m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), 
    m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), m(), 
    m(), m(), m(), m(), m(), m(), m())
# mlist should be a list of observed matrices, corresponding to different
# sampling events (censuses, days, etc.)
tt <- sapply(mlist, function(m) cbind(vectorize(m)), simplify = FALSE, USE.NAMES = FALSE)
# Create a dataframe with the pairwise interactions recorded in each
# sample.
ttt <- as.data.frame(unlist(tt, recursive = F))
ttt <- ttt[, c(seq(from = 3, to = 150, by = 3))]
MM <- data.frame(cbind(MM, ttt))
colnames(MM) <- c("Pairwise", rep("sample", 50))
head(MM[, 1:8])

##   Pairwise sample sample.1 sample.2 sample.3 sample.4 sample.5 sample.6
## 1  A1 - P1      1        1        0        0        0        0        0
## 2  A1 - P2      0        0        0        0        0        1        0
## 3  A1 - P3      0        0        0        0        0        0        0
## 4  A1 - P4      1        0        0        0        0        0        0
## 5  A1 - P5      1        0        0        0        0        0        1
## 6  A1 - P6      0        1        0        0        0        0        0

# Specify only the numerical columns!
mat <- t(MM[, 2:ncol(MM)])  # Note that I transpose the matrix to get
# samples as rows.
specpool(mat)  # Statistics

##     Species chao chao.se jack1 jack1.se jack2 boot boot.se  n
## All      39   41   3.742 40.96    1.386 41.94 40.4   1.166 50

all <- specaccum(mat, method = "random")
plot(all, ci.type = "poly", col = "blue", lwd = 2, ci.lty = 0, ylim = c(0, 40), 
    ci.col = "lightblue", main = "Accumulation analysis - Interactions", xlab = "Number of  censuses/samples", 
    ylab = "Number of distinct pairwise interactions")
boxplot(all, col = "yellow", add = TRUE, pch = "+")
