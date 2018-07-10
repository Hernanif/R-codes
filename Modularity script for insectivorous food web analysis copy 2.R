###Modularity script for the analysis of the Insectivorous data from the PhD ###
#Written by: Hernani Oliveira
#Date: 02/02/2017

library(igraph)
library(igraphdata)

Name                               Directed            Weighted        Components 
Edge-betweenness                      T                   T                 T  
Leading eigenvector                   F                   F                 T
Fast-greedy                           F                   T                 T
Louvain                               F                   T                 T
Walktrap                              F                   T                 F
Label propagation                     F                   T                 F
Infomap                               T                   T                 T
Spinglass                             F                   T                 F
Optimal                               F                   T                 T

#Loads the dataset
data(Bali)
iBali<-asIgraph(Bali)

#Calculates modularity using different algorithms
cw<-cluster_walktrap(iBali)
modularity(cw)
membership(cw)

ceb<-cluster_edge_betweenness(iBali)
modularity(ceb)
membership(ceb)

cs<-cluster_spinglass(iBali)
modularity(cs)
membership(cs)

cfg<-cluster_fast_greedy(iBali)
modularity(cfg)
membership(cfg)

clp<-clutser_label_prop(iBali)
modularity(clp)
membership(clp)

cle<-cluster_leading_eigen(iBali)
modularity(cle)
membership(cle)

cl<-cluster_louvain(iBali)
modularity(cl)
membership(cl)

co<-cluster_optimal(iBali)
modularity(co)
membership(co)


###Opens the file and reads the nodes with the attributes set for each node
dolphinVertices <??? read.csv("data/dolphin_vertices.csv")
head(dolphinVertices, n=4)
## Name Gender ##1 Beak Male ## 2 Beescratch Male ## 3 Bumper Male ## 4 CCL Female
And add this as the vertices argument to graph_from_data_frame.
Note: It's important that the first column of the vertex data frame is the names and they match the edge list perfectly.
dolphin <??? graph_from_data_frame(dolphinEdges, vertices=dolphinVertices,
directed=FALSE)


#Getting informations from a bigger graph
is.connected(jg)
[1] FALSE

no.clusters(jg)
[1] 4881

table(clusters(jg)$csize) 8
1    3 4 25389
4871 8 1 1

max(degree(jg, mode="in"))
[1] 248

max(degree(jg, mode="out"))
[1] 195

max(degree(jg, mode="all"))
[1] 313


###Opens and reads the data from the package igraphdata
library(igraphdata)
data("USairports")
graph_attr(USairports)

#These commands access the vertex attributes of each vertex
vertex_attr_names(USairports)
## [1] "name"     "City"     "Position"
vertex_attr(USairports, "City")


###Access all attributes of a vertex with double square brackets
V(USairports)[["JFK"]] # All attributes

###Add new attributes to a graph
V(USairports)[1:5]$City # Access attributes
## [1] "Bangor, ME"    "Boston, MA"    "Anchorage, AK" "New York, NY"
## [5] "Las Vegas, NV"
# Add new attributes
V(USairports)$Group <??? sample(c("A","B"), vcount(USairports), replace=TRUE)
V(USairports)[[1:5]] # Double square brackets give all attributes
## + 5/755 vertices, named:
##   name          City         Position Group
## 1  BGR
## 2  BOS
## 3  ANC Anchorage, AK N611028 W1495947     A
## 4  JFK  New York, NY N403823 W0734644     B
## 5  LAS Las Vegas, NV N360449 W1150908     B


###Select edges between vertices
We can also access edges between named vertices using the special %??????% (undirected) and %???>% (directed) operators.
E(USairports)["JFK" %??????% "BOS"] # Edges in both directions
## + 26/23473 edges (vertex names):
##  [1] BOS???>JFK BOS???>JFK JFK???>BOS JFK???>BOS BOS???>JFK JFK???>BOS BOS???>JFK
##  [8] JFK???>BOS BOS???>JFK BOS???>JFK BOS???>JFK BOS???>JFK BOS???>JFK JFK???>BOS
## [15] JFK???>BOS JFK???>BOS JFK???>BOS BOS???>JFK JFK???>BOS BOS???>JFK BOS???>JFK
## [22] JFK???>BOS JFK???>BOS BOS???>JFK JFK???>BOS BOS???>JFK


#Select all carriers from JFK to BOS
All carriers from JFK to BOS.
unique(E(USairports)["JFK" %???>% "BOS"]$Carrier) # Directed edges
## [1] "JetBlue Airways"              "Compass Airlines"
## [3] "Pinnacle Airlines Inc."       "Comair Inc."
## [5] "Atlantic Southeast Airlines"  "American Eagle Airlines Inc."
## [7] "Chautauqua Airlines Inc."


#Select all edges between two groups
The edge selectors can be between groups of vertices:
# Grep the state code from the city
inCal <??? grepl("CA$", V(USairports)$City)
inNy <??? grepl("NY$", V(USairports)$City)
# Edges from CA to NY
E(USairports)[V(USairports)[inCal] %???>% V(USairports)[inNy]]
## + 35/23473 edges (vertex names):
##  [1] LAX???>JFK LAX???>JFK LAX???>JFK LAX???>JFK SAN???>JFK SFO???>JFK SFO???>JFK
##  [8] SFO???>JFK BUR???>JFK LAX???>JFK LGB???>JFK OAK???>JFK SAN???>JFK SFO???>JFK
## [15] SJC???>JFK SMF???>JFK LAX???>JFK LAX???>JFK LAX???>JFK SAN???>JFK SAN???>JFK
## [22] SFO???>JFK SFO???>JFK SNA???>JFK LAX???>ALB LAX???>JFK LAX???>JFK SFO???>JFK
## [29] SFO???>JFK SFO???>JFK BUR???>FRG LAX???>JFK LAX???>JFK SFO???>JFK SFO???>JFK


#To get a new graph containing the selected vertices we must also copy over all of the edges between those vertices. This is done by the induced_subgraph function
# inCal has the vertex ids we want
calAirports <??? induced_subgraph(USairports, inCal)
calAirports
## IGRAPH DN?????? 34 381 ?????? US airports
## + attr: name (g/c), name (v/c), City (v/c), Position (v/c), Group
## | (v/c), Carrier (e/c), Departures (e/n), Seats (e/n), Passengers
## | (e/n), Aircraft (e/n), Distance (e/n)
## + edges (vertex names):
##  [1] LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO
##  [8] LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO LAX???>SFO
## [15] LAX???>SFO LAX???>SFO LAX???>SAN LAX???>SAN LAX???>SAN LAX???>SAN LAX???>SAN
## [22] LAX???>SAN LAX???>SAN LAX???>SAN LAX???>SAN LAX???>SAN LAX???>SAN LAX???>SAN
## [29] LAX???>SAN LAX???>SAN LAX???>SMF LAX???>SMF LAX???>SMF LAX???>SMF LAX???>SMF
## [36] LAX???>SMF LAX???>SMF LAX???>SNA LAX???>BUR LAX???>OAK LAX???>OAK LAX???>OAK
## + ... omitted several edges


#A common task is to subset all of the neighbours of a particular vertex. To return all neigh??? bours within a distance, d, of a number of targets we can do
#which returns a list containing the vertices within 2 of JFK and LAX. If we want the neighbourhood of a vertex as a new graph we can do
d2Vertices <??? ego(USairports, nodes = c("JFK","LAX"), order=2)
JFKNet <??? make_ego_graph(USairports, nodes = "JFK", order=2)[[1]]
# Returns a length 1 list


#The edges and vertices functions can be used to add edges
#And edges
# A directed completely empty graph
g <??? make_empty_graph(n = 0, directed = TRUE)
g <??? g + vertices(c("A","B","C"))
g <??? g + edges(c("A","C", "B","C")) # edge from A to C and "B" to "C"


#For deletion it's easiest to use edge and vertex selectors
#Note this will remove all attached edges. All edge and vertex subsetting can be used here.
g <??? g ??? V(g)["A"] # Remove edge A




#Add properties to a graph - Any of the plotting properties can also be set as attributes of the vertices/edges.
V(g)$shape <??? "circle" # Applies to all vertices
V(g)$size <??? 15
V(g)$color <??? "orange"
isVowel <??? V(g)$name %in% c("A","E","I","O","U")
# Now overwrite the vowel nodes
V(g)[isVowel]$shape <??? "square"
V(g)[isVowel]$color <??? "royalblue"
V(g)[isVowel]$size <??? 25
plot(g, layout=lo)


## Add properties to the edges - A selection of edge properties we can change:
# Setting edge attributes
E(g)$width <??? 1
E(g)[V(g)[isVowel] %??????% V(g)[isVowel]]$width <??? 4
## plot(g, layout=lo)

#Calls for the shortest path between two vertices
sp <??? shortest_paths(dolphin, from="Beak", to="Whitetip")
sp$vpath
## [[1]]
## + 4/62 vertices, named:
## [1] Beak     Grin     SN63     Whitetip


#Import data to Statnet
#The statnet suite is a complete set of network tools. However, it can be a little tricky to use compared to igraph. Here we will look at how to quickly get our data into network objects from files and converting from igraph.
library(network)
el <??? read.csv("data/dolphin_edges.csv")
# Read the edge list
dolphinNet <???network(el, matrix.type='edgelist',
                     ignore.eval=FALSE, directed=FALSE)
# Attach the vertex attributes
va <??? read.csv("data/dolphin_vertices.csv",
               stringsAsFactors = FALSE)
# it doesn't seem to like factors
dolphinNet%v%"Gender" <??? va$Gender
# It also has a default plot method
## plot(dolphinNet)


#Convert a network from igraph to statnet
library(intergraph)
# statnet ???> igraph
dolphinIgraph <??? asIgraph(dolphinNet, vnames="vertex.names")
# igraph ???> statnet
data("karate")
karateNet <??? asNetwork(karate)




####Exponential Random Graph Models
library(ergm)
fit <??? ergm(dolphinNet ~ edges)
# Check it worked
pFit <??? exp(fit$coef)/(1+exp(fit$coef))
p <??? network.density(dolphinNet)
p
## Evaluating log???likelihood at the estimate.
fit
## [1] 0.0840825
##
## MLE Coefficients:
##  edges
## ???2.388
pFit
##     edges
## 0.0840825


###Group Membership
fitGender <??? ergm(dolphinNet ~ edges + nodematch("Gender"))
## Evaluating log???likelihood at the estimate.
summary(fitGender)

##
## ==========================
## Summary of model fit
## ==========================
##
## Formula:   dolphinNet ~ edges + nodematch("Gender")
## <environment: 0x000000001fba2598>
##
## Iterations:  6 out of 20
##
## Monte Carlo MLE Results:
##                  Estimate Std. Error MCMC % p???value
## edges             ???2.9024     0.1385      0  <1e???04 ***
## nodematch.Gender   0.9538     0.1737      0  <1e???04 ***
## ?????????
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
##      Null Deviance: 2621  on 1891  degrees of freedom
##  Residual Deviance: 1060  on 1889  degrees of freedom
##
## AIC: 1064    BIC: 1075    (Smaller is better.)


######Making a unipartite projection from a bipartite graph
#Let's say you are interested in the co-membership relations between the individuals. What you need to do is to create a one-mode projection of the bipartite network. Note that there are actually TWO ways to project a bipartite network: You can make a co-membership network of nodes, or a network of groups that share members. You can get both of these at once with this function:
pr=bipartite.projection(bg)  #bg is an igraph object
pr


#you can see here that the bipartite projection has given us a list object with two graphs: pr$proj1 (5 vertices and 6 edges) and pr$proj2 (4 vertices and 6 edges). 
#get.adjacency(pr$proj1,sparse=FALSE,attr="weight")
#will give you the adjacency matrix of the first projection:
  
  A B C D E
A 0 1 1 1 0
B 1 0 2 0 1
C 1 2 0 0 1
D 1 0 0 0 1
E 0 1 1 1 0

#You can plot this projection:
plot(pr$proj1,edge.width=E(pr$proj1)$weight^2,edge.color="black",vertex.label=V(pr$proj1)$name)



###Plots the networks in a heatmap way
#We'll first make a sample distance matrix that represents the dissimilarity between 20 fictional species based on a single variable, e.g., size. We'll sort the data first so that species 1 will be close in size to species 2, and so on. 
size=rnorm(20) 
d=data.frame(sort(size)) 
dis=dist(d,diag=TRUE) 
m=as.matrix(dis)

#This will give us a symmetrical matrix of 'distances' between each species. The diagonal should all be 0, since all species will be perfectly similar to itself. 
#Now we want to display this distance matrix as a heat map. By far the easiest way to do this is to use the levelplot() function in the package 'lattice'.
library(lattice) 
levelplot(m[1:ncol(m),ncol(m):1])

#You can also recreate this figure without relying on the package 'lattice'. The function image() is useful for this. The rest of the following code has to do with the layout.
quartz(width=7,height=6) #make a new quartz window of a given size 
par(mar=c(2,3,2,1)) #set the margins of the figures to be smaller than default 
layout(matrix(c(1,2),1,2,byrow=TRUE),widths=c(7,1)) #set the layout of the quartz window. This will create two plotting regions, with width ratio of 7 to 1 
image(m[1:ncol(m),ncol(m):1],col=new.palette(20),xaxt="n",yaxt="n") #plot a heat map matrix with no tick marks or axis labels 
axis(1,at=seq(0,1,length=20),labels=rep("",20)) #draw in tick marks 
axis(2,at=seq(0,1,length=20),labels=rep("",20)) 

#adding a color legend 
s=seq(min(m),max(m),length=20) #20 values between minimum and maximum values of m 
l=matrix(s,ncol=length(s),byrow=TRUE) #coerce it into a horizontal matrix 
image(y=s,z=l,col=new.palette(20),ylim=c(min(m),max(m)),xaxt="n",las=1) #plot a one-column heat map


#A related function is heatmap(), which implements a hierarchical clustering algorithm and then displays a dendogram and heatmap matrix. You can control the color scheme the same way as before. Here, I use the symm=TRUE argument because it is a symmetrical matrix.
heatmap(m,symm=TRUE,col=new.palette(20))

#You'll notice though that the row and column orders have been re-ordered based on the hierarchical clustering results. But remember that branches of the tree can be flipped around. In the above example, it looks as though species 1 and 2 are closely related to species 20, but this is not the case. if you flip the tree around at the second branching node, you can get species 20 to be far from species 1 and 2. In fact, it should be possible to flip the branches around until you get the rows/columns to be ordered as 1,2,3,4... while keeping the same branches (this is because we ordered the species according to size to begin with in the example dataset above). So, the heatmap() function is useful if you are using hierarchical clustering methods, but may not be appropriate for other applications.



#Plotting centrality indices
#The centralityPlot function can be used to plot centrality indices. These are standardized to z
#z-scores by default (centered and divided by the standard deviations):
library(qgraph)
centralityPlot(Graph_lasso)

#The centralityPlot can be used to compare different networks as well:
centralityPlot(GGM = list(unregularized = Graph_pcor, regularized = Graph_lasso),
                 Ising = list(unregularized = Graph_Ising1, regularized = Graph_Ising2))





#######
#Compares the found by each modularity algorithm against one another
compare(as.numeric(facotr(V(iBali)$role)), cw, method="adjusted.rand")
compare(cw, ceb, method="adjusted.rand")
compare(cw, cs, method="adjusted.rand")
compare(cw, cfg, method="adjusted.rand")

#Plots the modules of interaction using the different algorithms for comparison
op<-par(mfrow=c(3,2), mar=c(3,0,2,0))
plot(ceb, iBali, vertex.label=V(iBali)$role, main"Edge betweenness")
plot(cfg, iBali, vertex.label=V(iBali), vertex.label=V(iBali)$role, main="Fast greedy")
plot(clp, iBali, vertex.label=V(iBali), vertex.label=V(iBali)$role, main= "Label propagation")
plot(cle, iBali, vertex.label=V(iBali)$role, main"Leading Eigenvector")
plot(cs, iBali, vertex.label=V(iBali)$role, main="Spinglass")
plot(cw, iBali, vertex.label=V(iBali)$role, main="Walktrap")



#Plots the degree distribution of different networks
degDol <??? degree(dolphin)
head(degDol)
hist(degree(dolphin))

#Plots the degree distribution of different networks with different axes
In-degree distribution
2 > plot(degree.distribution(jg, mode="in"), log="xy")




#Plotting community detection in networks
fc <- fastgreedy.community(simplify(as.undirected(jg2)))
memb <- community.to.membership(jg2,
                                  fc$merges,
                                  which.max(fc$modularity))
lay <- layout.drl(jg2)
jg3 <- graph.empty(n=vcount(jg2))
colbar <- rainbow(5)
col <- colbar[memb$membership+1]
col[is.na(col)] <- "grey"
plot(jg3, layout=lay, vertex.size=1,
       vertex.label=NA, asp=FALSE,
       vertex.color=col,
       vertex.frame.color=col)


#Calculates the values of Edge betweenness for each vertix
g <??? make_full_graph(4) + vertex(1) + make_full_graph(4)
g <??? g + edges(c(4,5,5,6))
V(g)$name <??? LETTERS[1:9]
betweenness(g)

##  A  B  C  D  E  F  G  H  I
##  0  0  0 15 16 15  0  0  0


#Simulation example
olphinSim <??? simulate(fitGender)
op <??? par(mfrow=c(1,2), mar=c(0,0,1,0))
plot(dolphinNet, vertex.col=c("pink","blue", "white")[factor(dolphinNet%v%"Gender")],
     main="Real Network")
plot(dolphinSim, vertex.col=c("pink","blue", "white")[factor(dolphinNet%v%"Gender")],
     main="Simulated Network")
par(op)

#Godness of fit function
The goodness of fit function, gof will simulated many networks and calculate summary statistics to see how close the model networks are to your original model. This is very useful to assess the quality of your model. Here we fit degree well (left) but clustering and geodesic distance poorly.
gofGender <??? gof(fitGender)
plot(gofGender)




#Way of comparing the maximum modularity of a graph according to the fast greedy algorithm
library(igraph)
# read graph from csv file
G<-read.graph("edgelist.txt", format="ncol")
fgreedy<-fastgreedy.community(G,merges=TRUE, modularity=TRUE)
memberships <-community.to.membership(G, fgreedy$merges, steps=which.max(fgreedy$modularity)-1)
print(paste('Number of detected communities=',length(memberships$csize)))
# Community sizes:
print(memberships$csize)
# modularity:
max(fgreedy$modularity)



#Plot the network with nodes representing the colors of the communities
g<-as.undirected
com<-community.to.membership(g, SNets_fastgreedy$merges, setps=which.max(SNets_fastgreedy$modularity)-1)
V(g)$color<-com$membership+2
g$layout<-layout.fruchterman.reingold(plot(g, edge.arrow=.5, vertex.label.family="serif", vertex.label.cex=0.7))




library(igraphdata)

#Comparing communities against null models
data(karate)
nv <- vcount(karate)
ne <- ecount(karate)
degs <- degree(karate)

ntrials <- 1000

num.comm.rg <- numeric(ntrials)
for(i in (1:ntrials)){
  g.rg<-erdos.renyi.game(nv, ne, type="gnm")
  c.rg<-fastgreedy.community(g.rg)
  num.comm.rg[i]<-length(c.rg)}

num.comm.grg<-numeric(ntrials)
for(i in (1:ntrials)){
  g.rg<-degree.sequence.game(degs, method="vl")
  c.grg<-fastgreedy.community(g.rg)
  num.comm.grg[i]<-length(c.grg)
}

rslts<-c(num.comm.rg, num.comm.grg)
indx<-c(rep(0, ntrials), rep(1, ntrials))
counts<-table(indx, rslts)/ntrials
barplot(counts, beside=TRUE, col=c("blue", "red"), xlab="Number of Communities", ylab="Relative Frequency",
        legend=c("Fixed Size", "Fixed Degree Sequence"))



#Calculates the number of articulation points in a graph
library(igraphdata)
data(yeast)
yeast.cut.vertices <- articulation.points(yeast.gc)
length(yeast.cut.vertices)


#Captures and plots the vertices according to their centrality values
library(igraph)
library(sna)
data("karate")
A <- get.adjacency(karate, sparse=FALSE)
library(network)
g <- network::as.network.matrix(A)
library(sna)
sna::gplot.target(g, degree(g), main="Degree", circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=c("blue", rep("red", 32), "yellow"), edge.col="darkgray")

#Calculates and plots the coreness of a graph
cores<-graph.coreness(karate)
sna::gplot.target(g, cores, circ.lab=FALSE, circ.col="skyblue", usearrows=FALSE, vertex.col=cores, edge.col="darkgray")
detach("package:network")
detach("package:sna")

#Calculates the communities in the network and the components that make its parts
kc<-fastgreedy.community(karate)
length(kc)
sizes(kc)
membership(kc)
plot(kc, karate)
library(ape)
dendplot(kc, mode="phylo")

#Groups communities according to their functional groups
func.class<-get.vertex.attribute(yeast.gc, "Class")
table(func.class)

yc<-fastgreedy.community(yeast.gc)
c.m<-membership(yc)

table(c.m, func.class, useNA=c("no"))
func.class

#PLots different networks while keeping the positions of the nodes in each one
opar<-par()
par(mfrow=c(2,4),
    mar=c(0.5, 0.5, 0.5, 0.5),
    oma=c(0.5, 1.0, 0.5, 0))
for(i in (1:8)){
  plot(gg.sl12[[i]], layout=l, vertex.size=5, edge.width=2*(E(g.week.wgtd)$weight)/1000,
       vertex.color=v.cols, vertex.label=NA)
}
  par(opar)
  
}


#Page 191 => This code plots the degree distribution of the different nodes in the networks during time
all.deg<-sapply(g.s112, degree)
sl.lab<-unlist(lapply(1:8, function(i) 
               paste(12*(1-i), "-", 12*i, "hrs", sep="")))
deg.df<-data.frame(Degree=as.vector(all.deg), Slice=rep(sl.lab, each=75),
                   Status=rep(V(g.week)$Status, times=8))
library(ggplot2)
p=qplot(factor(Degree), data=deg.df, geom="bar", fill=Status)
p + facet_grid(Slice ~ .) + xlab("Degree") +ylab("Count")





####### Plotting the graphs #######

library(igraph)

#Data analisis from el Nino dry season
attributes<-read.table("attr.Elninodryspecies92withoutzeros.txt", h=T)
dieta<-read.table("Elninodryspecies92withoutzeros.txt", h=T)

d1<-graph_from_incidence_matrix(dieta)

d1<-graph_from_incidence_matrix(dieta, weigth=TRUE)


V(d1)$ID=as.character(attributes$ID[match(V(d1)$name,attributes$Node.ID)]) # This code says to create a vertex attribute called "Sex" by extracting the value of the column "Sex" in the attributes file when the Bird ID number matches the vertex name.
V(d1)$ID


V(d1)$color=V(d1)$ID #assign the "Sex" attribute as the vertex color
V(d1)$color=gsub("B.plicata","green",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("L.brachyotis","blue",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("P.mesoamericanus","brown",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("P.personatus","red",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("R.tumida","yellow",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("S.bilineata","black",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$color=gsub("S.leptura","grey",V(d1)$color) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 


V(d1)$color=gsub("Araneae", "orange",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Blattodea", "pink",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Coleoptera", "darkblue",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Diptera", "darkgreen",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Hemiptera", "purple",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Hymenoptera", "beige",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Lepidoptera", "midnightblue",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Neuroptera", "azure",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Psocoptera", "cyan",V(d1)$color) #Bats will be black
V(d1)$color=gsub("Trichoptera", "khaki1",V(d1)$color) #Bats will be black


V(d1)$shape <- V(d1)$ID 
V(d1)$shape=gsub("B.plicata","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("L.brachyotis","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("P.mesoamericanus","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("P.personatus","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("R.tumida","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("S.bilineata","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("S.leptura","square",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 


V(d1)$shape=gsub("Araneae","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Blattodea","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Coleoptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Diptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Hemiptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Hymenoptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Lepidoptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Neuroptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Psocoptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 
V(d1)$shape=gsub("Trichoptera","circle",V(d1)$shape) #Plants will be green #gsub() function replaces all matches of a string, if the parameter is a string vector, returns a string vector of the same length and with the same attributes (after possible coercion to character). Elements of string vectors which are not substituted will be returned unchanged (including any declared encoding). 



#PLot the network
plot.igraph(d1, layout=layout.kamada.kawai, vertex.color=V(d1)$color, edge.color="grey", vertex.size=5, vertex.label=NA)


legend(-2.6, 1.5, legend=c("Araneae", "Blattodea", "Coleoptera", "Diptera",
                           "Hemiptera", "Hymenoptera", "Lepidoptera", "Neuroptera", "Psocoptera", "Trichoptera",
                           "B.plicata", "L.brachyotis", "P.mesoamericanus", "P.personatus", "R.tumida",
                           "S.bilineata", "S.leptura"), 
       col=c("orange", "pink", "darkblue", "darkgreen", "purple", "beige", "midnightblue", "azure", "cyan", "khaki1", "green",
             "blue", "brown", "red", "yellow", "black", "grey"), pch=c(19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 15, 15, 15, 15, 15, 15, 15),
       pt.cex=1.0,cex=0.8, bty="n", title="El Nino (dry season) - Dry forest")

pdf(file='test.pdf', width=6, height=6) 
dev.off() 


png(file="mag_feb.png", units="in", width=11, height=8.5, res=300)




ceb<-cluster_edge_betweenness(d1)
modularity(ceb)
membership(ceb)

cfg<-cluster_fast_greedy(d1)
modularity(cfg)
membership(cfg)

plot(cfg, d1, vertex.label=V(d1), main="Edge betweenness")

fgreedy<-fastgreedy.community(d1, merges=TRUE, modularity=TRUE)
max(fgreedy$modularity)





#Stochastic Block Models
