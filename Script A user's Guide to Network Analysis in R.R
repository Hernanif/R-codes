##### Script written from the book A User's Guide to Network Analsis in R #####
#Written by Hernani Oliveira
#Purpose: Create, analyse and retrieve basic informations from networks
#Date: 07/10/2016

library(igraph)
library(igraphdata)
library(statnet)
library(UserNetR)
library(network)
library(lattice)
library(qgraph)
library(arcdiagram)
library(intergraph)
library(circlize)
library(RSiena)

#Creates a matrix 
netmat1 <- rbind(c(0, 1, 1, 0, 0),
                 c(0, 0, 1, 1, 0),
                 c(0, 1, 0, 0, 0),
                 c(0, 0, 0, 0, 0),
                 c(0, 0, 1, 0, 0))

rownames(netmat1) <- c("A", "B", "C", "D", "E")
colnames(netmat1) <- c("A", "B", "C", "D", "E")
net1 <- network(netmat1, matrix.type="adjacency")
plot(net1)


all(as.matrix(net1) == as.sociomatrix(net1))

as.matrix(net1, matrix.type = "edgelist")

#Converts an object of the class network to an Igraph object
library(intergraph)
net1igraph<-asIgraph(net1)

#Set the node attributes of the graph
set.vertex.attribute(net1, "gender", c("F", "F", "M", "F", "M"))
net1 %v% "alldeg" <- degree(net1)
list.vertex.attributes(net1)



#Filters a subgraph of the network based on Node Values (the get inducedSubgraph() function returns a new network object that is filtered based on the vertex attribute criteria. This works because the %v% operator returns a list of vertex ids)
n1F<-get.inducedSubgraph(net1, which(net1 %v% "gender" == "F"))
n1F[,]

gplot(n1F, displaylabels=TRUE)

### Plots a network that is a subset of the example network who all have degree grater than or equal to 2 (it works the same way, but uses the %s% operator, which is a shortcut for the get.induceddSubgraph function)
deg<- net1 %v% "alldeg"
n2 <- net1 %s% which (deg > 1)
gplot(n2, displaylabels=TRUE)

#Removing isolates from the network
library(UserNetR)
data(ICTS_G10)
gden(ICTS_G10)
length(isolates(ICTS_G10))
n3<-ICTS_G10
delete.vertices(n3, isolates(n3))
gden(n3)
length(isolates(n3))

########Filtering based on edge values of a network (it can help to make the network less dense and easier to interpret or when you are only interested in a specific set of nodes)
data(DHHS)
d <- DHHS
gden(d)
op <- par(mar = rep(0, 4))
gplot(d,gmode="graph", edge.lwd=d %e% 'collab', edge.col="grey50", vertex.col="lightblue", 
      vertex.ce=1.0, vertex.sides=20)
par(op)

#Visualizes the values of the matrixes for six members of the network
as.sociomatrix(d)[1:6, 1:6]
list.edge.attributes(d)
as.sociomatrix(d, attrname="collab")[1:6, 1:6]
table(d %e% "collab")

#Filter the edges to only include formal collaboration tie vaues stored in the 'collab' edge attribute
#Then we filt out the ties that we want to ignore (in this case the ties that are coded 1 and 2 are replaced with 0s)
#Then we create a new network based on the filtered matrix (the key here is that a tie will be created anywhere a non-zero value is found in d.val)
#Also, by using the ignore.eval and names.eval options we store the retained edge values in an edg attribute called 'collab'
d.val <- as.sociomatrix(d, attrname="collab")
d.val[d.val < 3] <- 0
d.filt <- as.network(d.val, directed=FALSE, 
                     matrix.type="a", ignore.eval=FALSE,
                     names.eval="collab")

#Summary and the density of the graph 
summary(d.filt, print.adj=FALSE)
gden(d.filt)

#Now we can analyse a smaller set of ties for important structural information
op <- par(mar = rep(0, 4))
gplot(d.filt, gmode="graph", displaylabels=TRUE,
      vertex.col="lightblue", vertex.cex=1.3,
      label.cex=0.4, label.pos=5,
      displayisolates=FALSE)
par(op)

#The gplot function has a limited ability to display only the ties that exceed some lower treshold, using the tresh option
op <- par(mar = rep(0, 4))
d.val <- as.sociomatrix(d, attrname="collab")
gplot(d.val, gmode="graph", thresh=2, 
      vertex.col="lightblue", vertex.cex=1.3,
      label.cex=0.4, label.pos=5,
      displayisolates=FALSE)
par(op)


###### Transforming a directed network to a non-directed network
net1mat <- symmetrize(net1, rule="week")
net1mat

net1symm <- network(net1mat, matrix.type="adjacency")
network.vertex.names(net1symm) <- c("A", "B", "C", "D", "E")
summary(net1symm)

######## Basic Network Plotting and Layout ########
data(Moreno)
op<-par(mar = rep(0, 4), mfrow=c(1,2))
plot(Moreno, mode="circle", vertex.cex=1.5)
plot(Moreno, mode="fruchtermanreingold", vertex.cex=1.5)
par(op)

#Rules to better plot graphs
# - minimize edge crossings
# - maximize the symmetry of the layout of nodes
# - minimize the variability of the edge lengths
# - maximize the angle between edges when they cross or join nodes
# - minimize the total space used for the network display

#Fruchterman & Reingold algorithm => have connected nodes have a spring-like attractive force,
#while simultaneously assigning repulsive forces to all pairs of nodes. The springs act to pull
#connected nodes closer to one another, while the repulsive forces push unconnected nodes away from each other.

### Basic plotting algorithms and methods
data(Bali)
op <- par(mar = c(0, 0, 4, 0), mfrow = c(2, 3))
gplot(Bali, gmode="graph", edge.col="grey75", vertx.cex=1.5, mode='circle', main="circle")
gplot(Bali, gmode="graph", edge.col="grey75", vertx.cex=1.5, mode='eigen', main="eigen")
gplot(Bali, gmode="graph", edge.col="grey75", vertx.cex=1.5, mode='random', main="random")
gplot(Bali, gmode="graph", edge.col="grey75", vertx.cex=1.5, mode='spring', main="spring")
gplot(Bali, gmode="graph", edge.col="grey75", vertx.cex=1.5, mode='fruchtermanreingold', main="fruchtermanreingold")
gplot(Bali, gmode="graph", edge.col="grey75", vertx.cex=1.5, mode='kamadakawai', main="kamadakawai")
par(op)

#Finer control over network layout
mycoords1 <- gplot(Bali, gmode="graph", vertex.cex=1.5)
mycoords2 <- mycoords1
mycoords2[,2] <- mycoords1[,2]*1.5
mycoords1
mycoords2
op <- par(mar=c(4,3,4,3), mfrow=c(1,2))
gplot(Bali, gmode="graph", coord=mycoords1, vertex.cex=1.5, suppress.axes=FALSE,
      ylim=c(min(mycoords2[,2])-1, max(mycoords2[,2])+1),
      main="Original Coordinates")
gplot(Bali,gmode="graph", coord=mycoords2, vertex.cex=1.5, suppress.axes=FALSE,
      ylim=c(min(mycoords2[,2])-1, max(mycoords2[,2])+1), main="Modified coordinates")
par(op)

##### Effective network graphic design #####
#Produce a plot with attractive light blue nodes
library(statnet)
data(Bali)
gplot(Bali, vertex.col="slateblue2", gmode="graph")

#Visualize all 657 possible colors in R
colors()

#Show the variations of colors in Red-Green-Blue triplets of intensity and plots the graph using this variation
col2rgb('slateblue2')
gplot(Bali, vertex.col=rgb(122,103,238, 
                           maxColorValue=255), gmode="graph")

#Make transparent nodes (function specially important for highly dense graphs)
rgb() #goes from 0 to 1
ndum <- rgraph(300, tprob=0.025, mode="graph")
op <- par(mar = c(0, 0, 2, 0), mfrow=c(1,2))
gplot(ndum, gmode="graph", vertex.cex=2, vertex.col=rgb(0, 0, 139, maxColorValue=255),
      edge.col="grey80", edge.lwd=0.5,
      main="Fully opaque")
gplot(ndum, gmode="graph", vertex.cex=2, vertex.col=rgb(0,0,139, alpha=80,
                                                        maxColorValue=255),
      edge.col="grey80", edge.lwd=0.5,
      main="Partly transparent")
par(op)

#Colors differently each node of the network
rolelab <- get.vertex.attribute(Bali, "role")
op <- par(mar=c(0, 0, 0, 0))
plot(Bali, usearrows=FALSE, vertex.cex=1.5, label=rolelab, displaylabels=T, vertex.col="role")
par(op)

#Pick colors from Palette
library(RColorBrewer)
display.brewer.pal(5, "Dark2")

#Plots the Bali network from Palette
my_pal <- brewer.pal(5, "Dark2")
rolecat <- as.factor(get.vertex.attribute(Bali, "role"))
plot(Bali, vertex.cex=1.5, label=rolelab, displaylabels=T, vertex.col=my_pal[rolecat])

#Plotting a network with different node shapes
op <- par(mar=c(0,0,0,0))
sidenum <- 3:7
plot(Bali, usearrows=FALSE, vertex.cex=4, displaylabels=F, vertex.sides=sidenum[rolecat])
par(op)

#Plotting a network with different node sizes
op <- par(mar = c(0, 0, 2, 0), mfrow=c(1,3))
plot(Bali, vertex.cex=0.5, main="Too small")
plot(Bali, vertex.cex=2, main="Just right")
plot(Bali, vertex.cex=6, main="Too large")
par(op)

#Calculate three different measures of Centrality - Each of these lines of code produces a vector of centrality measures for each node (large numbers indicate greater centrality)
deg <- degree(Bali, gmode="graph")
deg

cls<-closeness(Bali, gmode="graph")
cls

bet <- betweenness(Bali, gmode="graph")
bet

#Uses the information of node centrality to assign node size and correct the size by using the logarithmic value of centrality
op<-par(mar = c(0,0,2,1), mfrow=c(1,2))
plot(Bali, usearrows=T, vertex.cex=deg, main="Raw")
plot(Bali, usearrows=FALSE, vertex.cex=log(deg), main="Adjusted")
par(op)

#These two other examples show other types of adjustments of node size that might be important to use
op <- par(mar = c(0,0,2,1), mfrow=c(1,2))
plot(Bali, usearrows=T, vertex.cex=cls, main="Raw")
plot(Bali, usearrows=FALSE, vertex.cex=4*cls, main="Adjusted")
par(op)

op<-par(mar = c(0,0,2,1), mfrow=c(1,2))
plot(Bali, usearrows=T, vertex.cex=bet, main="Raw")
plot(Bali, usearrows=FALSE, vertex.cex=sqrt(bet+1), main="Adjusted")
par(op)

#The function rescale takes a vector of node characteristics and rescales the values to fit between the low and high values
rescale <- function(nchar,low,high){
  min_d <- min(nchar)
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low
  rscl
}
plot(Bali, vertex.cex=rescale(deg,1,6), main= "Adjusted node sizes with rescale function.")

#Plotting a network with labels
get.vertex.attribute(Bali, "vertex.names")
op <- par(mar = c(0,0,0,0))
plot(Bali, displaylabels=TRUE, label.cex=0.8, pad=0.4, label.col="darkblue")
par(op)

#Plotting a network with text stored in an object vertex attribute to label the nodes
rolelab <- get.vertex.attribute(Bali, "role")
plot(Bali, usearrows=FALSE, label=rolelab, displaylabels=T, label.col="darkblue")

#Plotting a network with edge widths (the Bali network includes a tie attribute called IC, which is a simple five-level ordinal scale that was used to measure the amount of interaction between members of the network. This attribute is used to set the width of the ties of the network)
op <- par(mar = c(0, 0, 0, 0))
IClevel <- Bali %e% "IC"
plot(Bali, vertex.cex=1.5, edge.lwd=1.5*IClevel)
par(op)
  
#Plotting a network with edge color (we set up a color palette that can be used to index the correct color choicee, based on the categorical edge vector. We used in this case type#1 for blue, #type2 for red and #type3 for green )
n_edge <- network.edgecount(Bali)
edge_cat <- sample(1:3, n_edge, replace=T)
linecol_pal <- c("blue", "red", "green")
plot(Bali, vertex.cex=1.5, vertex.col="grey25", edge.col=linecol_pal[edge_cat], edge.lwd=2)

#Plotting a network with different edge types
n_edge <- network.edgecount(Bali)
edge_cat <- sample(1:3, n_edge, replace=T)
line_pal <- c(2, 3, 4)
gplot(Bali, vertex.cex=0.8, gmode="graph", vertex.col="gray50", edge.lwd=1.5, edge.lty=line_pal[edge_cat])

#Creating a network with legends
my_pal <- brewer.pal(5, "Dark2")
rolecat <- as.factor(get.vertex.attribute(Bali, "role"))
plot(Bali, vertex.cex=rescale(deg,1,5),
     vertex.col=my_pal[rolecat])
legend("bottomleft", legend=c("BM", "CT", "OA", "SB", "TL"), col=my_pal, pch=19,
       pt.cex=1.5,bty="n", title="Terrorist Role")

#### Advanced Network Graphics ####
#Simple INteractive Networks in igraph
library(intergraph)
library(igraph)
data(Bali)
iBali <- asIgraph(Bali)
Coord <- tkplot(iBali, vertex.size=3, vertex.label=V(iBali)$role,
                vertex.color="darkgreen")
MCoords <- tkplot.getcoords(Coord)
plot(iBali, layout=MCoords, vertex.size=5, vertex.label=NA, vertex.color="lightblue")

#Publishing Web-based interactive network diagrams
library(networkD3)
src <- c("A", "A", "B", "B", "C", "E")
target <- c("B", "C", "C", "D", "B", "C")
net_edge <- data.frame(src, target)
simpleNetwork(net_edge)

###Specialized Network Diagrams
#Arc Diagrams - generally used when the position of the nodes is less important than the pattern of the ties
library(devtools)
install_github("gastonstat/arcdiagram")
library(arcdiagram)
library(igraph)
library(intergraph)
data(Simpsons)
iSimp <- asIgraph(Simpsons)
simp_edge <- get.edgelist(iSimp)

arcplot(simp_edge)

s_grp <- V(iSimp)$group
s_col = c("#a661a", "#dfc27d", "#80cdc1", "#018571")
cols = s_col[s_grp]
node_deg <- degree(iSimp)

arcplot(simp_edge, lwd.arcs=2, cex.nodes=node_deg/2, labels=V(iSimp)$vertex.names, col.labels="darkgreen"
        font=1, pch.nodes=21, line=1, col.nodes=cols, bg.nodes=cols, show.nodes=TRUE)


#Chord diagrams
library(statnet)
library(circlize)
data(FIFA_Nether)
FIFAm <- as.sociomatrix(FIFA_Nether, attrname='passes')
names <- c("GK1", "DF3", "DF4", "DF5", "MF6", "FW7", "FW9", "MF10", "FW11", "DF2", "MF8")
rownames(FIFAm) = names
colnames(FIFAm) = names

FIFAm[FIFAm < 10] <- 0
FIFAm

chordDiagram(FIFAm)

grid.col <- c("#AA3939", rep("#AA6C39", 4), rep("#2D882D", 3), rep("#226666", 3))
chordDiagram(FIFAm, directional=TRUE, 
             grid.col = grid.col,
             order=c("GK1", "DF2", "DF3", "DF4", "DF5", "MF6", "MF8", "MF10", "FW7",
                     "FW9", "FW11"))

#Heatmaps for network data
data(FIFA_Nether)
FIFAm <- as.sociomatrix(FIFA_Nether, attrname = 'passes')
colnames(FIFAm) <- c("GK1", "DF3", "DF4", "DF5", "MF6", "FW7", "FW9", "MF10", "FW11",
                     "DF2", "MF8")
rownames(FIFAm) <- c("GK1", "DF3", "DF4", "DF5", "MF6", "FW7", "FW9", "MF10",
                     "FW11", "DF2", "MF8")
palf <- colorRampPalette(c("#669999", "#003333"))
heatmap(FIFAm[,11:1], Rowv = NA, Colv = NA, col = palf(60),
        scale="none", margins=c(11,11))

####### Actor Prominence #######
#Degree Centrality
net <- network(net_mat)
net %v% 'vertex.names'
degree(net, gmode="graph")

#Closeness centrality
closeness(net, gmode="graph")

#Betweenness centraliy
betweenness(net, gmode="graph")

#Relationships between different measures of centrality
data(DHHS)
df.prom <- data.frame(
  deg = degree(DHHS),
  cls = closeness(DHHS)
  btw = betweenness(DHHS)
  evc = evcent(DHHS)
  inf = infocent(DHHS)
  flb = flowbet(DHHS)
  )
cor(df.prom)

#Centralization: Network level indices of Centrality
dum1 <- rbind(c(1,2), c(1,3), c(1,4), c(1,5))
star_net <- network(dum1, directed=FALSE)
dum2 <- rbind(c(1,2), c(2,3), c(3,4), c(4,5), c(5,1))
circle_net <- network(dum2, directed = FALSE)
par(mar=c(4,4,.1,.1))
my_pal <- brewer.pal(5, "Set2")
gplot(star_net, usearrows=FALSE, displaylabels = FALSE, vertex.cex = 2,
      vertex.col=my_pal[1],
      edge.lwd=0, edge.col="grey50", xlab="Star Graph")
gplot(circle_net, usearrows = FALSE, displaylabels = FALSE, 
      vertex.cex=2, vertex.col=my_pal[3],
      edge.lwd=0, edge.col="grey50", xlab="Circle Graph")

closeness(circle_net)
centralization(circle_net, closeness)

closeness(star_net)
centralization(star_net, closeness)


#Reporting centrality
data(Bali)
str(degree(Bali))
summary(degree(Bali))

data(Bali)
my_pal <- brewer.pal(5, "Set2")
rolecat <- Bali %v% "role"
gplot(Bali, usearrows=FALSE, displaylabels=TRUE, vertex.col=my_pal[as.factor(rolecat)], edge.lwd=0, edge.col="grey25")
legend("topright", legend=c("BM", "CT", "OA", "SB", "TL"), col=my_pal, pch=19, pt.cex=2)

#PLots a table with values for centrality across the nodes (more useful for smaller tables)
data(Bali)
df.prom2 <- data.frame(
  degree = degree(Bali),
  closeness = closeness(Bali),
  betweenness = betweenness(Bali))
row.names(df.prom2) <- Bali %v% "vertex.names"
df.promsort <- df.prom2[order(-df.prom2$degree),]
cd <- centralization(Bali, degree)
cc <- centralization(Bali, closeness)
cb <- centralization(Bali, betweenness)
df.promsort <- rbind(df.promsort, c(cd, cc, cb))
row.names(df.promsort)[18] <- "\\emph{Centralization}"

#Calculateis the normalized degree centrality (this rescales the degree, so it matches between 0 and 1)
deg <- degree(Bali, rescale=TRUE)
op <- par(mfrow=c(1,2))
gplot(Bali, usearrows=FALSE, displaylabels=FALSE, vertex.cex=deg, 
      vertex.col=my_pal[as.factor(rolecat)]), edge.lwd=0, edge.col="grey25",
      main="Too small")
gplot(Bali, usearrows=FALSE, displaylabels=FALSE, vertex.cex=deg*20, vertex.col=my_pal[as.factor(rolecat)],
      edge.lwd=0, edge.col="grey25", main="A little better")
par(op)

#Comabines node level categorical information with node-level quantitative information
deg <- degree(Bali, rescale=TRUE)
gplot(Bali, usearrows = FALSE, displaylabels=TRUE, vertex.cex = deg*12, vertex.col=my_pal[as.factor(rolecat)]
      edge.lwd=0.5, edge.col="grey75")
legend("topright", legend=c("BM", "CT", "OA", "SB", "TL"), col=my_pal, pch=19, pt.cex=2)

#Calculate network cutpoints (cutpoints are points that if take out of the network will increase the number of components of it)
cpnet <- cutpoints(net, mode="graph", return.indicator=TRUE)
gplot(net, gmode="graph", vertex.col=cpnet+2, coord=coords, jitter=FALSE, displaylabels=TRUE)

#Confirms the nodes as cutting points of the network
net2 <- net2
components(net2)
delete.vertices(net2,7)
components(net2)
gplot(net2, gmode="graph", vertex.col=2, coord=coords[-7], jitter=FALSE, displaylbels=TRUE)

#Calculate the number of bridges of a network (bridges are the edge equivalents of the cutpoints in a network)
bridges <- function(dat, mode="graph", connected=c("strong", "weak")) {
  e_cnt<-network.edgecount(dat)
  if(mode == "graph") {
    cmp_cnt <- components(dat)
    b_vec <- rep(FALSE, e_cnt)
    for(i in 1:e_cnt) {
      dt2 <- dat
      delete.edges(dat2,i)
      b_vec[i] <- (components(dat2) !=cmp_cnt)
    }
  }
  else{
    cmp_cnt <- components(dat, connected=connected)
    b_vec <- rep(FALSE, e_cnt)
    for(i in 1:e_cnt) {
      dat2 <- dat
      delete.edges(dat2, i)
      b_vec[i] <- (components(dat2, connected=connected)
                   !=cmp_cnt)
    }
  }
  return(b_vec)
}

bridges(net)

#Display where are the bridges from a network
brnet <- bridges(Net)
gplot(Net, gmode="graph", vertex.col="red", edge.col=brnet+2, coord=coords, jitter=FALSE, displaylabels=TRUE)


####### Subgroups #######
#Cliques - it is a maximally complete subgraph; that it is a subset of nodes that have all possible ties among them
library(igraph)
clqexmp <- graph.formula(A:B:C:D--A:B:C:D,D-E,E-F-G-E)
clique.number(clqexmp) #returns the size of the largest clique
cliques(clqexmp) #returns all cliques constrained by minimum or maximum size
maximal.cliques(clqexmp) #find all the largest cliques in a network
V(clqexmp)[unlist(largest.cliques(clqexmp))] #returns a list of vertex names instead of IDs

#Clique explanation => cliques are very conservative definition of a cohesive group. Consider
#a subgraph made up of seven vertices. To be a clique, all of the 21 possible ties must exist
#between all seven members. If only one is missing, then the seven vertices will not belong 
#to one clique, even though the density of these seven vertices (20/21=0.95) would suggest a
#a cohesive subgroup.
#A consequence of this fragility is the second major issue of cliques: they simply are not 
#very common in larger social networks.

#Simulates four random networks where the average degree was constrained to aproximately 6
#and show that the number of cliques remain roughly constant
g25 <- erdos.renyi.game(25, 75, type="gnm")
g50 <- erdos.renyi.game(50, 150, type="gnm")
g100 <- erdos.renyi.game(100, 300, type="gnm")
g500 <- erdos.renyi.game(500, 1500, type="gnm")
nodes <- c(25,50,100,500)
lrgclq <- c(clique.number(g25), clique.number(g50), clique.number(g100), clique.number(g500))
numclq <- c(length(cliques(g25, min=3)), length(cliques(g50, min=3)), length(cliques(g100, min=3)),
            length(cliques(g500, min=3)))
clqinfo <- data.frame(Nodes=nodes, Largest=lrgclq, Number=numclq)
clqinfo

#K-core explanation => a number of variations for cliques has been proposed. One of them is the
#k-cores. K-core is a maximal subgraph where each vertex is connected to at least k other vertices
#in the subgraph. They have a number of advantages in relation to cliques: they are nested (every
#member of a 4-core is also a member of a 3-core, and so on), they do not overlap, and they are
#easy to identify

#Works with the network DHHS ad selects a subgraph that have from 3 to 4 ties
data(DHHS)
library(intergraph)
iDHHS <- asIgraph(DHHS)
graph.density(iDHHS)
iDHHS <- subgraph.edges(iDHHS, E(iDHHS), [collab > 2])
graph.density(iDHHS)

#Identify the k-core structure of the network
coreness <- graph.coreness(iDHHS)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness


#Plot the network using the k-core set information and label the nodes with k-core membership value
Vname <- get.vertex.attribute(iDHHS, name='vertex.names', index=V(iDHHS))
V(iDHHS)$name <- Vname
V(iDHHS)$color <- coreness + 1
op <- par(mar = rep(0, 4))
plot(iDHHS, vertex.label.cex=0.8)
par(op)

colors <- rainbow(maxCoreness)
op <- par(mar = rep(0, 4))
plot(iDHHS, vertex.label=coreness, vertex.color=colors[coreness])
par(op)

#Progressively peels away each of the lower k-cores to better visualize k-core structure and plots it
V(iDHHS)$name <- coreness
V(iDHHS)$color <- colors[coreness]
iDHHS1_6 <- iDHHS
iDHHS2_6 <- induced.subgraph(iDHHS, vids=which(coreness > 1))
iDHHS3_6 <- induced.subgraph(iDHHS, vids=which(coreness > 2))
iDHHS4_6 <- induced.subgraph(iDHHS, vids=which(coreness > 3))
iDHHS5_6 <- induced.subgraph(iDHHS, vids=which(coreness > 4))
iDHHS6_6 <- induced.subgraph(iDHHS, vids=which(coreness > 5))

lay <- layout.fruchterman.reingold(iDHHS)
op <- par(mfrow=c(3,2), mar = c(3,0,2,0))
plot(iDHHS1_6, layout=lay, main="All k-cores")
plot(iDHHS2_6, layout=lay[which(coreness > 1),], main="k-cores 2-6")
plot(iDHHS3_6, layout=lay[which(coreness > 2),], main="k-cores 3-6")
plot(iDHHS4_6, layout=lay[which(coreness > 3),], main="k-cores 4-6")
plot(iDHHS5_6, layout=lay[which(coreness > 4),], main="k-cores 5-6")
plot(iDHHS6_6, layout=lay[which(coreness > 5),], main="k-cores 6-6")
par(op)

########Community detection ###########
#Modularity (ranges from -1/2 to +1 - the closer to 1, the more the network exhibits clustering with respect to the given node grouping)
#Example of modularity calculation using a bad grouping and a good grouping graph
g1 <- graph.formula(A-B-C-A, D-E-F-D, G-H-I-G, A-D-G-A)
V(g1)$grp_good <- c(1,1,1,2,2,2,3,3,3)
V(g1)$grp_bad <- c(1,2,3,2,3,1,3,1,2)
op <- par(mfrow=c(1,2))
plot(g1, vertex.color=(V(g1)$grp_good), vertex.size=20, main="Good Grouping")
plot(g1, vertex.color=(V(g1)$grp_bad), vertex.size=20, main="Bad Grouping")
par(op)
modularity(g1, V(g1)$grp_good)
modularity(g1, V(g1)$grp_bad)


library(intergraph)
data(DHHS)
iDHHS <- asIgraph(DHHS)
table(V(iDHHS)$agency)
V(iDHHS)[1:10]$agency
modularity(iDHHS, (V(iDHHS)$agency+1))

data(Moreno)
iMoreno <- asIgraph(Moreno)
table(V(iMoreno)$gender)
modularity(iMoreno, V(iMoreno)$gender)

data(Facebook)
levels(factor(V(Facebook)$group))
grp_num <- as.numeric(factor(V(Facebook)$group))
modularity(Facebook,grp_num)

#Community Detection Algorithms
cw <- cluster_walktrap(iMoreno)
membership(cw)
modularity(cw)
plot(cw, iMoreno)

cw <- cluster_walktrap(iDHHS)
modularity(cw)
membership(cw)
table(V(iDHHS)$agency, membership(cw))

data(Bali)
iBali <- asIgraph(Bali)

cw <- cluster_walktrap(iBali)
modularity(cw)
membership(cw)

ceb <- cluster_edge_betweenness(iBali)
modularity(ceb)
membership(ceb)

cs <- cluster_spinglass(iBali)
modularity(cs)
membership(cs)

cfg <- cluster_fast_greedy(iBali)
modularity(cfg)
membership(cfg)

clp <- cluster_label_prop(iBali)
modularity(clp)
membership(clp)

cle <- cluster_leading_eigen(iBali)
modularity(cle)
membership(cle)

cl <- cluster_louvain(iBali)
modularity(cl)
membership(cl)

co <- cluster_optimal(iBali)
modularity(co)
membership(co)

table(V(iBali)$role, membership(cw))
compare(as.numeric(factor(V(iBali)$role)),cw,method="adjusted.rand")
compare(cw,ceb,method="adjusted.rand")
compare(cw,cs,method="adjusted.rand")
compare(cw,cfg, method="adjusted.rand")

op<-par(mfrow=c(3,2), mar=c(3,0,2,0))
plot(ceb, iBali, vertex.label=V(iBali)$role, main="Edge Betweenness")
plot(cfg, iBali, vertex.label=V(iBali)$role, main="Fastgreedy")
plot(clp, iBali, vertex.label=V(iBali)$role, main="Label Propagation")
plot(cle, iBali, vertex.label=V(iBali)$role, main="Leading Eigenvector")
plot(cs, iBali, vertex.label=V(iBali)$role, main="Spinglass")
plot(cw, iBali, vertex.label=V(iBali)$role, main="Walktrap")
par(op)



####### Affiliation Networks #######
#An affiliation network is one where the members are affiliated with one another based on co-membership
#in a group, or co-participation in some type of event.

#Creates a 2-mode affiliation network
C1 <- c(1,1,1,0,0,0)
C2 <- c(0,1,1,1,0,0)
C3 <- c(0,0,1,1,1,0)
C4 <- c(0,0,0,0,1,1)
aff.df <- data.frame(C1,C2,C3,C4)
row.names(aff.df) <- c("S1", "S2", "S3", "S4", "S5", "S6")

#Bipartite graphs
library(igraph)
bn <- graph.incidence(aff.df)
plt.x <- c(rep(2,6), rep(4,4))
plt.y <- c(7:2, 6:3)
lay <- as.matrix(cbind(plt.x,plt.y))
shapes <- c("circle", "square")
colors <- c("blue", "red")
plot(bn, vertex.color=colors[V(bn)$type+1], vertex.shape=shapes[V(bn)$type+1], vertex.size=10, 
     vertex.label.degree=-pi/2, vertex.label.dist=1.2, vertex.label.cex=0.9, layout=lay)

#Affiliation Network basics
bn <-graph.incidence(aff.df)
bn
get.incidence(bn)
V(bn)$type
V(bn)$name

#Creating affiliation networks from edge lists
el.df <- data.frame(rbind(c("S1", "C1"),
                          c("S2", "C1"),
                          c("S2", "C2"),
                          c("S3", "C1"),
                          c("S3", "C2"),
                          c("S3", "C3"),
                          c("S4", "C2"),
                          c("S4", "C3"),
                          c("S5", "C3"),
                          c("S5", "C4"),
                          c("S6", "C4")))
el.df
bn2 <- graph.data.frame(el.df,directed=FALSE)
bn2

V(bn2)$type <- V(bn2)$name %in% el.df[,1]
bn2

graph.density(bn)==graph.density(bn2)

#Plotting affiliation networks
shapes <- c("circle", "square")
colors <- c("blue", "red")
plot(bn, vertex.color=colors[V(bn)$type+1], vertex.shape=shapes[V(bn)$type+1],
     vertex.size=10, vertex.label.degree=-pi/2, vertex.label.dist=1.2, vertex.label.cex=0.9)

#Projections
bn.pr <- bipartite.projection(bn)
bn.pr
graph.density(bn.pr$proj1)
bn.student <- bn.pr$proj1
bn.class <- bn.pr$proj2
graph.density(bn.student)

get.adjacency(bn.student, sparse=FALSE, attr="weight")
get.adjacency(bn.class,sparse=FALSE,attr="weight")

shapes <- c("circle", "square")
colors <- c("blue", "red")
op <- par(mfrow=c(1,2))
plot(bn.student, vertex.color="blue", vertex.shape="circle", main="Students",
     edge.width=E(bn.student)$weight*2, vertex.size=15, vertex.label.degree=-pi/2,
     vertex.label.dist=1.2, vertex.label.cex=1)
plot(bn.class,vertex.color="red", vertex.shape="square", main="Classes",
     edge.width=E(bn.student)$weight*2, vertex.size=15, vertex.label.degree=-pi/2,
     vertex.label.dist=1.2,vertex.label.cex=1)
par(op)

###Example: Hollywood actors as an affiliation network
###Analysis of entire hollywood affiliation network
#First step - load the file and explore the basic affiliation structure of the network
data(hwd)
h1 <- hwd
h1
V(h1)$name[1:10]
V(h1)$type[1:10]
V(h1)$IMDBrating[1:10]
V(h1)$name[155:165]

#Examines a small subset of the network
V(h1)$shape <- ifelse(V(h1)$type==TRUE,
                      "square", "circle")
V(h1)$shape[1:10]
V(h1)$color <- ifelse(V(h1)$type==TRUE, "red", "lightblue")


h2 <- subgraph.edges(h1, E(h1) [inc(V(h1)[name %in% c("The Wolf of Wall Street", "Gangs of New York", "The Departed")])])
plot(h2, layout = layout_with_kk)

#Analyse the network of interactions
graph.density(h1)

table(degree(h1,v=V(h1)[type==FALSE]))
mean(degree(h1,v=V(h1)[type==FALSE]))

V(h1)$deg <- degree(h1)
V(h1)[type==FALSE & deg > 4]$name
busy_actor <- data.frame(cbind(
  Actor = V(h1)[type==FALSE & deg > 4]$name
  Movies = V(h1)[type==FALSE & deg >4]$deg))
busy_actor[order(busy_actor$Movies, decreasing=TRUE),]

#Loops for through the actor nodes in the network and sums up IMDBrating
for(i in 161:1365) {
  V(h1)[i]$totrating <- sum(V(h1)[nei(i)]$IMDBrating)}

#Analyse the overall popularity of the movies
max(V(h1)$totrating, na.rm=TRUE)
pop_actor <- data.frame(cbind(Actor = V(h1)[type==FALSE & totrating > 40]$name,
                              Popularity = V(h1)[type==FALSE &
                              totrating > 40]$totrating))
pop_actor[order(pop_actor$Popularity,decreasing=TRUE),]

#Examine the network using some more traditional graphical and statistical approaches - calculate avgrating, 
#a characteristic that is based on the mean IMDBrating, rather than the sum. Then, a simple scatterplot and 
#regression are examined to see the relationship between number of movies and the average ratings of those movies
for(i in 161:1365){
  V(h1) [i]$avgrating <- mean(V(h1)[nei(i)]$IMDBrating)}
num <- V(h1)[type==FALSE]$deg
avgpop<- V(h1)[type==FALSE]$avgrating
summary(lm(avgpop~num))
scatter.smooth(num, avgpop, col=";ightblue", ylim=c(2,10),span=.8, xlab="Number of Movies",
               ylab="Avg. Popularity")
}
}
))

#Analysis of the Actor and movie projections
h1.pr <- bipartite.projection(h1)
h1.act <- h1.pr$proj1
h1.mov <- h1.pr$proj2
h1.act
h1.mov
op <- par(mar = rep(0, 4))
plot(h1.mov, vertex.color="red", vertex.shape="circle", vertex.size=(V(h1.mov)$IMDBrating)-3, vertex.label=NA)
par(op)

#Some basic network descriptive provide more information about the hollywood movie network
graph.density(h1.mov)
no.clusters(h1.mov)
clusters(h1.mov)$csize
table(E(h1.mov)$weight)

#Filter the complete movie network to examine the single large connected component
h2.mov<-induced.subgraph(h1.mov, vids=clusters(h1.mov)$membership==1)
plot(h2.mov, vertex.color="red", edge.width=sqrt(E(h1.mov)$weight), vertex.shape="circle",
     vertex.size=(V(h2.mov)$IMDBrating)-3, vertex.label=NA)

#Identify the higher density cores of the graph and zooms in
table(graph.coreness(h2.mov))
h3.mov <- induced.subgraph(h2.mov, vids=graph.coreness(h2.mov)>4)
h3.mov
plot(h3.mov, vertex.color="red", vertex.shape="circle", edge.width=sqrt(E(h1.mov)$weight),
     vertex.label.cex=0.7, vertex.label.color="darkgreen", vertex.label.dist=0.3,
     vertex.size=(V(h3.mov)$IMDBrating)-3)


############ Modeling Networks ###############
#ccording to Freeman (2004), modern social network analysis has four main characteristics:
# 1 - it is motivated by a structural intuition based on ties linking social actors
# 2 - it is grounded in systematic empirical data
# 3 - it draws heavily on graphic imagery
# 4 - it relies on the use of mathematical and/or computational models

#Erdos-Renyi Random Graph Model - despit the simplicity of random graph models, it hasled to a big
#number of discoveries about network structures, such as: for large n the network wil have a Poisson 
#degree distribution; random graphs become entirely connected for fairly low values of average degree
library(igraph)
g <- erdos.renyi.game(n=12, 10, type='gnm')
g
graph.density(g)
op <- par(mar=c(0,1,3,1), mfrow=c(1,2))
plot(erdos.renyi.game(n=12,10,type='gnm'),vertex.color=2, main="First random graph")
plot(erdos.renyi.game(n=12,10,type='gnm'), vertex.color=4, main="Second random graph")
g<-erdos.renyi.game(n=1000,.005,type='gnp')
plot(degree.distribution(g), type="b", xlab="Degree", ylab="Proportion")

crnd <- runif(500,1,8)
cmp_prp <- sapply(crnd,function(x) 
  max(clusters(erdos.renyi.game(n=1000, p=x/999))$csize)/1000)
smoothingSpline <- smooth.spline(crnd, cmp_prp, spar=0.25)
plot(crnd, cmp_pro, col='grey60', xlab="Avg. Degree", ylab="Largest Component Proportion")
lines(smoothingSpline, lwd=1.5)

n_vect <- rep(c(50,100,500,1000,5000), each=50)
g_diam <- sapply(n_vect, function (x))
diameter(erdos.renyi.game(n=x,p=6/(x-1)))

#Runs a total of 250 simulations, producing random graphs from 50 to 5,000 nodes
library(lattice)
bwplot(g_diam~factor(n_vect), panel=panel.violin, xlab="Network Size", ylab="Diameter")

###Small World Model
#The problem with Random Graph Models is that quite ofetn they have network properties that quite
#often don't ressemble real network  such as they have low levels of clustering and different
#degree distributions
#Small world models have more realistic levels of transitivity along with small diameters
#Demonstration of rewiring according to Small World model
g1 <- watts.strogatz.game(dim=1, size=30,nei=2,p=0)
g2 <- watts.strogatz.game(dim=1, size=30,nei=2,p=0.05)
g3 <- watts.strogatz.game(dim=1, size=30,nei=2,p=0.20)
g4 <- watts.strogatz.game(dim=1, size=30,nei=2,p=1)
op <- par(mar=c(2,1,3,1), mfrow=c(2,2))
plot(g1, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=0")))
plot(g2, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=0.05")))
plot(g3, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=0.20")))
plot(g4, vertex.label=NA, layout=layout_with_kk, main=expression(paste(italic(p), "=1")))

#Simulation that quickly show how rewiring reduces the diameter of a network in the small-world model
#The simulation is set to calculate 300 networks, ten each for the number of edges to rewire
#rnging from 1 to 30.
g100 <- watts.strogatz.game(dim=1, size=100,nei=2,p=0)
diameter(g100)
p_vect <- rep(1:30, each=10)
g_diam <- sapply(p_vect, function(x)
  diameter(watts.strogatz.game(dim=1, size=100, nei=2, p=x/200)))
smoothingSpline = smooth.spline(p_vect, g_diam, spar=0.35)
plot(jitter(p_vect,1), g_diam, col='grey60', xlab="NUmber of Rewired Edges",
     ylab="Diameter")
lines(smoothingSpline, lwd=1.5)

#Scale-free models
#They are tipically represented by a heavy-tailed degree distribution that aproximately follow
#a power law. The power law propoerty arises by a cumulative advantage or preferential attachment
#of specific nodes. This means that as the network grows, new ndoes are more likely to form ties
#with other nodes that already have many ties, due to their visibility in the network. The richer
#gets richer phenomena has been shown to lead to the power-law distribution in networks.

#Color coded example of the hubs and preferential attachment
g<-barabasi.game(500, directed = FALSE)
V(g)$color <- "lightblue"
V(g)[degree(g) >9]$color <- "red"
node_size <- rescale(node_char = degree(g), low=2, high=8)
plot(g, vertex.label=NA, vertex.size=node_size)

median(degree(g))
mean(degree(g))
table(degree(g))
op<-par(mfrow=c(1,2))
plot(degree.distribution(g), xlab="Degree", ylab="Proportion")
plot(degree.distribution(g), log='xy', xlab="Degree", ylab="Proportion")
par(op)

#The function barabasi.game can be adjusted to produce a number of different preferential attachment
#models

#Shows the tendency of the nodes to be connected or not according to a Barabasi model
g<-barabasi.game(500, out.dist = c(0.25, 0.5, 0.25), directed = FALSE, zero.appeal=1)
V(g)$color <- "lightblue"
V(g)[degree(g) > 9]$color <- "red"
node_size <- rescale(node_char = degree(g), low = 2, high = 8)
plot(g, vertex.label = NA, vertex.size=node_size)

#Ilustrates how preferential attachment networks grow
g1 <- barabasi.game(10,m=1, directed=FALSE)
g2<-barabasi.game(25,m=1,directed=FALSE)
g3<-barabasi.game(50,m=1,directed=FALSE)
g4<-barabasi.game(100,m=1,directed=FALSE)
op<-par(mfrow=c(2,2), mar=c(4,0,1,0))
plot(g1,vertex.label=NA, vertex.size=3, xlab="n = 10")
plot(g2,vertex.label=NA, vertex.size=3, xlab="n = 25")
plot(g3,vertex.label=NA, vertex.size=3, xlab="n = 50")
plot(g4,vertex.label=NA, vertex.size=3, xlab="n = 100")
par(op)

#Comparing random models to empirical networks
data(lhds)
ilhds<-asIgraph(lhds)
ilhds
graph.density(ilhds)
mean(degree(ilhds))
g_rnd <- erdos.renyi.game(1283, .0033, tytpe='gnp')
g_smwrld <- watts.strogatz.game(dim=1, size=1283,nei=2,p=.25)
g_prfatt <- barabasi.game(1283, out.dist=c(.15,.6,.25), directed=FALSE, zero.appeal=2)

#Makes three plots to compare different centrality measures
Data(ilhds)
net1<-asIgraph(ilhds)
centralityPlot(ilhds)

##### Statistical Network MOdels #####
#ERGMs (Exponential Random Graph MOdels)
#They are popular for at least four reasons:
#1 - They can handle complex dependences of network data without types of degeneray problems
#that were frequently encountered in earlier network models.
#2 - They are flexible and can handle many different types of predictors and variables.
#3 - The generative approach where overall network characteristics are predicted from individual 
#actor and local structural properties enhances the validity of the models.
#4 - Models have been implemented in programming suites and statistical packages suh as R, making 
#it easier for applied analysis to build, and disseminate the results of their network models

#Building Exponential Random GRaph MOdels
data(TCnetworks)
TCcnt <- TCnetworks$TCcnt
TCcoll <- TCnetworks$TCcoll
TCdiss <-  TCnetworks$TCdiss
TCdist <- TCnetworks$TCdist 
summary(TCdiss, print.adj=FALSE)
components(TCdiss)
gden(TCdiss)
centralization(TCdiss, betweenness, mode="graph")
deg<-degree(TCdiss, gmode='graph')
lvl <- TCdiss %v% 'agency_lvl'
plot(TCdiss, usearrows=FALSE, displaylabels=TRUE, vertex.cex=log(deg), vertex.col=lvl+1,
     label.pos=3, label.cex=.7, edge.lwd=0.5, edge.col="grey75")
legend("bottomleft". legend=c("Local", "State", "National"), col=2:4, pch=19, pt.cex=1.5)

#Building a NUll Model
library(ergm)
DSmod0 <- ergm(TCdiss ~ edges, control=control.ergm(seed=40))
class(DSmod0)
summary(DSmod0)
plogis(coef(DSmod0))

#Including Node Attributes
scatter.smooth(TCdiss %v% 'tob_yrs', degree(TCdiss,gmode='graph'), xlab='Years of Tobacco Experience',
               ylab='Degree')

DSmod1 <- ergm(TCdiss ~ edges + nodefactor('lead_agency') + nodecov('tob_yrs'),
               control=control.ergm(seed=40))
summary(DSmod1)

#Estimates the probability of observing certain typesof ties using the fitted parameter estimates
p_edg <- coef(DSmod1)[1]
p_yrs <- coef(DSmod1)[3]
plogis(p_edg + 5*p_yrs + 10*p_yrs)

#Including Dyadic Precitors
mixingmatrix(TCdiss, 'agency_lvl')
mixingmatrix(TCdiss, 'agency_cat')
DSmod2a <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl'), control=control.ergm(seed=40))
summary(DSmod2a)

DSmod2b <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl', diff=TRUE), 
                control=control.ergm(seed=40))
summary(DSmod2b)

DSmod2c <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodemix('agency_lvl', base=1), 
                control=control.ergm(seed=40))
summary(DSmod2c)

#Including Relational Terms (Network Predictors)
as.sociomatrix(TCdist,attrname='distance')[1:5, 1:5]
as.sociomatrix(TCcnt, attrname='contact') [1:5, 1:5]
DSmod3 <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl', diff=TRUE)
               + edgecov(TCdist,attr='distance') + edgecov(TCcnt,attr='contact'),
               control=control.ergm(seed=40))
summary(DSmod3)

#Including Local Structual Predictors (dyad Dependency)
DSmod4 <- ergm(TCdiss ~ edges + nodecov('tob_yrs') + nodematch('agency_lvl', diff=TRUE) +
               edgecov(TCdist,attr='distance') + edgecov(TCcnt, attr="contact") + 
                 gwesp(0.7, fixed=TRUE), control=control.ergm(seed=40))
summary(DSmod4)

#Examining Exponential Random Graph Models
#Model Interpretation
prd_prob1 <- plogis(-6.31 + 2*1*.099 + 1.52 + 4*1.042 + .858*(.50^4))
prd_prob1

prd_prob2 <- plogis(-6.31 +2*5*.099 + 1*1.042 + .858*(.50^4))
prd_prob2

#Model fit
DSmod.fit <- gof(DSmod4, GOF=~distance + espartners + degree + triadcensus, burnin=1e+5, interval=1e+5)
summary(DSmod.fit)
op <- par(mfrow=c(2,2))
plot(DSmod.fit, cex.axis=1.6, cex.label=1.6)
par(op)

#Model diagnostics
mcmc.diagnostics(DSmod4)

#Simulating Networks based on fit models
sim4 <- simulate(DSmod4, nsim=1, seed=569)
summary(sim4, print.adj=FALSE)
op <- par(mfrow=c(1,2), mar=c(0,0,2,0))
lvlobs <- TCdiss %v% 'agency_lvl'
plot(TCdiss, usearrows=FALSE, vertex.col=lvl+1, edge.lwd=0.5, edge.col="grey75",
     main="Observed TC Network")
lvl4 <- sim4 %v% 'agency_lvl'
plot(sim4, usearrows=FALSE, vertex.col=lvl4+1, edge.lwd=0.5, edge.col="grey75",
     main="Simulated Network - model 4")

##### Dynamic Network Models #####
#It applies mainly to the changes in node interactions in the network and not to the size of the 
#network
library(igraph)
library("UserNetR")
data(Coevolve)
fr_w1 <- Coevolve$fr_w1
fr_w2 <- Coevolve$fr_w2
fr_w3 <- Coevolve$fr_w3
fr_w4 <- Coevolve$fr_w4

colors <- c("darkgreen", "SkyBlue2")
shapes <- c("circle", "square")
coord <- layout.kamada.kawai(fr_w1)
op <- par(mfrow=c(2,2), mar=c(1,1,2,1))
plot(fr_w1, vertex.color=colors[V(fr_w1)$smoke+1], vertex.shape=shapes[V(fr_w1)$gender],
     vertex.size=10, main="Wave 1", vertex.label=NA, edge.arrow.size=0.5, layout=coord)
plot(fr_w2, vertex.color=colors[V(fr_w2)$smoke+1], vertex.shape=shapes[V(fr_w2)$gender],
     vertex.size=10, main="Wave 2", vertex.label=NA, edge.arrow.size=0.5, layout=coord)
plot(fr_w3, vertex.color=colors[V(fr_w3)$smoke+1], vertex.shape=shapes[V(fr_w3)$gender],
     vertex.size=10, main="Wave 3", vertex.label=NA, edge.arrow.size=0.5, layout=coord)
plot(fr_w4, vertex.color=colors[V(fr_w4)$smoke+1], vertex.shape=shapes[V(fr_w4)$gender],
     vertex.size=10, main="Wave 4", vertex.label=NA, edge.arrow.size=0.5, layout=coord)
par(op)

library(RSienaTest)
matw1 <- as.matrix(get.adjacency(fr_w1))
matw2 <- as.matrix(get.adjacency(fr_w2))
matw3 <- as.matrix(get.adjancency(fr_w3))
matw4 <- as.matrix(get.adjancency(fr_w4))
matw1[1:8, 1:8]

fr4wav <- sienaDependent(array(c(matw1, matw2, matw3, matw4), dim=c(37, 37, 4)), sparse=FALSE)
class(fr4wav)

library(Matrix)
w1 <- cbind(get.edgelist(fr_w1), 1)
w2 <- cbind(get.edgelist(fr_w2), 1)
w3 <- cbind(get.edgelist(fr_w3), 1)
w4 <- cbind(get.edgelist(fr_w4), 1)
w1s <- spMatrix(37, 37, w1[,1], w1[,2], w1[,3])
w2s <- spMatrix(37, 37, w2[,1], w2[,2], w2[,3])
w3s <- spMatrix(37, 37, w3[,1], w3[,2], w3[,3])
w4s <- spMatrix(37, 37, w4[,1], w4[,2], w4[,3])
fr4wav2 <- sienaDependent(list(w1s, w2s, w3s, w4s))
fr4wav2

gender_vect <- V(fr_w1)$gender
table(gender_vect)
gender <- coCovar(gender_vect, centered=FALSE)
gender

smoke<-array(c(V(fr_w1)$smoke,V(fr_w2)$smoke, V(fr_w3)$smoke, V(fr_w4)$smoke), dim=c(37,4))
smokebeh <- sienaDependent(smoke, type="behavior")
smokebeh

friend <-sienaDataCreate(fr4wav, smokebeh, gender)
friend
print01Report(friend,modelname= 'Coevolve Example')

effectsDocumentation(frndeff)

frndeff <- getEffects(friend)
frndeff <- includeEffects(frndeff, sameX, interaction1="gender", name="fr4wav")
frndeff <- includeEffects(frndess, egoX, interaction="smokebeh", name="fr4wav")
frndeff <- includeEffects(frndeff, altX, interaction1="smokebeh", name="fr4wav")
frndeff <- includeEffects(frndeff, sameX, interaction1="smokebeh", name="fr4wav")
frndeff <- includeEffects(frndeff, avSim, interaction="fr4wav", name="smokebeh")
frndeff <- includeEffects(frndeff,totSim, interaction="fr4wav", name="smokebeh")
frndeff <- includeEffects(frndeff,recip, interaction="fr4wav", name="smokebeh")

##Model estimation
myalgorithm <- sienaAlgorithmCreate(projname='coevolve')
set.seed(999)
RSmod1 <- siena07(myalgorithm, data=friend, effects=frndeff, batch=TRUE, verbose=FALSE,
                  useCluster=TRUE, initC=TRUE, nbrNodes=3)

###Model exploration 
#MOdel interpretation
summary(RSmod1)

frndeff2 <- includeEffects(frndeff, totSim, interaction="fr4wav", name="smokebeh",
                           include=FALSE)
frndeff2 <- includeEffects(frndeff2, transTrip, name="fr4wav", include=FALSE)
myalgorithm <- sienaAlgorithmCreate(projname='coevolve2')
set.seed(999)
RSmod2 <- siena07 (myalgorithm, data=friend, effects = frndeff2, prevAns=RSmod1, batch=TRUE,
                   verbose=FALSE, useCLuster=TRUE, initC=TRUE, nbrNodes=3, returnDeps=TRUE)
summary(RSmod2)

#Godness of fit
table(degree(fr_w4, mode="in"))
gofi <- ienaGOF(RSmod2, IndegreeDistribution, levls=1:10, verbose=FALSE, join=TRUE,
                varName="fr4wav")
plot(gofi)

TriadCensus <- function(i, data, sims, wave, groupName, varNAme, levls=1:16){
  unloadNamespace("igraph")
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  if(network.edgecount(x) <= 0) {x<- symmetrizes(x)}
tc <- sna:triad.census(x)[1,levls]
tc
  }

goftc <- sienaGOF(RSmod2, TriadCensus, varName="fr4wav", verbose=False, join"TRUE")
descriptives.sienaGOF(goftc)
plot(goftc, center=TRUE, scale=TRUE)

#Model simulations
str(RSmod2$sims([500]))

RSmod2$sims[[500]] [[1]] [[1]] [[3]] [1:25,]

RSmod2$sims[[500]] [[1]] [[2]] [[3]]

library(igraph)
el <- RSmod2$sims[[500]] [[1]] [[2]] [[3]]

library(igraph)
el <- RSmod2$sims[[500]] [[1]] [[1]] [[3]]
sb <- RSmod2$sims[[500]] [[1]] [[2]] [[3]]
fr_w4_sim <- graph.data.frame(el, directed=TRUE)
V(fr_w4_sim)$smoke <- sb
V(fr_w4_sim)$gender <- V(fr_w4)$gender
fr_w4_sim
modularity(fr_w4_sim, membership = V(fr_w4_sim)$smoke+1)
modularity(fr_w4, membership = V(fr_w4)$smoke+1)
colors <- c("darkgreen", "SkyBlue2")
coord <- layout.kamada.kawai(fr_w4)
op <- par(mfrow=c(1,2), mar=c(1,1,2,1))
plot(fr_w4, vertex.color=colors[V(fr_w4)$gender], vertex.size=10, main="Observed - Wave 4",
     vertex.label=NA, edge.arrow.size=0.5, layout=coord)
plot(fr_w4_sim, vertex.color=colors[V(vr_w4_sim)$smoke+1],
     vertex.shape=shapes[V(fr_w4_sim)$gender],
     vertex.size=10, main="Simulated - Wave 4", vertex.label=NA, edge.arrow.size=0.5,
     layout=coord)
par(op)


###Simulations
library(igraph)
N <- 25
netdum <- erdos.renyi.game(N, p=0.10)
graph.density(netdum)
mean(degree(netdum))

Bh <- runif(N,0,1)
BhCat <- cut(Bh, breaks=5, labels=FALSE)
V(netdum)$Bh <- Bh
V(netdum)$BhCat <- BhCat
table(V(netdum)$BhCat)

library(RcolorBrewer)
my_pal <- brewer.pal(5, "PiYG")
V(netdum)$color <- my_pal[V(netdum)$BhCat]
crd_save <- layout.auto(netdum)
plot(netdum, layout = crd_save)

g <- netdum
get.adjlist(g)[24]
g[[24,]]

V(g)[24]$Bh
V_adj <- unlist(get.adjlist(g)[24])
V(g)[V_adj]$Bh

BhDiff <- abs(V(g)[V_adj]$Bh - V(g)[24]$Bh)
BhDiff

gdum <- g
gdum[24,9] <- FALSE
get.adjlist(gdum)[24]

gdum <- g
V_sel <- V_adj[BhDiff == max(BhDiff)]
gdum[24,V_sel] <- FALSE
get.adjlist(gdum)[24]
plot(gdum, layout = crd_save)

gdum <- g
V_sel <- sample(V_adj,1,prob=BhDiff)
gdum[24,V_sel] <- FALSE
get.adjlist(gdum)[24]

smplCheck <- sample(V_adj, 500, replace=TRUE, prob=BhDiff)
table(smplCheck)

vtx <- 24
nodes <- 1:vcount(g)
V_nonadj <- nodes[-c(vtx,V_adj)]
V_nonadj

BhDiff2 <- 1-abs(V(g)[V_nonadj]$Bh - V(g)[vtx]$Bh)
BhDiff2

Sel_V <- sample(V_nonadj,1,prob=BhDiff2)
gnew <- g
gnew[vtx,Sel_V] <- TRUE
get.adjlist(gnew)[vtx]

E(gnew)$color <- "grey"
E(gnew, P = c(vtx, Sel_V))$color <- "darkred"
plot(gnew, layout = crd_save)

Sel_update <- function(g,vtx){
  V_adj <- neighbors(g,vtx)
  if(length(V_adj)==0) return(g)
  BhDiff1 <- abs(V(g)[V_adj]$Bh - V(g)[vtx]$Bh)
  Sel_V <- sample(V_adj,1,prob=BhDiff1)
  g[vtx,Sel_V] <- FALSE
  nodes <- 1: vcount(g)
  V_nonadj <- nodes[-c(vtx, V_adj)]
  BhDiff2 <- 1-abs(V(g)[V_nonadj]$Bh - V(g)[vtx]$Bh)
  Sel_V <- sample(V_nonadj, 1, prob=BhDiff2)
  g[vtx,Sel_V] <- TRUE
  g
}

gtst <- g
node <- 24
gnew <- Sel_update
}





