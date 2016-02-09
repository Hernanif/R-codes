#R graphs

qgraph(cor(object)) #makes a correlation between the interactions and nodes in the network





#Tutorial
###############################################################
###############################################################
####
####            PART I: NETWORKS
####
###############################################################
###############################################################
# the <span class="nx4spu57oyp" id="nx4spu57oyp_3">igraph</span> library is good for network based analyses
library(igraph)
 
# First lets build a network using basic formulas:
 
graph.onelink<-graph.formula(A-+B)
 
# This gives us a two species network (A and B) with one link (represented by A-+B). With this function the (+) sign signifies the "arrowhead".
# We can visualize our simple 2 species network with plot.igraph().
 
plot.igraph(graph.onelink)
 
# Using graph.formula() we can create any graph we want, as long as we are willing to write out every interaction by hand. Here is another simple example, a four species food chain:
 
graph.foodchain<-graph.formula(A-+C,B-+C,C-+D)
 
# and plot it:
 
plot.igraph(graph.foodchain)
#A and B are eaten by C while D eats C
 
#igraph has a function for generating random networks of varying size and connectance.
 
graph.random.gnp<-erdos.renyi.game(n=20,p.or.m=.5,type="gnp",directed=T)
plot.igraph(graph.random.gnp)
 
# We can also <span class="nx4spu57oyp" id="nx4spu57oyp_1">change</span> the layout of the graph, here we will plot the nodes in a circle
plot.igraph(graph.random.gnp,layout=layout.circle)
 
# Here we have created a random directed graph with 20 species ("n") and a connectance ("p") of 0.5 (that is any two nodes have a 50% probability of being connected).
# By setting "type='gnp'" we tell the function to assign links with the probability "p" that we specify.
# Similarly we can set the number of links that we want in the system to a value "m" that we specify.
 
graph.random.gnm<-erdos.renyi.game(n=20,p.or.m=100,type="gnm",directed=T)
plot.igraph(graph.random.gnm)
 
# Here the number of links in the network is set to 100, and they are assigned uniformly randomly
 
# Rather than being truly random, many real networks exhibit some type of organization. Of particular note is the prevalance of scale-free networks. A scale free network is one whose degree distribution is such that a majority of nodes have relatively few links, while few nodes have many links (following a power law).
# To model scale free networks Barabasi and Albert developed the preferential attachment model in 1999. In this model new nodes are more likely to link to nodes with a higher number of links.
 
# In igraph we can use the barabasi.game() function:
graph.barabasi.1<-barabasi.game(n=50,power=1)
 
# For this graph I will introduce some new plotting tools to specify layout and vertex/edge properties.
 
plot.igraph(graph.barabasi.1,
layout=layout.fruchterman.reingold,
vertex.size=10,         # sets size of the vertex, default is 15
vertex.label.cex=.5,    # size of the vertex label
edge.arrow.size=.5        # sets size of the arrow at the end of the edge
)
 
# there are a number of different plotting parameters see
#?igraph.plotting
#for details
 
plot.igraph(graph.barabasi.1,
layout=layout.fruchterman.reingold,
vertex.size=10,
vertex.label.cex=.5,
edge.arrow.size=.5,
mark.groups=list(c(1,7,4,13,10,16,15,41,42,29),
c(2,48,5,36,43,33,9)), # draws polygon around nodes
mark.col=c("green","blue")
)


# In the above plot a green and blue polygon are used to highlight nodes 1 and 2 as hubs. The "mark.groups" argument allows you to draw a polygon of specified color ("mark.col") around the nodes you specify in a list. Because barabasi.game() will give you a different graph each time, the groups I have recorded will not be the same each time.
 
# We can use a community detection algorithm to determine the most densely connected nodes in a graph.
 
barabasi.community<-walktrap.community(graph.barabasi.1)
 
# This algorithm uses <span class="nx4spu57oyp" id="nx4spu57oyp_11">random walks</span> to find the most densely connected subgraphs.
 
members<-membership(barabasi.community)
# The members() function picks out the membership vector (list of nodes in the most densely connected subgraph) from the communtiy object (e.g., walktrap community).
 
par(mar=c(.1,.1,.1,.1))    # sets the edges of the plotting area
plot.igraph(graph.barabasi.1,
layout=layout.fruchterman.reingold,
vertex.size=10,
vertex.label.cex=.5,
edge.arrow.size=.5,
mark.groups=list(members),
mark.col="green"
)

# With the above plot the group with the green polygon surrounding it is the nodes listed as being a part of the walktrap community.
 
# Now we will play around with the "power" argument to see how that impacts the graphs.
# We will generate 4 networks with preferential attachment at varying levels.
barabasi.game.2<-barabasi.game(n=50,power=.75)
barabasi.game.3<-barabasi.game(n=50,power=.5)
barabasi.game.4<-barabasi.game(n=50,power=.25)
barabasi.game.5<-barabasi.game(n=50,power=0)
 
# These can be organized into a list for convenience.
barabasi.graphs<-list(barabasi.game.2,barabasi.game.3,barabasi.game.4,barabasi.game.5)
 
# Now lets use community detection, this time with the walktrap algorithm.
bg.community.list<-lapply(barabasi.graphs,walktrap.community)
bg.membership.list<-lapply(bg.community.list,membership)
 
txt<-c("a","b","c","d")    # vector for labeling the graphs
 
# Plot these four graphs in one window with:
par(mfrow=c(2,2),mar=c(.2,.2,.2,.2))
# The for loop here plots each graph in the list one by one into the window prepared by par.
for(i in 1:4){
plot.igraph(barabasi.graphs[[i]],
layout=layout.fruchterman.reingold,
vertex.size=10,
vertex.label.cex=.5,
edge.arrow.size=.5,
mark.groups=list(bg.membership.list[[i]]),
mark.col="green",
frame=T # the frame argument plots a box around the graph
)
text(1,1,txt[i]) # calls from the vector to label the graph, adds to the                          # graph that was last plotted
}
 
# Later we will look at the properties of these graphs to see exactly how they are different


# With the above plot the group with the green polygon surrounding it is the nodes listed as being a part of the walktrap community.
 
# Now we will play around with the "power" argument to see how that impacts the graphs.
# We will generate 4 networks with preferential attachment at varying levels.
barabasi.game.2<-barabasi.game(n=50,power=.75)
barabasi.game.3<-barabasi.game(n=50,power=.5)
barabasi.game.4<-barabasi.game(n=50,power=.25)
barabasi.game.5<-barabasi.game(n=50,power=0)
 
# These can be organized into a list for convenience.
barabasi.graphs<-list(barabasi.game.2,barabasi.game.3,barabasi.game.4,barabasi.game.5)
 
# Now lets use community detection, this time with the walktrap algorithm.
bg.community.list<-lapply(barabasi.graphs,walktrap.community)
bg.membership.list<-lapply(bg.community.list,membership)
 
txt<-c("a","b","c","d")    # vector for labeling the graphs
 
# Plot these four graphs in one window with:
par(mfrow=c(2,2),mar=c(.2,.2,.2,.2))
# The for loop here plots each graph in the list one by one into the window prepared by par.
for(i in 1:4){
plot.igraph(barabasi.graphs[[i]],
layout=layout.fruchterman.reingold,
vertex.size=10,
vertex.label.cex=.5,
edge.arrow.size=.5,
mark.groups=list(bg.membership.list[[i]]),
mark.col="green",
frame=T # the frame argument plots a box around the graph
)
text(1,1,txt[i]) # calls from the vector to label the graph, adds to the                          # graph that was last plotted
}
 
# Later we will look at the properties of these graphs to see exactly how they are different

