#Phylogenies in Ecology
#Script written by: Hernani Oliveira from the book Phylogenies in Ecology
#Date: 12/05/2017
#E-mail: oliveiradebioh@gmail.com

library(ape)
library(picante)
library(geiger)
library(pez)
library(seqinr)
library(treebase)

#Reading and plotting a tree from a dataset in a website
s <- "owls(((Strix_aluco:4.2,Asio_otus:4.2):3.1,Athene_noctua:7.3):6.3,Tyto_alba:13.5);"
cat(s, file = "ex.tre", sep = "\n")
tree.owls <- read.tree("ex.tre")
tree.owls
plot(tree.owls)

#Explores the object pylo
class(tree.owls)
attributes(tree.owls)
tree.owls$edge

#PLotting the trees
par(mfrow=c(1,2))
plot(tree.owls,  edge.width=3, label.offset=0.3)
add.scale.bar()
text(1,1.1,"MY")

owl.tmp<-tree.owls
owl.tmp$tip.label<-c(1:4)

plot(owl.tmp, edge.width=3, show.node.label=TRUE, label.offset=0.3)
add.scale.bar()
text(1,1.1, "MY")


#For lagre trees, it's important to remember the numbering ndoe rules
##NUmber 1 is the bottom tip and number n is the topmost tip, corresponding to n species
## n+1 is for the root. For all subsequent splits, the next node is the bottom one.

#Code for creating a tree with 6 tips . It needs to be carefully done to ensure that edges and lengths line up correctly
tr1<- read.tree(text="(((t2:0.1610994965, t4:0.1610994965): 0.2636854169, 
      t5:0.4247849134):0.5752150866, (t1: 0.8616120011,
      (t6: 0.5784472598, t3: 0.5784472598): 0.2831647414): 0.1383879989);")
plot(tr1, show.tip.label=FALSE, edge.width=3)
tr1$edge

tree.owls$tip.label
tree.owls$edge.length

#Useful functios form the ape package
#Function          #Main inputs and arguments        Description
#chronos           phylo object, lambda value        This performs rate smoothing to create an ultrametric tree
#cophenetic.phylo  phylo object                      This returns all pairwise distances between species. Just use copehentic(); the .'phylo' is implied from the object type
#drop.tip          phylo object, vector of tip       This creates a new tree without tips that are to be excluded
#                  labels to be dropped      
#mrca              phylo object                      this plots the phylogeny and creates a phylo object                      
#multi2di          phylo object                      this function resolves polytomies
#pic               phylo object, vector of trait     this performs phylogenetically independent contrats, useful for analysis where phylogenetic relationships need to be controlled for 
#                  values
#plot.phylo        phylo object                      this plots the phylogeny. Just use plot(); the '.phylo' is implied from the object type 
#read.tree         file or text                      this reads in a phylogeny and creates a phylo object
#rcoal             number of tips                    this returns a random coalescence tree, which is ultrametric
#root              Phylo object, outgroup            this roots and reroots a tree
#rtree             Number of tips                    this returns a random tree, which is not ultrametric
#vcv.phylo         Phylo object                      this returns the variance-covariance matrix representing species relatedness. Just use vcv(); the '.phylo' is implied from the object type
#write.tree        Phylo object, file                this writes a Newick file of the tree to the specified location


#This returns a list object with three elemtns (the names starting with $) that includes the phylogeny, community matrix, and trait matrix described
library(picante)
data(phylocom)
phylocom

#Take a look at what the object contains. It shows that there are three named elements
attributes(phylocom)

#Pulls out the attributes of the object
comm<-phylocom$sample
phy<-phylocom$phylo
traits<-phylocom$traits
comm
traits
phy

#Calculates the phylogenetic distance - It enables to calculate PD using community data and the phylogeny directly
pd(comm, phy)

#To calculate mean pariwise distance (MPD) and mean nearest taxon distance (MNTD)
#require the species pairwise phylogenetic distance matrix, rather than the phylogeny
#This is the function to calculate MPD. 
mpd(comm, cophenetic(phy))

#It returns the mean pairwise distance, one for each sample. It returns a large matrix with the pairwise phylogenetic distances between all pair of species
cophenetic(phy)

#####Useful functions contained in picante, the main inputs and arguments that need to be specified, and a brief description of their use
#Function                  Main input and arguments            Description
#comdist                   community matrix, phylogenetic      This returns average pairwise disances between community pairs
#comdistnt                 distance matrix using cophenetic
#evol.distinct             Phylo object                        This returns a measure of species evolutionary distinctiveness
#mpd, mntd                 Community matrix, phylogenetic      These functions return a vector of mean pairwise or nearest taxon distances within communities
#                          distance matrix using cophenetic()
#pd                        Community matrix, Phylo object      This returns community phylogenetic diversity values
#Phylosignal               A vector of trait values, Phylo     This returns Blomberg's K and PIC values, both measures of phylogenetic signal. Note that trait vector needs to be in the same order as the tip labels 
#                          object
#ses.mpd,                  Community matrix, phylogenetic      These functions use randomizations to calculate standardized effect sizes and significance of mean pairwise, or nearest taxon distances within communities
#ses.mntd                  distance matrix using copehenetic()
#ses.pd                    COmmunity matrix, Phylo object      This function uses randomizations to calculate standardized effect sizes and significance of community phylogenetic diversity values



##### Finding and adapting available tree
#In many cases for available taxa, there are already existing phylogenies built
Iridaceae <- search_treebase('"Watsonia', by="taxon", max_trees=4) 
summary(Iridaceae)
plot(Iridaceae[[1]])

#Search for phylogenies based on the authors of the phylogeny
Goldblatt<-search_treebase("Goldblatt", by="author", exact_match=T)[[1]]

#Query by study unique identifier
study.ID <- search.treebase("325", by="id.study")

#Plotting Non-ultrametric, ultrametric and uninformative trees
tr1<-rtree(12)
tr2<-chronos(tr1)
tr3<-tr2
par(mfrow=c(1,3))
plot(tr1, edge.width=2, cex=2, main="A) Non-ultrametric", cex.main=2)
plot(tr2, edge.width=2, cex=2, main="B) Ultrametric", cex.main=2)
plot(tr3, edge.width=2, cex=2, main="C) Uninformative", cex.main=2)

#Ultrametric rate smoothing 
#using a mean path length (MPL) method that assumes random mutation 
#and a fixed molecular clock. It calculates the age of a node as the 
#mean of all the distances from this node to all tips descending from it.
#Uses the function chronoMPL() - this method is fast and easy to use, , but 
#has an unfortunate habit of returning some negative branch lengths. Thus, it should be used with caution
#The other function used is chronos, which uses the penalized maximum likelihood
#method to estimate divergence times developed by Sanderson
tr.ran<-rtree(7)
tr.ran.u<-chronoMPL(tr.ran)
tr.ran.rs<-chronos(tr.ran)
par(mfrow=c(1,3))
plot(tr.ran, edge.width=2, cex=2, main="A) Non-ultrametric", cex.man=2)
plot(tr.ran.u, edge.width=2, cex=2, main="B) ChronoMPL", cex.main=2)
plot(tr.ran.rs, edge.width=2, cex=2, main="C) Chronos", cex.main=2)














#Take a look at the parameters - returns a lot of values not included in the trees
#ploglik is the penalized log likelihood value we will use to select the optimal lambda value
#rates gives the rate transformation for each edge in the original phylogeny and is the same length and order as the vector of edge lengths
#PHIIC$Llambda gives the lambda value the rates were calculated with
attributes(tr.ran.rs)

#Finds the optimal lambda value using a for loop, but first we need to specify a gradient of lambda values to test
l<-c(0:10000)
LL.out<-NULL
for (i in 1:length(l)){
  LL.out[i]<-attributes(chronos(tr.ran, lambda=l[i]))$ploglik
}

#Now the container is holding 10,001 penalized log likelihood values 
#and we can select the maximum penalized log likelihood value and
#then the corresponding lambda
l[LL.out==max(LL.out)]

#Visualizes the likelihood curve
plot(log(l+1), LL.out, type="l", lwd=3, cool="gray", xlab=expression(paste
            ("Log(", lambda,")")), ylab="Log likelihood")


#Edge length and rate transformations
#Even if we have a well-supported ultrametric tree, we may wish to alter edge lengths for a number of reasons.
#First, we may want to change edge distance, say from molecular distance to time.
#Alternatively, we may wish to alter edge lengths according to some specific
#model of evolutionary change. For example, we can assess whether early or recent 
#evolutionary change disproportionately explains our ecological pattern
#of interest by transforming tree edge lengths using Pagel's lambda.
# Pagel's lambda transforms edge lengths by raising the edge depths to the power
#of lambda. When lambda is less than 1, edges near the tips of a tree become shorter
#which can be interpreted as evolutionary change being concentrated early in the 
#evolution of the clade.
#Conversely,  lambda greater than 1, edges near



#Uses the rescale function using thepackage geiger
#Even if there is a well supported tree, we may wish to change edge lengths
#for a number of reasons. First, we may want to change edge distance, say from
#molecular distance to time. Alternatively, we may wish to alter edge lengths
# according to some specific model of evolutionary change. For example,
#we can assess whether early or recent evolutionary change disproportionately
# explains our ecological pattern of interest by transforming treee edge lengths
#using Pagel's lambda
#Pagel's lambda transforms edge lengths by raising the edge depths to the power
#of lamdba. When lambda is less than 1, edges near the tips of a tree become shorter,
# which can be interpreted as evolutionary change being concentrated early in the 
#evolution of the clade. COnversely, lambda greater than 1, results in stretching
#of the edges near the tips, and can be seen asmore recent evolutionary change.
#A lambda of 1 is analagous to Brownian motion evolution, and just returns the
#original tree.
#The small lambda values compresses the terminal edges and stretches the branches
#deeper in the tree. When we use a large lambda value, the terminal edges are stretched
#corresponding to high character divergence among closely related taxa
library(geiger)
tr<-rcoal(25)
tr.DO.1 <- rescale(tr, "delta", 0.1)
tr.D1 <- rescale(tr, "delta",1)
tr.D10 <- rescale(tr, "delta", 10)
par(mfrow=c(1,3), cex.main=2)
plot(tr.DO.1, show.tip.label=FALSE, edge.width=2, 
     main=expression(paste("A)",Delta," =0.1")))
plot(tr.D1, show.tip.label=FALSE, edge.width=2, 
     main=expression(paste("B)",Delta," = 1")))
plot(tr.D10, show.tip.label=FALSE, edge.width=2, 
     main=expression(paste("C)",Delta," = 10")))

##Transforms trees using Pagel's K
#TO assess whether different rates of evolution are associated with branching,
#we can transform trees using Pagel's K. Pagel's K raises edge lengths to the 
#power of K. When K is less than 1, longer edges are shortened more than shorter
#ones, so that clades with many lineages appear to have more evolutionary change
#than less diverse ones. A value of K=0 sets all branch lengths equal, and is 
#equivalent to a speciational model of evolution. A K greater than 1 stretches
#longer edges more, and so clades with fewer lineages undergo relatively more
#evolutionary change. A value of K=1 returns the original tree. Again, we will
#use the rescale() function in geiger to do this transformation
#The small K thus suggests higher rates of evolution within the most diverse clade
#and a larger K suggests higher rates in the less diverse clade.
tr.KO.1<-rescale(tr, "kappa", 0.1)
tr.K1<-rescale(tr, "kappa", 1)
tr.K2<-rescale(tr, "kappa", 2)
par(mfrow=c(1,3), cex.main=2)
plot(tr.KO.1, show.tip.label=FALSE, edge.width=2,
     main=expression(paste("A) ", Kappa, "= 0.1")))
plot(tr.K1, show.tip.label=FALSE, edge.width=2,
     main=expression(paste("B) ", Kappa, " = 1")))
plot(tr.K2, show.tip.label=TRUE, edge.width=2, main=expression(paste("C) ",
      Kappa," =2")))

#Removes the phylogenetic structure of the tree
#At an extreme, a complete lack of phylogenetic structure can be represented
#by a "star" phylogeny, where all species descend from the root node and are thus 
#all equally related to one another. Pagel's lambda transforms our phylogeny
#by reducing the overall contribution of internal edges relative to the terminal
#edges. The lambda value is scaled from 0 to 1 (it is possible to obtain values
#of lambda greater than one, but these have no clear evolutionary interpretation),
#with removing all phylogenetic structure and 1 returning our original phylogeny
#lambda=0 returns a star phylogeny. A lambda of 0.5 gives us a phylogeny where the
#relative importance of the phylogenetic structure is reduced by increasing
#the amount of species independent evolution relative to the edges that 
#represent patterns of relatedness. Finally lambda equal to 1 just returns our
#original phylogeny, which was produced ny another rcoal()
tr.LO<- rescale(tr, "lambda", 0)
tr.LO5<-rescale(tr, "lambda", 0.5)
tr.L1<-rescale(tr, "lambda", 1)
par(mfrow=c(1,3), cex.main=2)
plot(tr.LO, show.tip.label=FALSE, edge.width=2,
     main=expression(paste("A) ", lambda," =0")))
plot(tr.LO5, show.tip.label=FALSE, edge.width=2, main=expression(paste("B)" , 
    lambda, "= 0.5")))
plot(tr.L1, show.tip.label=FALSE, edge.width=2, main=expression(paste("C) ", 
      lambda, "= 1")))


###Phylogenetic patterns within communities
tr<-rcoal(5) #creates a random tree with 5 edges. Rcoal returns an ultrametric 
#coalescent tree, whereas the alternative rtree() returns a nonultrametric tree
tre<-rtree(5)
tre$edge.length #return edge lengths
tr$edge.length
plot(tr)
plot(tr)
cophenetic(tre) #get pairwise differences between edge lengths

#Calculates Faith's phylogenetic diversity
#One of the phylogenetic measures with the longest history is Faith's phylogenetic
#diversity or Faith's PD. There is some confusion about how to employ this measure
#when moving from a regional pool to local habitats, with some studies keeping the root
#node of the regional hylogeny in any assemblage subtree, even if the taxa do not
#traverse the root; other studies calculate PD using only the assemblage taxa.
#Within a single assemblage we will define PD as the total amount of evolutionary 
#history represented by a group of extant species excluding the root. To calculate it,
#we simply sum all the edge lengths from the vector of edge lengths as:
sum(tr$edge.length) #PD varies depending on the distribution of node heigths within an ultrametric tree. A star phylogeny where all species are equally related and originate from the root node, has the greatest PD value. If we were to scale the edges so that the maximal, or root to tip value is 1, then PD equals species richness


#Calculates PD for five communities
data(phylocom)
pd(phylocom$sample, phylocom$phylo, include.root=FALSE)
cophenetic(tr)

#Two measures to derive pairwise distance matrix
#Mean pairwise distance (also referred to as MPD) and mean nearest taxon distance (MNTD)
#MNTD is also referred to as MNND - mean nearest neighbor distance. MPD is simply 
#calculated as the mean of the nondiagonal elements in the pairwise distance matrix
dist.tr<-cophenetic(tr) #creates a distance matrix using the object dist.tr and the cophenetic function
dist.tr<-dist.tr[lower.tri(dist.tr, diag=FALSE)] #subsets the matrix using the lower.tri() function, with the argument diag=FALSE excluding the diagonal elements (i.e., the zeros)
mean(dist.tr) #calculates MPD by taking the mean of these values

#It is also possible to do the same calculation using the function mpd() in picante, but we need to create a community matrix
a<-matrix(c(1, 1, 1, 1, 1), nrow=1, dimnames=list("s1", c("t1", "t2",
          "t3", "t4", "t5"))) #creates a community matrix
mpd(a, cophenetic(tr)) #calculates mean pairwise distance

#Calculates the MNTD from the distance matrix
b<-cophenetic(tr)
diag(b) <- NA
apply(b, margin = 2, min, na.rm=TRUE)
mean(apply(b,2,min,na.rm=TRUE)) #returns the minimum distances foreach species in the assemblage
mntd(a, cophenetic(tr)) #uses the function from picante to get the same values 

#The real benefit of the pd (), mpd() and mntd() functions in picante is that they calculate their respective measures across multiple sites
#Uses Jasper Ridge plant community data to examine these metrics
j.tree<-read.tree(".../jasper_tree.phy")
j.com<-read.csv(".../jasper_data.csv", row.names=1)
pd.out<-pd(j.com, j.tree, include.root=FALSE)
head(pd.out)
plot(pd.out$SR, pd.out$PD, xlab="Species richness", ylab="PD", pch=16)

#Examines how MPD and MNTD are related to richness, but mpd() and mntd() return vectors, and not a data frame as with pd()
#We can simply add the vectors as columns to the object pd.out from above
#We can add MPD and MNTD as newcolumns because the input data is in the same order
#(but there are ways to ensuyre data are ordered the same by using the match() function)
#Be careful if you sort the community matrix between calls to the functions, because
#then the values may not be in the correct order
pd.out[,3] <- mpd(j.com, cophenetic(j.tree))
pd.out[,4] <- mntd(j.com, cophenetic(j.tree))
names(pd.out)[3:4] <- c("MPD", "MNTD")
par(mfrow=c(1,2))
plot(pd.out$SR, pd.out$MPD, xlab="Species richness",
     ylab="MPD (millions of years)", pch=16)
plot(pd.out$SR, pd.out$MNTD, xlab="Species richness",
     ylab="MNTD (millions of years)", pch=16)


#To test wether communities are phylogenetically clustered or overdispersed, we can compare actual PD, MPD and MNTD values to those expected by chance
#To do so we use a null model where we randomize the assemblages or the evolutionary relationships among species
#There are a number of ways to create and use null assemblages. Here we use one of the simplest null models,
#randomly shuffling the tip labels on the phylogeny. thus when we recalculate our metrics for the communities, the phylogenetic distances 
#will not correspond to the actual distances separating species, but the overall structure of the phylogenetic topology is retained.
#The random shuffling of tip labels is done with this:
sample(tr$tip.label, length(tr$tip.label), replace=FALSE)

#We can then put this in a for() loop or apply () function (the specific form of the apply() function to do this is the function replicate()
#and create a reabeled phylogeny and calculate our metric, say, 5,000 times.
#We can then standardize the metrics by calculating an effect size against the null
#values using a z-value. The z-value is a dimensionless value that indicates the number
#of standard deviations an observation is from the mean of the distribution)

#The function to standardize PD, MPD and MNTD in picante (ses.pd(), ses.mpd(), and ses.mntd())
#are fairly straightforward and return a wealth of information (note that the package pez also calculates these).
#Using the phylocom example data in picante we can lok at how the different assemblages
#in figure 3.5 are reflected in the standardized MPD and MNTD
ses.mpd(phylocom$sample, cophenetic(phylocom$phylo), runs= 999)
ses.mntd(phylocom$sample, cophenetic(phylocom$phylo), runs= 999)

#The output reports the number of species, the observed MPD value, and the mean
# and standard deviation of MPD from the null randomization. It also reports the observed rank,
#which is where the observed value is located when the null values are ordered
#from smallest to largest. Thus, a rank of 1 means that the observed MPD value 
#is smaller than all the null values. The z-value is reported, the sign indicates 
#clustering (negative) or overdispersion (positive), and the magnitude is the number
#of standard deviations theobservation is from the mean. The larger the absolute value,
#the more likely it is that the observed value is significantly different than the null
#expectation. The reported P-value is calculated as: P= mpd.obs.ranl/(runs+1),
#and is a two tailed test; thus, significance at the treshold alfa=0.05 level is 
#achieved when P is less or equal to 0.025 or P is more or equal to 0.975.

#Given the significance criterion, the clumped and even assemblages are significantly different
#than the null expectation. The only difference between MPD and MNTD is "clump4"
#which includes species sister-pairs dispersed through the phylogeny. This assemblage
#has small MNTD values because each species has a close relative in the assemblage, but the MPD
#is no different from the null expectation because the distance matrix contains both very large
#and very small distances and the average of these is roughly equal to the overall average edge length.
#Thus, comparing MNTD and MPD can be valuable, and can tell us something about the phylogenetic depth that best corresponds to nonrandom patterns. In general,
#MNTD tekk us more about patterns near the tips whereas MPD is more influenced by phyogenetic distances deeper in the phylogeny

#The assemblages in this example's data are selected to maximize clustering and overdispersion, and the
#large z-values reflect this. With real data, interpreting the values may require a litle more sublety.
#Here we will go through the ses.mpd() results using the Jasper Ridge pot data example
smpd.out <- ses.mpd(j.com, cophenetic(j.tree), runs=999)


#According to the output, only j110 is significantly clustered (<0.025) and only J11 is overdispersed
# (P<0.975). These plots could be unique in some way (e.g., recent disturbance, higher elevation, etc.), 
#but we normally expect that one in twenty will have a significant value of random chance.
# Two significant findings out of thirty does not instill confidence in a general conclusion. Still,
#we could use the information in smpd.out to perform additional analysis.
#Let's create a hypothetical treatment for these plots, with half the plots
#on a dry slope and half on a moist slope. To make this interesting, let's assign the plots to these treatments
#nonramdnly, with negative z-values belonging to "dry" sites and positive z-values to "moist"
#sites. We can add a column to smp.out for these treatment designations, and make them a factor
smpd.out$treat <- NA
smpd.out$treat[smpd.out$mpd.obs.z<0]<-"Dry"
smpd.out$treat[smpd.out$mpd.obs.z>0]<-"Wet"
smpd.out$treat <- factor(smpd.out$treat)

#Now let's plot the two treatments and then run standard statistical analysis
#to see if they are significantly different from each other
mod <- aov(smpd.out$mpd.obs.z~smpd.out$treat)
summary(mod)

### Calculating community diversity metrics
#Calculates the average PD
pd.out <- pd.out$PD / pd.out$SR

library(pez)
j.pez <- comparative.comm(j.tree, as.matrix(j.com))
out <- pez.evenness(j.pez)
pd.out$PAE <- out$pae
pd.out$IAC <- out$iac
pd.out$MPDab <- mpd(j.com, cophenetic(j.tree), abundance.weighted=TRUE)
pd.out$MNTDab <- mntd(j.com, cophenetic(j.tree), abundance.weighted=TRUE)

pureD<-function(samp, dis){
  N <- dim(samp)[1]
  out <- numeric(N)
  for(i in 1:N){
    sppInSample <- names(samp[i, samp[i, ] >0])
    if(length(sppInSample) > 1){
      sample.dis <-dis[sppInSample]
      diag(sample.dis) <- NA
      out[i] <- sum(apply(sample.dis, 2, min, na.rm=TRUE))
    }
    else {
      out[i] <- NA
    }
  }
  out
}

pd.out$pureD <- pureD(j.com, cophenetic(j.tree))

#Create a function for intensive and extensive quadratic entropies 
quad.ent <- function(samp, dis, type = c("int", "ext")){
  N <- dim(samp)[1]
  out<- numeric(N)
  for(i in 1:N) {
    sppInSample <- names(samp[i, samp[i,] >0])
    if(length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample], sppInSample]
      sample.dis <- sample.dis[lower.tri(sample.dis)]
      if(type == "int") {
        out[i] <- sum(sample.dis)/length(sppInSample)^2
      }
      if (type == 'ext') {
        out[i] <- sum(sample.dis)
      }
    }
    else {
      out[i] <- NA
    }
  }
  out
}

pd.out$Intensive <- quad.ent(j.com, cophenetic(j.tree), "int")
pd.out$Extensive <- quad.ent(j.com, cophenetic(j.tree), "ext")

outD <- raoD(j.com, j.tree)
pd.out$Rao <- outD$Dkk

#Calculating phylogenetic species variability, phylogenetic specis richness and phylogenetic species evenness. #Phylogenetic species variability is a measure of how much species should vary from one another on the phylogeny. PSR simply multiplies PSV by the number of species. When interpreting this metric, it might be helpful to remember that species richness is just maximal relatedness possible (i.e. all species equally distantly related) when a phylogenetic tree is scaled with root to tip distance of 1; thus, the smaller the value of PSR, the more close relatives there are in the phylogeny. In essence, PSE creates polytomies below the species level, each individual represented by as single branch of length 0, thus inflating the covariance matrix. Phylogenetic species evenness (PSE) scales PSV by abundances
pd.out$PSV <- psv(j.com, j.tree)[,1]
pd.out$PSR <- psr(j.com, j.tree)[,1]
pd.out$PSE <- pse(j.com, j.tree)[,1]

#Shows the relationship between PSV and MPD
plot(pd.out$MPD, pd.out$PSV, xlab="MPD", ylab="PSV", pch=16)

#Calculates the diversity measures based on ED values
dDP <- function(phy, com, q=2){
  ed.list <- evol.distinct(phy)
  out <- numeric(nrow(con))
  
  for (i in 1:nrow(com)) {
    ed.tmp <- ed.list[match(colnames(j.com[i, j.com[i,]>0]), 
                            as.character(ed.list$Species)),]
    l<-ed.tmp$w/sum(ed.tmp$w)
    out[i] <- sum(l^q) ^ (1/1-q))
  }
  out
}

pd.out$dDP <- dDP(j.tree, j.com, q=2) 

#One important issue is that choosing small q values (e.g. -> 1) give higher weight to rarespecies and larger values (e.g., -> 3) give higher weight to common species. Different values of q correspond to different diversity metrics (e.g., )
out.s <- pez.shape(j.pez)
pd.out$Hed <- out.s$Hed
pd.out$Eed <- out.s$Eed
pd.out$Haed <- out$Haed
pd.out$Eaed <- out$Eaed

library(vegan)
pd.mds <- metaMDS(pd.out, trace=FALSE)
ordiplot(pd.mds, type= "t", display= "species")

### Null models and hypothesis testing
#Examine a nonphylogenetic example to establish the logic of a randomization test
dat <- data.frame(x = rnorm(20, 15))
dat$y <- dat$x + rnorm(20, 0.5, 0.5)
plot(dat$x, dat$y, xlab="X", ylab="Y", pch=16)
abline(lm(dat$y~dat$x))
mod <- lm(dat$y~dat$x)
summary(mod)

coef.obs <- mod$coefficients[2]
lm.rand <- function (x,y) {
  r.coef <- lm(y~sample(x, length(x)))$coefficients[2]
  r.coef
}
lm.rand(dat$x, dat$y) #returns the coefficient from a linear model with a randomized x variable

coef.rand.test <- function(x, y, N=999) {
  coef.obs <- lm(y~x)$coefficients[2]
  coef.null <- replicate(N, lm.rand(x,y))
  coef.obs.rank <- rank(c(coef.obs, coef.null))[1]
  coef.obs.p <- coef.obs.rank/ (N+1)
  if (coef.obs.p > 0.5) coef.obs.p <- (1-coef.obs.p)*2
  else coef.obs.p <- coef.obs.p*2
  data.frame(coef.obs, mean.coef.null = mean(coef.null),
             sd.coef.null = sd(coef.null),
             coef.obs.rank, coef.obs.p)
}
coef.rand.test(dat$x, dat$y)


#One issue to note is that randomization tests do have an important assumption; that is, if there are multple treatments or groupings being randomized, they should come from the same population. For example, if we were examining patterns, we wouldn't want to randomimze community data from Borneo and Minnesota in the same analysis.

#Example on how to randomize the data
com.dat <- matrix(c(10, 0, 0, 0, 5, 0, 0, 3, 4, 0, 0, 9, 0, 0, 8, 6, 0, 0, 7, 0, 9, 0, 0, 0, 5, 3, 2, 2, 1, 2), nrow=5, byrow=TRUE)
rownames(com.dat) <- c("Com1", "Com2", "Com3", "Com4", "Com5")
colnames(com.dat) <- c("Sp1", "Sp2", "Sp3", "Sp4", "Sp5", "Sp6")
length(x[x>0])

#Calculates species richnnes for each community
apply(com.dat, MARGIN=1, function(x) length(x[x>0]))

#Calculates the number of communities occupied by the different species
apply(com.dat, 2, function(x) length(x[x>0]))

#Calculates community abundance
apply(com.dat, 1, sum)

#Calculates species abundances
apply(com.dat, 2, sum)

com.tmp <- matrix(sample(com.dat), nrow=5)

#This command uses the apply function on the randomized matrix, and using the max() function to return the maximal richness 999 times
out <- NULL
for (i in 1:999) {
  out[i] <- max(apply(matrix(sample(com.dat), nrow=5), 1, function(x) length(x[x>0])))
}

#A different way to calculate significance by comparing the observed value directly to the randomized value
max.obs <- max(apply(com.dat, 1, function(x) length(x[x>0])))
obs.rank <- rank(c(max.obs, out))[1]
P_value <- obs.rank/(length(out)+1)

mean(out)
quantile(out, c(0.025, 0.975))

out.p <- NULL
for(j in 1:1000){
  out <- NULL
  for(i in 1:999){
    out[i] <- max(apply(matrix(sample(com.dat), nrow=5), 1, function(x) length(x[x>0])))
  }
  obs.rank <- rank(c(max.obs, out))[1]
  coef.obs.p <- obs.rank/(length(out) +1)
  if (coef.obs.p > 0.5) out.p[j] <- (1-coef.obs.p)*2
  else out.p[j] <- coef.obs.p*2
}

hist(out.p, xlab="Ramdomized P-values", main=NULL)
abline(v=0.05, lty="dashed", col="orange", lwd=2)

#The picante package already has a randomized function
randomizeMatrix(com.dat, null.model="frequency")
randomizeMatrix(com.dat, null.model="richness")
randomizeMatrix(com.dat, null.model="independentswap")
randomizeMatrix(com.dat, null.model="trialswap")

#Randomized phylogenetic data
data(phylocom)
out.mpd <- ses.mpd(phylocom$sample, cophenetic(phylocom$phylo), runs=999)
out.mpd[3,]
out.mpd.f <- ses.mpd(phylocom$sample, cophenetic(phylocom$phylo), runs=999, null.model="frequency")
out.mpd.r <- ses.mpd(phylocom$sample, cophenetic(phylocom$phylo), runs=999, null.model="richness")
out.mpd.i <- ses.mpd(phylocom$sample, cophenetic(phylocom$phylo), runs=999, null.model="independentswap")
out.mpd.t <- ses.mpd(phylocom$sample, cophenetic(phylocom$phylo), runs=999, null.model="trialswap")

to.plot<-data.frame(Constraint=c("Tip-swap", "Frequency", "Richness", "Independent swap", "Trial swap"),
                      z_value=c(out.mpd[3,6], out.mpd.f[3,6], out.mpd.r[3,6], out.mpd.i[3,6], out.mpd.t[3,6]), P_value=c(out.mpd[3,7], out.mpd.f[3,7], out.mpd.r[3,7], out.mpd.i[3,7], out.mpd.t[3,7]))
to.plot 

print(levels(to.plot$Constraint))

to.plot$Constraint <- factor(to.plot$Constraint, levels(to.plot$Constraint)[c(3,1,4,2,5)])

barplot(height=to.plot$P_value, names=to.plot$Constraint, xlab="Constraint", ylab="P-value")
abline(h=0.05, lty= "dashed", lwd=2, col="grey65")

#Ramdomizing to test trait data
tmp.traits <- phylocom$traits
rownames(tmp.traits) <- sample(rownames(phylocom$traits))
tmp.traits


#Altering the tree
j.tree2 <- j.tree
j.tree2$edge.length <- sample(j.tree$edge.length)
j.tree3<-chronopl(j.tree2, 1, age.max=max(vcv(j.tree)))


###### Building trees
##First step => find a published phylogeny in the primary literature or online 
#databases such as Tree Base (httpp://treebase.org/)
#Paup (Phylogenetic Analysis Using Parsimony was largely used in the 1990's and 2000' following the phyloegentic revolution)
#PhyML (http://athc.lirmm.fr/phyml; Guindon et al. 2010)
#RaxML (http://sco.h-its.org/exelixis/web/software/raxml/index.html; Stamatakis 2014)
#Bayesian Methods that implement Makov chain Monte Carlo (MCMC) methods for sampling 
#from a probability distribution are noew becoming more common, allowing for the 
#sampling of parameters space representing millions of trees
#Two of the most widely used are Mr. Bayes (http://mrbayes.sourceforge.net/; Ronquist and Huelsenbeck 2003)
#and Beast (http://beast.bio.ed.ac.uk/; Drummond et al. 2012)
#Here we provide the basic steps for reconstructing phylogeny in R from sequence data
#with a focus on ML methods
#We assume the reader has a list of taxa for which to reconstruct the phylogeny, and 
#walk through how to query GenBank and download raw sequence data, sequence alignment,
#and phylogenetic inference

#Downloading and aligning DNA sequences. This function returns a DNAbin object containing the sequence data in binary format. 
avenua.fatua <- read.GenBank("AJ746257.1") 
avenua.fatua

#Evaluates the complete sequence
as.character(Avenua.fatua)

#Chooses the genbank as the main bank to access for the sequences
#Makes  a query for a specific species 
choosebank("genbank")
Avenua.fatua <-query("Avenua.fatua", "sp=Avenua fatua AND K=rbcL")
getSequene(Avena.fatua$req[[1]], as.string=TRUE)
closebank()

#
species<-read.csv("Jasper species.txt", header=T)
dna.matrix<-list()
choosebank("genBank")
for (x in 1:length(species[,1])) {
  my.query<-paste("sp=", specie[x,1], "AND K=rbcl", sep="")
  jasp <- query("jasp", my.query)
  dna.matrix[[x]]<-get.sequence(jasp$req[[1]])
}
closebank()
}

coigen<-read.GenBank("JF434759.1")


