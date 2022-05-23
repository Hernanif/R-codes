#Script do R para a apresentacao da UnB
#Autor: Hernani F. M. Oliveira
#Data: 20/04/2022


install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(ggplot2)
library(igraph)
library(scholarnetwork)
library(statnet)

setwd("~/Desktop/Apresentacao da UnB")

#Read the file witht the data
number.of.articles<-read.table("number.of.articles.txt", h=T)
number.of.articles 

impact.factor<-read.table("impact.factor.txt", h=T)
impact.factor


########################################
impact.factor<-c(5.09, 7.09, 3.549, 4.657, 5.139, 3.286, 7.148, 2.416, 3.14, 4.171, 1.544, 2.707, 2.91, 1.859, 2.047, 1.968, 2.984, 1.391, 0.69, 1.436, 0.833)
journal<-c("Journal of Animal Ecology", "Molecular Ecology Resources", "Biodiversity and Conservation", "Ecological Applications", "Diversity and Distributions",
           "Zoological Journal of the Linean Society", "Global Ecology and Biogeography", "Journal of Mammalogy", "Zoologica Scripta", "Frontiers in Ecology and Evolution",
           "Folia Geobotanica", "Evolutionary Ecology", "Ecology and Evolution", "Plant Ecology", "Diversity", "Tropical Conservation Science", "PeerJ", 
           "Journal of Ethnobiology", "Therya", "African Journal of Zoology", "European Journal of Ecology")
journal<-c("journal", "journal", "journal", "journal", "journal",
           "journal", "journal", "journal", "journal", "journal", "journal", "journal", "journal", "journal", "journal", "journal", "journal", 
           "journal", "journal", "journal", "journal")

reviewer <- data.frame (first_column  = impact.factor,
                  second_column = journal)



# Create Data
data <- data.frame(
  group=c("A1", "A2", "B1", "B2", "B3", "C", "Sem Avaliação"),
  value=c(8, 1, 5, 2, 1, 1, 2)
)


data <- data.frame(
  group=c("A1", "A2", "B3", "Sem Avaliação", "B2", "C", "B1"),
  value=c(8, 1, 1, 1, 2, 2, 5)
)

data

8 + 1 + 5 + 2 + 1 + 1 + 2
8/20


# Basic piechart
ggplot(data, aes(x="", y=value, fill=reorder(data$value, data$group))) +
  geom_bar(stat="identity", width=1, color ="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels


  
######## Financiamentos

# Create Data
data1 <- data.frame(
  group=c("Bolsas pessoais", "Financiamento para pesquisa", "Congressos e Cursos", "Bolsas para orientandos"),
  value=c(1153705, 32006, 10906, 14400)
)

data1


library(RColorBrewer)
coul <- brewer.pal(4, "Set2") 
barplot(height=data1$value, names=data1$group, col=coul,  ylim=c(0,1200000), cex.names = 2.0, axis.lty=4)

?barplot


library(ggplot2)


p<-ggplot(data1, aes(x=reorder(group,-value), y=value, fill=group)) + 
  geom_bar(stat="identity")+theme_minimal() + theme(axis.text.x = element_text(face="bold", color="black", 
                                                                                size=24),
                                                     axis.text.y = element_text(face="bold", color="black", 
                                                                                size=23, )) +  scale_y_continuous(breaks = seq(0, 1200000, by = 200000))
p

1153705+ 9600+ 32006+ 10906


1206217 +4800

  
reviewer
boxplot(reviewer)

# Add basic box plot
ggplot(reviewer, aes(x=second_column, y=first_column)) + 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2), size=5) + theme(axis.text.x = element_text(face="bold", size=32), axis.text.y = element_text(face="bold",  size=32)) 



# Add notched box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(notch = TRUE)+
  geom_jitter(position=position_jitter(0.2))

##########################################

library(ggplot2)

#Regression to show the trends of the impact factor of my articles and number of articles

#Regression of impact factor by year
par(mfrow=c(2,2))
ggplot(data=impact.factor, aes(as.factor(x=Year), y=Impact.Factor, group=1)) +
  geom_point(size=5)+  
  geom_smooth(method = "gam") + theme(axis.text.x = element_text(face="bold", color="black", 
                                                                 size=32, angle=45),
                                      axis.text.y = element_text(face="bold", color="black", 
                                                                 size=32, angle=45)) 

ggplot(data=impact.factor, aes(as.factor(x=Year), y=Impact.Factor, group=1)) +
  geom_point(size=5)+  
  geom_smooth(method = "lm") + theme(axis.text.x = element_text(face="bold", color="black", 
                                                                size=32, angle=45),
                                     axis.text.y = element_text(face="bold", color="black", 
                                                                size=32, angle=45)) 


  


#Regression of the number of articles
ggplot(number.of.articles, aes(as.factor(x=Year), y=Number.of.Articles, group=1)) +
  geom_point(size=5)+  
  geom_smooth(method = "gam") + theme(axis.text.x = element_text(face="bold", color="black", 
                                                                size=32, angle=45),
                                     axis.text.y = element_text(face="bold", color="black", 
                                                                size=32, angle=45)) + scale_y_continuous(breaks = seq(0, 10, by = 2))


abline(v=2008, col="blue", lwd=2) 
abline(v=2018, col="green", lwd=2)


################################################

#Text mining of the words from my articles

text <- readLines(file.choose())

inspect(docs)

docs <- Corpus(VectorSource(text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



####################

kellogg = get_publications("b_aHS1oAAAAJ&hl=pt-BR") #id from url in google scholar page
write.csv(kellogg, "~/Desktop/Apresentacao da UnB/kellogg.csv", row.names = FALSE)

kellogg<-read.csv("kelloggI.csv", h=T)

kellogg

#convert factor variables to strings
kellogg[,sapply(kellogg, class)=="factor"] = 
  as.character(kellogg[,sapply(kellogg, class)=="factor"])
saveRDS(object = kellogg, "kellogg_scholar_pull.RDS")

names(kellogg)

ellipsis_indices = grep(kellogg$author, pattern =  "\\.\\.\\.")
author_complete = sapply(kellogg$pubid[ellipsis_indices], 
                         get_complete_authors, id = "NP_WECMAAAAJ")
saveRDS(object = author_complete, file = "author_complete.RDS")

#helper function to put last names in a regular format (first letter uppercase, rest lowercase):
lowerName <- function(x) {gsub(x, pattern = "\ ([A-Z]){1}([A-Z]+)", replacement = '\ \\1\\L\\2', perl = TRUE)}

#helper function to convert a name to an initial (applied after lowername):
initialName <- function(x) {gsub(x, pattern = "([A-Za-z]){1}([a-z]+)", replacement = '\\1', perl = TRUE)}

author_complete_reformat = lapply(author_complete, function(elem) {
  
  # Split strings of authors into individual authors based on commas:
  split_elem = strsplit(elem, ", ")[[1]]
  split_elem = sapply(split_elem, gsub, pattern = "(\\x.{2})+", replacement ="")
  
  # Put author names into INITIALS, Lastname format:
  rename_elem = sapply(split_elem, function(name) {
    
    #in case of name like "H J\xfc\xbe\x8d\x86\x94\xbcrvinen":
    name2 = iconv(name, "latin1", "ASCII", sub = "") 
    name2 = lowerName(name2)
    name2 = strsplit(name2, " ")[[1]]
    
    lastname = last(name2)
    
    if (length(name2) > 1) {
      name2 =  sapply(1:(length(name2)-1), function(i) {initialName(lowerName(name2[i]))})
      name2 = paste(paste(name2, collapse = ""), lastname, sep = " ")
    } else {
      name2 = lastname
    }
    return(name2)
  })
  
  # Put separated names back in single strings:
  rename_elem = paste(rename_elem, collapse = ", ")
  
  return(rename_elem)
})


# Save original author column as "author_orig" and update the "author column"
kellogg$author_orig = kellogg$author
kellogg$author = as.character(kellogg$author)
kellogg$author[ellipsis_indices] = author_complete_reformat

kellogg$author = sapply(kellogg$author, str_replace, pattern = "L Kellogg", replacement = "LH Kellogg")

kellogg.coauthors = sapply(as.character(kellogg$author), strsplit, ", ")

  
  kellogg.coauthors = lapply(kellogg.coauthors, trimws)


  kellogg.coauthors.unique = unique(unlist(kellogg.coauthors))[order(unique(unlist(kellogg.coauthors)))]

kellogg.bipartite.edges = lapply(kellogg.coauthors, function(x) {kellogg.coauthors.unique %in% x})
kellogg.bipartite.edges = do.call("cbind", kellogg.bipartite.edges) # dimension is number of authors x number of papers
rownames(kellogg.bipartite.edges) = kellogg.coauthors.unique

kellogg.mat = kellogg.bipartite.edges %*% t(kellogg.bipartite.edges) #bipartite to unimode
mat = kellogg.mat[order(rownames(kellogg.mat)), order(rownames(kellogg.mat))]

kellogg.statnet = as.network(kellogg.mat, directed = FALSE, names.eval = "edge.lwd", ignore.eval = FALSE)
kellogg.statnet # view network summary
summary(kellogg.statnet)


authors<- c("RL Muylaert", "RD Stevens", "CEL Esbérard", "MAR Mello", "GST Garbino", "LH Varzinczak", "D Faria", "MM Weber", "PK Rogeri", "AL Regolin", "HFM Oliveira", "LM Costa", 
            "MAS Barros", "G Sabino_Santos Jr", "MAC Morais", "VS Kavagutti", "FC Passos", "E Marjakangas", "FGM Maia", "MC Ribeiro", "M Galetti", "NF Camargo", "LMS Aguiar",
            "Y Gager", "DR Hemprich-Bennett", "SC Le Comber", "SJ Rossiter", "EL Clare", "ZJ Czenze", "JT Tucker", "JE Littlefair", "M Brigham", 
            "A Hickey", "S Parsons", "B Rodriguez-Herrera", "JF Ribeiro", "AJA Camargo", "EM Vieira", "M Oprea", "RI Dias",  "E Ramon", "RCC Martins",
            "B Pereira", "DC Silva", "PL Zangrandi", "FMCB Domingos", "NT Fominka", "GCT Taboue", "FE Luma", "CA Robinson", "EB Fokam", "RBP Pinheiro", "IG Varassin", 
            "M Kuzmina", "ACM Martins", "B Zimbres", "RJ Sa-Neto", "J Marinho-Filho", "KC Tanalgo", "JAG Tabora", "D Haelewaters", "CT Beranek", "A Otálora-Ardila", 
            "E Bernard", "F Gonçalves", "A Eriksson", "M Donnelly", "JM González", "HF Ramos", "AC Rivas", "PW Webala", "S Deleva", "R Dalhoumi", "J Maula", "D Lizarro", 
            "LF Aguirre", "N Bouillard", "MNRM Quibod", "J Barros", "MA Turcios-Casco", "M Martínez", "DI Ordoñez-Mazier", "JAS Orellana", "EJ Ordoñez-Trejo", 
            "D Ordoñez", "A Chornelia", "JM Lu", "C Xing", "S Baniya", "LH Dias-Silva", "N Ruadreo", "A Hughes", "GB Freire", "T Silva", "C Collier", 
            "HP Rodrigues", "JP Dias", "JP Santos", "OJ Marini-Filho", "AVL Freitas", "AM Smilanich", "LA Dyer", "IR Diniz", "SL Peters", "RJ Gray", "RLM Novaes",
            "SMS Mandel")
str(authors)
table(authors)

numero.de.artigos<-c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 22, 1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 3,
                     1, 3, 1, 3, 4, 1, 1, 1, 1, 
                     1, 1, 2, 1, 1, 1, 1, 1,  1, 1,
                     1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 
                     1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                     1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                     1)
str(numero.de.artigos)
table(numero.de.artigos)

countries<-c("New Zealand", "USA", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", 
             "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "Finland", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil",
             "Germany", "UK", "UK", "UK", "Canada", "New Zealand", "UK", "UK", "Canada", 
             "New Zealand", "New Zealand", "Costa Rica", "Brazil", "Brazil", "Brazil", "Brazil", "Brazil",  "Brazil", "Brazil",
             "Brazil", "Brazil", "Canada", "Brazil", "Cameroon", "Cameroon", "Cameroon", "USA", "Cameroon", "Brazil", "Brazil",
             "Canada", "Brazil", "Brazil", "Brazil", "Brazil", "Philippines", "Philippines", "Czech Republic", "Australia", "Brazil", 
             "Brazil", "Brazil", "Brazil", "Canada", "Cuba", "Cuba", "Cuba", "Kenya", "Bulgaria", "Tunisia", "Philippines", "Bolivia",
             "Bolivia", "Belgium", "Philippines", "Brazil", "Honduras", "Honduras", "Honduras", "Honduras", "Honduras", 
             "Honduras", "China", "China", "Israel", "India", "Brazil", "Thailand", "China", "Brazil", "Brazil", "USA", 
             "Brazil", "Brazil", "Brazil", "Brazil", "Brazil", "USA", "USA", "Brazil", "Canada", "Vietnam", "Brazil", 
             "Brazil")
str(countries)
table(countries)

colors<-c("cornflowerblue", "yellow4", "yellow", "yellow", "yellow", "yellow", "yellow", "yellow", "yellow", "yellow", "yellow", "yellow", 
             "yellow", "yellow", "yellow", "yellow", "yellow", "darkorchid", "yellow", "yellow", "yellow", "yellow", "yellow",
             "deeppink", "gold4", "gold4", "gold4", "orange", "cornflowerblue ", "gold4", "gold4", "orange", 
             "cornflowerblue", "cornflowerblue", "cyan3", "yellow", "yellow", "yellow", "yellow", "yellow",  "yellow", "yellow",
             "yellow", "yellow", "orange ", "yellow", "white", "white", "white", "yellow4", "white", "yellow", "yellow",
             "orange", "yellow", "yellow", "yellow", "yellow", "aquamarine", "aquamarine", "bisque1", "black", "yellow", 
             "yellow", "yellow", "yellow", "orange", "chocolate", "chocolate", "chocolate", "chartreuse1", "grey", "khaki", "aquamarine", "darkgreen",
             "darkgreen", "blue", "aquamarine", "yellow", "darkslateblue", "darkslateblue", "darkslateblue", "darkslateblue", "darkslateblue", 
             "darkslateblue", "darkolivegreen", "darkolivegreen", "cadetblue4", "coral1", "yellow", "azure2", "darkolivegreen", "yellow", "yellow", "yellow4", 
             "yellow", "yellow", "yellow", "yellow", "yellow", "yellow4", "yellow4", "yellow4", "orange", "maroon", "yellow", 
             "yellow")
str(countries)
table(countries)


df <- data.frame(authors, countries, numero.de.artigos, colors)
df
colorRampPalette #cria cores diferentes para a coloracao de um objeto



1)Australia=black        
2)Belgium=blue
3)Bolivia=darkgreen          
4)Brazil=yellow       
5)Bulgaria=grey       
6)Cameroon=white         
7)Canada=orange          
8)China=darkolivegreen      
9)Costa Rica=cyan3           
10)Cuba=chocolate 
11)Czech Republic=bisque1        
12)Finland=darkorchid 
13)Germany=deeppink       
14)Honduras=darkslateblue          
15)India=coral1         
16)Israel=cadetblue4          
17)Kenya=chartreuse1    
18)New Zealand=cornflowerblue    
19)Philippines=aquamarine       
20)Thailand=azure2         
21)Tunisia=khaki             
22)UK=gold4            
23)USA=yellow4        
24)Vietnam=maroon 

kellogg.statnet


plot.network(kellogg.statnet,  edge.col = "grey",   edge.lwd = "edge.lwd", label = "vertex.names", label.cex = 1.0, label.pad = 0, label.pos = 1)
str(kellogg.statnet)
?plot.network


kellogg.statnet



###################################


ggplot(impact.factor, aes(as.factor(x=Year), y=Impact.Factor)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  geom_smooth(method = "lm")
abline(v=2008, col="blue", lwd=2) 
abline(v=2018, col="green", lwd=2)



ggplot(data=impact.factor, aes(as.factor(x=Year), y=Impact.Factor, group=1)) +
  geom_line(color="blue")+
  geom_point()+  
  geom_smooth(method = "lm")




# 2. Plot a first line
plot(impact.factor$Year, impact.factor$Impact.Factor, type = "b", frame = FALSE, pch = 19,
     col = "black", xlab = "Ano", ylab = "Impact Factor", 
     lty = 1, lwd = 1)
abline(v=2008, col="blue", lwd=2) 
abline(v=2018, col="green", lwd=2) 


ggplot(number.of.articles, aes(as.factor(x=Year), y=number.of.articles)) + 
  geom_boxplot()+
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  geom_smooth(method = "lm")
abline(v=2008, col="blue", lwd=2) 
abline(v=2018, col="green", lwd=2)


number.of.articles 


plot(number.of.articles$Year, number.of.articles$Number.of.Articles, type = "b", frame = FALSE, pch = 19,
     col = "black", xlab = "Ano", ylab = "Impact Factor", 
     lty = 1, lwd = 1)
abline(v=2008, col="blue", lwd=2) 
abline(v=2018, col="green", lwd=2) 



# Libraries
install.packages("tidyverse")
install.packages("hrbrthemes")
install.packages("viridis")


library(tidyverse)
library(hrbrthemes)
library(viridis)


# Plot
impact.factor %>%
  ggplot( aes(x=factor(Year), y=Impact.Factor, fill=Year)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")+
  geom_smooth(formula = y ~ x, method = "lm")



# Plot
impact.factor %>%
  ggplot( aes(x=factor(Year), y=Impact.Factor, fill=Year)) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_smooth(formula = y ~ x, method = "lm")


ggplot(impact.factor, aes(impact.factor$Year), impact.factor$Impact.Factor)) + 
  geom_point() +
  geom_smooth()

ggplot(aes(x=factor(impact.factor$Year), y=impact.factor$Impact.Factor, fill=Year)) + 
  geom_point() +
  geom_smooth(method = "lm")






impact.factor2<-read.table("Impact.Factor.txt", h=T)

install.packages("ggplot2")
library(ggplot2)

Impact.FActor<-c("6.185", "11.123", "5.541", "24.651", "4.171", "3.558", "4.677", "1.75", "3.24", "1.404", "5.499", "1.968", "11.123", "2.047", "2.91", "2.91", "3.549", "2.047", "11.123", "5.499", "2.163")
Impact.Factor<-c(6.185, 11.123, 5.541, 24.651, 4.171, 3.558, 4.677, 1.75, 3.24, 1.404, 5.499, 1.968, 11.123, 2.047, 2.91, 2.91, 3.549, 2.047, 11.123, 5.499, 2.163)


mean(Impact.Factor)
sd(Impact.Factor)

boxplot(Impact.Factor)


articles<-("Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles", "Articles")
df <- data.frame(IF, articles)
df

a <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'pencil_case')
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(2.5, 8, 10, 7)
# Join the variables to create a data frame
df <- data.frame(a,b,c,d)
df


IF

articles1<-as.data.frame(articles)

boxplot(impact.factor2, xlab="Articles", ylab="Impact Factor")
mean(IF$IF)
sd(IF$IF)

main = "Mean ozone in parts per billion at Roosevelt Island",
xlab = "Parts Per Billion",
ylab = "Ozone",

ggplot(impact.factor2, aes("Articles","Impact Factor")) + geom_jitter()
p + geom_point()

p + geom_jitter()


geom_jitter(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "jitter",
  ...,
  width = NULL,
  height = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
