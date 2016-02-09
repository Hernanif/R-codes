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

