###Temporal networks
#A evolu��o temporal das redes nos leva a outra perspectiva da estrutura social e, em alguns casos, agrega dados em uma janela de tempo que pode destacar difus�es de estruturas temporais de informa��es importantes, comunidades, forma��es de opini�o, etc. N�o muitas op��es para visualiza��o de redes din�micas em softwares de SNA, a maioria trata somente redes est�ticas. E em alguns casos, eles s�o bem limitados em layouts din�micos. O Gephi tem um plugin bem interessante de Timeline, mas nenhum ainda possui uma possibilidade de transformar a anima��o em v�deo. Veremos neste post como fazer isso usando um script em R e usar o ffmpeg para tornar snapshots de grafos em v�deo. A id�ia � simples: 1- Gerar snapshots da rede em tempos diferentes usando R e sua biblioteca igraph; 2- Juntar todos em um v�deo usando o ffmpeg. Para o passo 1, n�s precisamos �desenhar� a rede para fazermos o cada snapshot. Precisamos organizar em um layout os n�s e as arestas espec�ficas de acordo com o tempo. O pacote igraph tem v�rios layouts, por exemplo, o Kamada-Kawai e o Fruchterman-Reingold. Mas temos complica��es no meio do caminho: Os algoritmos de layout geralmente, em suas itera��es visam minimizar a energia das for�as f�sicas entre os n�s, e come�am de uma    configura��o inicial que tipicamente � de uma condi��o, inicialmente, aleat�ria. Isso significa que chamadas sucessivas do algoritmo resultaria em diferentes resultados, caso sua rede n�o evolu�sse continuamente. Em nossa situa��o de rede temporal, temos que o layout de um snapshot para o outro seria diferente e geraria uma descontinuidade de layouts de um snapshot para o outro. Para nossa sorte, no igraph 0.6, podemos especificar as posi��es iniciais dos n�s. Podemos usar isso nos dois algoritmos citados acima. O plano � usar o layout do snapshot anterior como condi��o inicial do pr�ximo snapshot. A implementa��o seria assim:

library(igraph) 
par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,0)) 
g <- watts.strogatz.game(1,20,3,0.4) 
layout.old <- layout.fruchterman.reingold(g)
for(i in 1:4){ 
layout.new <- layout.fruchterman.reingold(g,params=list(niter=10,maxdelta=2,start=layout.old)) 
plot(g,layout=layout.new) 
layout.old <- layout.new 
}

#Percebe-se que h� dois par�metros passados para a fun��o de layout: niter=10 que especifica o n�mero de itera��es de minimiza��o de energia no algoritmo. Esse n�mero deveria ser pequeno, caso contr�rio o resultado final seria bem diferente da condi��o inicial. O mesmo se faz com o outro par�metro maxdelta=2 que controla o m�ximo de mudan�a de posi��oque os n�s sofrem no processo de minimiza��o. O outro problema � que em uma rede temporal, os n�s e/ou as arestas aparecem e desaparecem dinamicamente. Assim a rede que depende do tempo pode ter n�meros diferentes de n�s e/ou arestas de um snapshot para o outro. Isso significa que a rede anterior n�o pode ser usada como condi��o inicial para a pr�xima rede. A solu��o para esse problema � considerar todos (passado/presente/futuro) n�s e arestas para calcular o layout mas s� mostrar os n�s presentes no grafo, fazendo com que os outros se tornem transparentes. Esse truque permite a reutiliza��o dos layouts entre os passos, mas tamb�m produzir� uma visualiza��o mais ou menos constante, onde o layout em seu tempo espec�fico n�o estaria relacionado com a estrutura instant�nea do grafo temporal. Para superar esse problema, tiramos vantagem de uma propriedade dos algoritmos: n�s que s�o conectados s�o atra�dos entre si por suas arestas. Em determinado instante, n�s podemos modificar a atra��o entre os n�s dependendo se o n� est� presente ou n�o. S� podemos, no igraph 0.6, usar isso no algoritmo Fruchterman-Reingold, que tem o par�metro weights, um vetor que diz os pesos das arestas que � usado para medir a atra��o entre os n�s conectados. Por exemplo, usaremos isso para ser 1 o peso de um n� presente e 0(zero) o peso dos outros n�s. Isso produzir� um layout onde os n�s presentes s�o bem conectados enquanto os outros, passado/futuro, s�o repelidos deles. Esse efeito enfatiza o aparecimento e o desaparecimento dos n�s, mas pode criar muita confus�o caso isso aconte�a muito. Para testar essas id�ias, trabalharemos com um exemplo importante na Teoria de Grafos Complexos: um algoritmo para gerar redes sem escala de forma aleat�ria, Barab�si-Albert Model. Em nossa implementa��o, mantemos o mecanismo simples: come�ar com um certo n�mero de n�s, e a cada passo adicionar um n� para se conectar ao n�s existentes, que s�o selecionados proporcionalmente ao n�mero de links, que eles os n�s presentes, j� tem. Esse mecanismo, n�s leva, a ter n�s bem fortemente conectados juntos, �hubs�, com uma grande quantidade de n�s fracamente conectados. Um modelo para esse n�s, � encontrado no seguinte link: Cada linha na forma id1 | id2 | tempo, indica que h� um link entre id1 e id2 e ele aparece no tempo determinado. Dependendo do contexto, isso pode representar que a conex�o foi ativada em um instante espec�fico(por exemplo quando se d� RT em um tweet) ou que foi o instante exato em que a aresta surgiu(como o modelo de barab�si-Albert, aqui mostrado). O c�digo que gera os snapshots e produz figuras PNG para cada um, vem a seguir:

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

