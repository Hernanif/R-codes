
###Non-parametric tests
##Kruskal-Wallis (equivalent to an ANOVA)
#plotting the data

A<-c(27,14,8,18,7)
B<-c(48,18,32,51,22)
C<-c(11,0,3,15,8)
D<-c(44,72,81,55,39)

#take a look at the data
summary(A) 
summary(B)
summary(C)
summary(D)

#Taking a better look at the differences between samples
boxplot(A,B,C,D, names=c("A","B","C","D"), ylab="No. orquídeas",
col=3)

#Puts the data on a vertical format
orq.dados<-data.frame(Campo<-gl(4,5), Orquideas<-c(A,B,C,D))

#Check the data
orq.dados

#Runs the Kruskal Wallis test
kruskal.test(Orquideas~Campo, orq.dados)




#Mann-whitney Wilcoxon test
A<-c(8,12,15,21,25,44,44,60)
B<-c(2,4,5,9,12,17,19)
boxplot(A,B, names=c("A","B")) #used to verify if the statistics of the network are right
wilcox.test(A,B) #runs the test

#Mann-Whitney Wilcoxon paired test
ago<-c(10.3,11.4,10.9,12.0,10.0,11.9,12.2,12.3,11.7,12.0)
set<-c(12.2,12.1,13.1,11.9,12.0,12.9,11.4,12.1,13.5,12.3)
boxplot(ago,set,names=c("Agosto","Setembro"))
wilcox.test(ago,set, paired=TRUE)



##############################################################################

correnteza<-c(rep("forte",7),rep("fraco",7),rep("moderado",7))
nadadeira<-c(30,70,50,60,70,50,40,30,50,40,50,20,20,30,40,50,40,60,60,50,40)
peixe<-data.frame(correnteza,nadadeira)
rm(correnteza)
rm(nadadeira)
str(peixe)
attach(peixe)
head(peixe)

#Visualização de Dados
boxplot(nadadeira~correnteza, col="red",ylab="Nadadeira")

#To rotate boxplot labels, add las=2 to the general formula

#Testa premissa de normalidade
shapiro.test(nadadeira[correnteza=="forte"])
shapiro.test(nadadeira[correnteza=="fraco"])
shapiro.test(nadadeira[correnteza=="moderado"])

#Test specific columns for normality inside spreadsheet (P.mesoamericanus_normal in the below case)
shapiro.test(motut$P.mesoamericanus_normal)


#Testa premissa de homocedasticidade
bartlett.test(nadadeira~correnteza)

#Calcula média
(x.forte<-mean(nadadeira[correnteza=="forte"]))
(x.fraco<-mean(nadadeira[correnteza=="fraco"]))
(x.moderado<-mean(nadadeira[correnteza=="moderado"]))
(x.total<-mean(nadadeira))

#Calcula as Somas de Quadrados (SS)
(ss.total<-sum((nadadeira-x.total)^2))
(ss.dentro.forte<-sum((nadadeira[correnteza=="forte"]-x.forte)^2))
(ss.dentro.fraco<-sum((nadadeira[correnteza=="fraco"]-x.fraco)^2))
(ss.dentro.moderado<-sum((nadadeira[correnteza=="moderado"]-x.moderado)^2))
(ss.dentro<-ss.dentro.forte+ss.dentro.fraco+ss.dentro.moderado)
(ss.entre.forte<-7*(x.forte-x.total)^2)
(ss.entre.fraco<-7*(x.fraco-x.total)^2)
(ss.entre.moderado<-7*(x.moderado-x.total)^2)
(ss.entre<-ss.entre.forte+ss.entre.fraco+ss.entre.moderado)

#Calcula variância (MS)
(ms.entre<-ss.entre/(3-1))
(ms.dentro<-ss.dentro/((7-1)*3))

#Teste F
(f<-ms.entre/ms.dentro)
curve(df(x,2,18),0,5)
abline(v=f,col="red",lwd=2)
1-(pf(f,2,18)) #valor de p
qf(0.95,2,18) #valor crítico para alfa=0,05

#Faz ANOVA
a<-aov(nadadeira~correnteza)
summary(a)

#Proporção da variação total explicada por correnteza (r²)
ss.entre/ss.total

#Teste de comparação múltipla
TukeyHSD(a)

#ANOVA não-paramétrica
kruskal.test(nadadeira~correnteza)

r.nadadeira<-rank(nadadeira)
r.nadadeira
nadadeira
summary(aov(r.nadadeira~correnteza))
h<-522.9/var(r.nadadeira)
h

14/01/2013
#Define o diretório de trabalho
setwd("/Users/grcolli_on_bachia/Desktop")
getwd

#Carrega os dados
beija.flor<-read.table("aula6a.dat",h=T)
head(beija.flor)
attach(beija.flor)
class(beija.flor)

#Visualiza dados
table(bloco,acucar) (mostra quantas combinações existem para os blocos e os açúcares)
table(bloco) (mostra quantas observações existem oara cada bloco)
table(acucar) (mostra quantas observações existem para cada variável açúcar)
par(mfro=c(1,2))
plot(frequencia~bloco+acucar,las=1,col="cyan") (faz um gráfico pra blocos mais um gráfico para açúcares)
par(mfrow=c(1,1))

#Faz ANOVA de Blocos Aleatórios
anova.beija.flor<-aov(frequencia-bloco+acucar) (como eu não estou interessado na interação entre o bloco e açúcar, colocasse o sinal de adição(+). Caso houvesse interesse em verificar a interação, deveria ser colocado o sinal de vezes (*)) 
                                               (se ele estivesse interessado na interação, ele teria colocado Vezes no lugar de sinal de adição)
#Calcula médias
model.tables(anova.beija.flor,"means",se=T) (ele é interessante porque ele dá a média total por blocos e por tratamento, além do erro-padrão para as diferenças de média)
model.tables(anova.beija.flor,"effects",se=T)
plot.design(frequencia~bloco+acucar)

#Testes de comparação múltiplo
TukeyHSD(anova.beija.flor)
parwise.t.test(frequencia,acucar,p.adj="bonf")
parwise.t.test(frequencia,acucar,p.adj="holm")

#Testa premissas (o teste de premissas muitas vezes é feito por meio da análise dos resíduos)
par(mfrow=c(2,2))
plot(anova.beija.flor)
par(mfrow=c(1,1))
pnorm(-0.0005)
qnorm(-0.0005)
qnorm(0.999)
qnorm(0.9995)
sqrt(qnorm(0.995))
sqrt(qnorm(0.999))
shapiro.test(porcentagem[RB])
shapiro.test(porcentagem[RB=="RB"])
shapiro.test(frequencia[acucar=="sacarose"])
shapiro.test(anova.beija.flor$residuals)
print.deafult(anova.beija.flor) (imprime tudo o que existe dentro do objeto entre parênteses)
bartlett.test(RB~porcentagem)
fligner.test(frequencia~acucar) (Outro teste possível para homocedasticidade)
class(anova.beija.flor)
summary(aov(frequencia~bloco+acucar))
summary(aov(frequencia~acucar))

## ANOVA fatorial
# ***************

#Carrega os dados
peixes<-read.table("aula6b.dat",h=T)
head(peixes)
attach(peixes)

#Visualiza os dados
table(isca,trecho)
table(isca)
table(trecho)
par(mfrow=c(1,2))
plot(capturas~isca+trecho,las=1,col="cyan")
par(mfrow=c(1,1))
interaction.plot(isca,trecho,capturas)
interaction.plot(trecho,isca,capturas)

#Faz ANOVA fatorial
anova.peixes<-aov(capturas~isca+trecho)
summary(anova.peixes)

#Calcula médias
tapply(capturas,isca,mean)
model.tables(anova.peixes,"means")

#Teste de Comparação Múltipla
TukeyHSD(anova.peixes)
parwise.t.test(capturas,isca,p.adj="holm")

#Testa premissas (o teste de premissas muitas vezes é feito por meio da análise dos resíduos)
par(mfrow=c(2,2))
plot(anova.peixes)
par(mfrow=c(1,1))
shapiro.test(anova.peixes$residuals)
bartlett.test(capturas~isca)
bartlett.test(capturas~trecho)
bartlett.test(capturas~isca*trecho)
fligner.test(capturas~isca)
fligner.test(capturas~trecho)
fligner.test(capturas~trecho*isca)

modelo1<-anova.peixes
modelo2<-aov(captura~isca)
anova(modelo1,modelo2)
summary(aov(rank(capturas)~isca*trecho))
summary(aov(capturas)~isca*trecho))

15/01/2013
#Entra os dados
especie<-c(rep("G.amarali",8),rep("G.darwini",8),rep("G.geckoides",8))
especie
individuo<-c("a","a","b","b","c","c","d","d","e","e","f","f","g","g","h","h","i","i","j","j","k","k","l","l")
ovo<-rep(c("um","dois"),12)
massa<-c(585,595,778,809,840,836,701,683,698,698,560,545,507,493,638,658,566,575,778,792,699,692,621,645)
lagartos<-data.frame(especie,individuo,ovo,massa)
rm(especie,individuo,ovo,massa)
head(lagartos)
attach(lagartos)
length(massa)

#Faz ANOVA normal
summary(aov(massa~especie))

#Faz ANOVA Hierárquica
anova.lagartos<-aov(massa~especie+Error(individuo/especie)) 
(quando se coloca error, vai se dizer qual é a variação individual)
summary(anova.lagartos)
curve(df(x,2,21),col="red",lwd=3,xlim=c(0,4),ylim=c(0,1))
curve(df(x,2,9),col="blue",lwd=3,xlim=c(0,4),ylim=c(0,1),add=T)
qf(0.95,2,21)
qf(0.95,2,9)

#Calcula médias
medias.ovos<-aggregate(massa,list(Individuo=individuo),mean)
medias.ovos
m<-tapply(massa,individuo,mean)
m
class(m)
class(medias.ovos)
sp<-c(rep("G.amarali",4),rep("G.darwini",4),rep("G.geckoides",4)

anova.medias<-aov(massa~especie+Error(individuo/especie)) 


#Faz ANOVA com médias
summary(aov(medias.ovos$x-sp))

##ANOVA de Medidas Repetidas
#***************************

berne<-c(1,1,3,5,2,3,4,3,5,4,6,8,6,7,5)
mes<-c(rep("m1",5),rep("m2",5),rep("m3",5))
sujeito<-rep(c("t1","t2","t3","t4","t5"),3)
tatu<-data.frame(sujeito,mes,berne)
rm(sujeito,mes,berne)
attach(tatu)
head(tatu)
tatu

#ANOVA normal
summary(aov(berne~mes))

#ANOVA de Medidas Repetidas
anova.tatu<-aov(berne~mes+Error(sujeito/mes)) (caso se estivesse interessado na interação, seria mes*tratamento em todos os casos. Quando se coloca mes*tratamento, ele calcula o valor do tratamento, o valor do mes e o valor da interação entre os dois)) anova.tatu<-aov(berne~mes*tratamento+Error(sujeito/mes*tratamento))
summary(anova.tatu)

#ANOVA de Medidas Repetidas Fatorial
anova.tatu<-aov(berne~mes*tratamento+Error(sujeito/mes*tratamento))

#Teste de Premissas
par(mfrow=c(2,2))
plot(anova.tatu)
par(mfrow=c(1,1))
anova.tatu$residuals

#Interaction plot
interaction.plot(mes,sujeito,berne)


16/01/2013

***** Correlação linear
library(MASS)
data(crabs)
head(crabs)
tail(crabs)
attach(crabs)

#Covariancia
cov(CL,CL)
var(CL) #a covariância de uma variavel consigo mesma e a sua variancia
cov(CL,FL) #a covariância de duas variaveis independentes é zero.
cov(FL,CL)
sum(((CL-mean(CL))*FL-mean(FL)))/(length(FL)-1))
constante<-rep(50,200)
constante
cov(CL,constante)
cov(crabs[,-c(1:3)])
cov(crabs[,4:8]) #como as covariâncias dependem da escala, elas devem ser padronizadas de acordo com o desvio-padrão para que o problema da escala seja resolvido e isso é feito
var(FL)                   
cor(CL,CW) #a partir desse valor, ainda tem que ser testada a significância
cov(CL,CW)
plot(CL,CW,type="p",pch=1,cex=2,col="red") #pch= plot character #dica: mexer em nomes do gráfico em outro programa (corel)
pairs(crabs[,4:8],col="red") #faz a correlação entre variáveis par a par
cor(crabs[,4:8])
cor.test(CL,CW)
shapiro.test(CW)
shapiro.test(CL)

#Correlação de Spearman(nao-parametrica)
cor.test(porcentagem,RB,method="spearman")
cor.test(rank(CW),rank(CL)) #estes dois comandos são equivalentes

#Regressao
modelo<-lm(CW~CL)
modelo
abline(modelo,col="blue")
abline(v=mean(CL),lty=2)
abline(h=mean(CW),lty=2)
anova(modelo)
sum((CW-mean(CW))^2)
SS.total<-sum((CW-mean(CW))^2)
SS.residuals<-sum((CW-predict(modelo))^2)
SS.modelo<-sum((predict(modelo)~mean(CW))^2)
SS.total
SS.residuals
SS.modelo
SS.modelo/SS.total #coeficiente de determinação
cor(CL,CW)^2
curve(df(x,1,198))

par(mfrow=c(2,2))
plot(mfrow=c(2,2))
plot(modelo)

shapiro.test(residuals(modelo))
plot(rstandard(modelo)) #gráfico dos resíduos padronizados
plot(rstudent(modelo))
influence.measures(modelo) # o melhor indicador é a distância de cook e o asterisco representa a influência
plot(cooks.distance(modelo,type="h")) #faz um gráfico somente sobre a distância de cook. Só é preocupante se a distância de Cook for maior do que 2.
boxplot(rstandard(modelo))
qnorm(0,99)

Aula 17/01/2013

#Carrega os dados
library(MASS)
data(crabs)
head(crabs)

#Faz regressao multipla
reg<-lm(CW~FL+RW+BD+CL, crabs) #a ordem das variáveis não é importante. Se houver interesse nas interações, basta colocar um asterisco entre as variáveis da fórmula)
summary(reg)
anova(reg) #Usa-se o comando anova quando já se tem os valores. Caso 
attach(crabs)
var(CW)*199

#Calcula coeficientes padronizados
reg.std<-lm(CW~scale(FL)+scale(RW)+scale(CL)+scale(BD))
summary(reg.std)

#Faz seleção automática de modelos
summary(step(reg))
stepAIC(reg,direction="backward",trace=2)

#Faz seleção manual de modelos
modelo.nulo<-lm(CW~1,crabs)
modelo.completo<-lm(CW~FL+RW+CL+BD)

#Tenta remover variáveis do modelo completo
drop1(modelo.completo,test=c("F"),trace=T) #o p mostrará se a retirada do modelo é significativo ou não
?drop1

#Tenta acrescentar variáveis do modelo nulo
add1(modelo.nulo,scope=formula(modelo.completo),test=c("F")) #o scope servirá para dizer de onde ele retirará os dados das variáveis para acrescentar ao modelo. Nesse caso, ele retirará as variáveis da fórmula do modelo.completo que já foi colocado antes. 

#Acrescenta CL ao modelo
modelo.nulo<-update(modelo.nulo,.~.+CL) # .~. significa manter tudo o que estava antes

#Tenta remover variaveis do modelo
drop1(modelo.nulo,test=c("F"),trace=T)

#Tenta acrescentar variáveis do modelo nulo
add1(modelo.nulo,scope=formula(modelo.completo),test=c("F")) #o scope servirá para dizer de onde ele retirará os dados das variáveis para acrescentar ao modelo. Nesse caso, ele retirará as variáveis da fórmula do modelo.completo que já foi colocado antes. 

#Acrescenta CL ao modelo
modelo.nulo<-update(modelo.nulo,.~.+RW) # .~. significa manter tudo o que estava antes

#Tenta remover variaveis do modelo
drop1(modelo.nulo,test=c("F"))

#Analise de modelo medio
install.packages("MuMIn", dependence=T)
library(MuMIn)
dd.reg<-dredge(reg)
dd.reg

modelos.selecionados<-get.models(dd.reg,delta<60) #60 é o valor de delta observado na tabela para o qual parece existir uma piora significativa na qualidade de explicação do modelo
summary(modelos.selecionados)
summary(model.avg(modelos.selecionados))
summary(reg)

#Testa premissas
par(mfrow=c(2,2))
plot(reg)
par(mfrow=c(1,1))
pairs(crabs[,4:8])
cor(crabs[,4:8])

#Analise de covariancia
attach(crabs)
plot(CL,CW,pch=1,col=c("blue","orange")[as.numeric(sp)],cex=2)
cor.test(CW,CL)
summary(lm(CW~CL*sp)) #testa premissa de ausência de interação
summary(lm(CW~CL+sp)) #faz ANCOVA (invalido nesse caso devido á interação, mas o comando da ancova é esse)


#### Análise de Componentes Principais
#**********************************

#Carrega os dados
library(MASS)
data(crabs)
attach(crabs)
head(crabs)

#Visualização de Dados Multivariados
pairs(crabs[,4:8],pch=19,col="red")

#Matrizes de correlação e covariancia
cor(crabs[,4:8])
cor(crabs[,c(4,5,8)) #correlaciona variáveis específicas

#Matrizes de correlação e covariância
cov.crabs<-cor(crabs[,4:8])
round(cov.crabs,2)
sum(diag(cov.crabs)) #a diagonal principal tem as variâncias e os outros dados são as covariâncias.
sscp.crabs<-cov.crabs*199
round(sscp.crabs,2)
sum(sscp.crabs)
sum(diag(sscp.crabs))
round(cor(crabs[,4:8]),2) #numa matriz de 5 x 5, existem 10 correlações importantes a serem verificadas.
diag(sscp.crabs)

#Faz PCA
pca.crabs<-prcomp(crabs[,4:8])
pca.crabs
summary(pca.crabs) #autovaloes
round(pca.crabs$rotation,2) #autovetores #o coeficiente não significa 
diag(cov.crabs)
plot(pca.crabs)
biplot(pca.crabs) #os scores estão padronizados
biplot(pca.crabs,pch=1,col=as.number)
abline(h=0,lwd=1,lty=2,col="blue")
pca.crabs$rotation[,1]
pca.crabs$rotation[1]
pca.crabs$x       #os scores não estão padronizados aqui

#Faz PCA com matriz de correlação
pca.crabs<-prcomp(crabs[,4:8], scale=T)  #resultados independentes da escala
pca.crabs
summary(pca.crabs) #autovaloes
round(pca.crabs$rotation,2) #autovetores #o coeficiente não significa 
diag(cov.crabs)
plot(pca.crabs)
biplot(pca.crabs) #os scores estão padronizados
biplot(pca.crabs,pch=1,col=as.number)
abline(h=0,lwd=1,lty=2,col="blue")
pca.crabs$rotation[,1]
pca.crabs$rotation[1]
pca.crabs$x       #os scores não estão padronizados aqui
cor(pca.crabs$x)

#Calcula os scores
autovetores<-pca.crabs$rotation
autovetores
class(autovetores)
dados<-as.matrix(crabs[,4:8])
scores.pca<-dados%*%autovetores #o sinal de % é devido à multiplicação de matrizes
var(scores.pca)  #os scores representam a variação total dos dados, mas a variação está redistribuída
sum(diag(var(scores.pca))
cor(scores.pca)
plot(scores.pca[,1],scores.pca[,2])
plot(scale(scores.pca[,1]),scale(scores.pca[,2]))


21/01/2013
##Análise de Correspondência
#-----------

install.packages("vegan",dependencies=T)
library(vegan)
data(dune)
head(dune)

#PCA
plot(prcomp(dune))
biplot(prcomp(dune))
cov(dune)
cor(dune)

#Calcula qui-quadrado
soma.linhas<-apply(dune,1,sum) #quando se coloca 1 no comando apply, o R soma linhas
soma.linhas #observções (indivíduos) por sítios
plot(sort(soma.linhas,decreasing=T),type="h",lwd=10,col="blue")

soma.colunas<-apply(dune,2,sum) #quando se coloca 2 no comando apply, O R soma linhas
soma.colunas #observções (indivíduos) por espécies
plot(sort(soma.colunas,decreasing=T),type="h",lwd=10,col="red")

soma.total<-sum(dune)
soma.total

f.esperado<-(soma.linhas%o%soma.colunas)/soma.total
f.esperado

qui.quadrado<-sum((dune-f.esperado)^2/f.esperado)
qui.quadrado

#Ca com vegan
dune.ca<-cca(dune)
dune.ca
summary(dune.ca)

qui.quadrado/soma.total #inertia

plot(dune.ca)
biplot(prcomp(dune),pc.biplot=T)
plot(dune.ca,scaling=1) #o scaling provoca mais uma mudança nas escalas dos dados, mas não modifica o reusltado da análise

library(MASS)
ca.dune<-corresp(dune)
ca.dune
biplot(ca.dune,nf=2)

install.packages("ca",dependencies=T)
1-pchisq(qui.quadrado,(19*20))
chisq.test(dune)

22/01/2013
## Análise de agrupamento (vamos tentar agrupar os caranguejos apenas por meio de suas medidas corporais)

#carrega pacotes
library(vegan)
library(MASS)

#Carrega os dados
data(crabs)
head(crabs)
obs<-sample(1:200,20) #seleciona 20 números ao acaso da amostra de 200 repetições sem reposição
obs
sort(obs) #ordena os valores
amostras<-c(12,21,23,24,28,41,47,54,70,76,91,92,93,101,112,115,131,146,177,197)
sum(amostras)
crabs.20<-crabs[amostras,]
crabs.20

#Visualiza 20 observações
pc.crabs.20<-prcomp(crabs.20[,4:8],scale=T)
pc.crabs.20
par(mfrow=c(1,2))
plot(pc.crabs.20$x[,1],pc.crabs.20$x[,2],pch=c("F","M")[as.numeric(crabs.20$sex)])
plot(pc.crabs.20$x[,1],pc.crabs.20$x[,2],col=c("blue","orange")[as.numeric(crabs.20$sp)])
par(mfrow=c(1,1))

#Calcula a matriz de distâncias
crabs.dist<-vegdist(crabs.20[,4:8],method="euclidean") #faz uma matriz de distâncias utilizando a distância euclidiana
round(crabs.dist,1)
crabs.dist
?vegdist
row.names(crabs) #lista o número das linhas

#Esperimenta diferentes açgoritmos
crabs.single<-hclust(crabs.dist,method="single") #hclust manda fazer um agrupamento hierárquico e manda fazer um dendrograma  #o método single é o método de agrupamento pelo vizinho mais próximo. Ele pergunta qual a menor distância entre dois caranguejos.
plot(crabs.single,pch)

crabs.complete<-hclust(crabs.dist,method="complete") #o complete manda fazer a menor distância ao vizinho mais distante
plot(crabs.complete)

crabs.UPGMA<-hclust(crabs.dist,method="average") #o average manda fazer a menor distância média
plot(crabs.UPGMA)

crabs.UPGMC<-hclust(crabs.dist,method="centroid") #faz a menor distância entre as distâncias dos centróides # mostra um encadeamento entre os grupos
plot(crabs.UPGMC)

crabs.ward<-hclust(crabs.dist,method="ward") #minimiza a variância entre as distâncias
plot(crabs.ward)

#Correlação cofrenética
crabs.single.cof<-cophenetic(crabs.single)
cor(crabs.dist,crabs.single.cof)

crabs.complete.cof<-cophenetic(crabs.complete)
cor(crabs.dist,crabs.complete.cof)

crabs.UPGMA.cof<-cophenetic(crabs.UPGMA)
cor(crabs.dist,crabs.UPGMA.cof)

crabs.UPGMC.cof<-cophenetic(crabs.UPGMC)
cor(crabs.dist,crabs.UPGMC.cof)

crabs.ward.cof<-cophenetic(crabs.ward)
cor(crabs.dist,crabs.ward.cof)

#Distância de Gower (somas de quadrados) -> é uma medida dos ajustes. Seria a soma dos quadrados dos resíduos. O melhor método seria aquele que tem o meor valor, pois representa que existe uma menor diferença entre o observado e o esperado.
gower.dist.simples<-sum((crabs.single.cof-crabs.dist)^2)
gower.dist.complete<-sum((crabs.complete.cof-crabs.dist)^2)
gower.dist.UPGMA<-sum((crabs.UPGMA.cof-crabs.dist)^2)
gower.dist.UPGMC<-sum((crabs.UPGMC.cof-crabs.dist)^2)
gower.dist.ward<-sum((crabs.ward.cof-crabs.dist)^2)

#Gráfico da largura da silhueta
library(cluster)
mls<-numeric(nrow(crabs.20))
mls
for (k in 2:(nrow(crabs.20)-1)){
    sil<-silhouette(cutree(crabs.UPGMA,k=k),crabs.dist)   
    mls[k]<-summary(sil)$avg.width
}
melhor.k<-which.max(mls) #indica qual é o valor máximo
melhor.k

kmeans(crabs.dist,centers=2) #irá dividir os dados em K grupos de 2 a 2 porque foi colocado centers=2 # esse teste é usado quando se tem um número pré-definido de grupos a serem formados, ou seja você indica para o programa quantos grupos você quer que sejam formados
crabs.cascade.KM<-
cascadeKM(crabs.dist,inf.gr=2,sup.gr=19,criterion="ssi")
?cascadeKM
plot(crabs.cascade.KM,sortg=T) #o aior valor de SSI indica a melhor forma de agrupamento
crabs.cascade.KM$results
crabs.cascade.KM$partition

  
23/01/2013

##Análise Multivariada de Variância (MANOVA)

#Carrega os dados
haynes<-read.table("haynes.dat",h=T)
head(haynes)
attach(haynes)
table(site)

#Examina os dados
pairs(haynes[,2:5],pch=c("D","S","H","W")[as.numeric(site)])
biplot(prcomp(haynes[,2:5]))

#Calcula média das variáveis
m<-colMeans(haynes[,-1])
m

#Cria matriz de médias
mm<-matrix(rep(m,12),12,4,byrow=T)
mm

#Cria matrix com médias dos sítios
delray<-apply(haynes[site=="Delray",-1],2,mean)   #o apply dá a média do grupo todo. Faz a média para todo o data.frame
delray

seaspray<-apply(haynes[site=="Seaspray",-1],2,mean) #2 é por coluna
seaspray

woodside<-apply(haynes[site=="Woodside",-1],2,mean)
woodside

mdelray<-matrix(rep(delray,4),4,4,byrow=T)
mdelray

mseaspray<-matrix(rep(seaspray,4),4,4,byrow=T)
mdelray

mwoodside<-matrix(rep(woodside,4),4,4,byrow=T)
mdelray

mg<-rbind(mdelray,mseaspray,mwoodside) #rbind junta as matrizes por linhas
mg

#Cria matriz dos dados originais
mh<-as.matrix(haynes[,-1])
mh

#Cria matriz dos desvios totais
dt<-mh-mm
dt

#Calcula matriz SSCP total
sscp.total<-t(dt)%*%dt #o t é de transposição
sscp.total
round(sscp.total,2)
cov(mh)

#Calcula os desvios das médias dos grupos
dg<-mg-mm
dg

#Calcula matriz SScp entre grupos
sscp.grupos<-t(dg)%*%dg
sscp.grupos

#Calcula desvios dentro de grupos
dd<-mh-mg
dd

#Calcula a matriz SSCP dentro de grupos
sscp.dentro<-t(dd)%*%dd
sscp.dentro

#Prova
sscp.total
sscp.grupos+sscp.dentro # esses dois valores tem que ser iguais. Se eles nãod erem iguais, existe alguma coisa errada.

#Calcula lambda de Wilk
det(sscp.dentro)/det(sscp.grupos+sscp.dentro) #quanto menor o valor de lambda de wilk, maior é o valor da diferença

#Faz MANOVA
mh<-as.matrix(haynes[,-1])
mh
mhaynes<-manova(mh~site) # mh tem que ser uma matriz. Não pode ser um data.frame
summary(mhaynes,test="Wilks") #resultado. Existe diferença entre os sites com relação aos metais pesados tomados simultaneamente.
print.default(summary(mhaynes))

#Verificações que o Guarino fez
sum(summary(mhaynes)$Eigenvalues)
sum(diag(sscp.grupos))
sum(diag(sscp.dentro))
sum(diag(cov(mh)))
var(lcu)

##Análise Discriminante

library(MASS)
haynes.discr<-lda(site~lcu+lpb+lni+lmn) #lda= linear discriminant analysis #é sempre importante padronizar as medidas dos dados com o scale, mesmo que os dados estejam com as mesmas unidades.
haynes.discr #a quantidade de direções de máxima variação vai ser sempre igual ao número de grupos -1.
             #porporção de traços seria a proporção de variação dos grupos que é explicada por cada dimensão
(haynes.discr<-lda(site~scale(lcu)+scale(lpb)+scale(lni)+scale(lmn)))

#Calcula grupos esperados
(haynes.discr.fit<-predict(haynes.discr)) #dá as probabilidades de cada observação a pertencer a esses sítios. A observação se encaixa naquela que tiver a maior probabilidade.
haynes.discr.fit$class

#Avalia a Eficiência do modelo
table(site,haynes.discr.fit$class) #verifica onde as variáveis foram classificadas

# Gráfico das funções discriminantes
plot(haynes.discr.fit$x[,1],haynes.discr.fit$x[,2],pch=("D","S","W")[as.numeric(site)])
plot(haynes.discr,dimen=2)

scores<-haynes.discr.fit$x
install.packages("car",dependecies=T)
library(car)
dataEllipse(scores[1:4,1],scores[1:4,2],levels=c(0.95),pch="D",center.pch=19,center.cex=1.5,robust=T,xlim=c(-8,8),ylim=c(-6,6),col="blue",xlab="LDA'",ylab="LDA2",las=1,bty="n")
dataEllipse(scores[5:8,1],scores[5:8,2],levels=c(0.95),pch="D",center.pch=19,center.cex=1.5,robust=T,xlim=c(-8,8),ylim=c(-6,6),col="red",xlab="LDA'",ylab="LDA2",las=1,add=T)
dataEllipse(scores[9:12,1],scores[9:12,2],levels=c(0.95),pch="D",center.pch=19,center.cex=1.5,robust=T,xlim=c(-8,8),ylim=c(-6,6),col="darkgreen",xlab="LDA'",ylab="LDA2",las=1,add=T)

#pode-se olhar também a matriz de confusão para verificar o quão bom é o modelo.
#Jacknife 
#Bootstrap

24/01/2013
##Análise de Correlação Canônica
#.............

bolger<-read.table("bolgeretal997.txt",h=T)
head(bolger)
pairs(bolger[,-1],col="blue")
biplot(prcomp(bolger[,-1])) #precisa dar o scale pq os dados não estão padronizados
biplot(prcomp(bolger[,-1],scale=T))

install.packages("calibrate",dependencies=T)
library(calibrate)
ccora.bolger<-canocor(bolger[,5:13],bolger[,2:4])
ccora.bolger

$ccor #Correlações Canônicas (matriz de correlação canônica dos dados bolger)
          [,1]      [,2]      [,3]   #esses dados sugerem que a correlação entre esses conjuntos de dados ocorrem em mais de uma dimensão
[1,] 0.9529602 0.0000000 0.0000000
[2,] 0.0000000 0.7635846 0.0000000
[3,] 0.0000000 0.0000000 0.6248061

$A #Coeficientes canônicos dos roedores  (são 9 linhas pq são nove espécies de roedores e 3 colunas pq são 3 variáveis)
              [,1]        [,2]       [,3]
 [1,] -0.002850132 -0.80107782 -0.6845661
 [2,] -0.002584390  0.09098132 -0.8481207
 [3,]  0.568198932 -0.01610844 -0.6409029
 [4,]  0.640052022 -0.20159616 -1.4474521
 [5,] -1.229873547  0.18930943  1.6412169
 [6,]  1.109863062 -1.16609969 -0.8237638
 [7,]  0.576551311 -0.99234711 -0.6063756
 [8,] -0.076840527 -0.38966869 -0.4752606
 [9,] -0.289454443  1.77032288  1.5435431

$B #Coeficientes canonicos das variáveis dos fragmentos
           [,1]       [,2]       [,3]
[1,]  0.9414336  0.3784044 -0.1252203
[2,] -0.1670719 -0.1777184 -1.0109448
[3,] -0.1627896  1.0357372  0.1407953

$U #Scores das variáveis canônicas dos roedores #tem um score para cada fragmento
             [,1]       [,2]        [,3]
 [1,]  3.46144486  0.9683528 -1.26511715
 [2,]  0.82428660 -0.2552042 -0.40835272
 [3,]  1.88437380  0.8483081  1.03181004
 [4,]  0.50575664 -1.1734271  0.28701547
 [5,]  1.09758644 -0.6455618  1.34274722
 [6,] -0.58980604  1.1140182 -0.83155452
 [7,] -0.59288227  1.2223142 -1.84108106
 [8,] -0.59602820 -1.2185761 -2.16364482
 [9,] -0.58695814  0.1632132  0.33965784
[10,] -0.58857555  1.0706998 -0.42774391
[11,] -0.05205695 -1.8155107  0.33247200
[12,]  0.68641742 -0.5925010  0.03159995
[13,] -0.58757338  0.1848724  0.13775253
[14,] -0.58980604  1.1140182 -0.83155452
[15,] -0.58549933  0.9624038  0.58178263
[16,] -0.59110624 -1.3918497 -0.54840236
[17,] -0.09612707 -1.0631950  1.22836765
[18,] -0.56093285  0.8751404  1.36446986
[19,]  0.04805559 -1.4220977 -1.31731064
[20,] -0.58903219 -0.6143182 -0.10437226
[21,] -0.58488408  0.9407446  0.78368794
[22,] -0.14481077 -0.3758018  1.15442504
[23,] -0.58572765  0.1198948  0.74346846
[24,] -0.58611457  0.9840630  0.37987733

$V #Scores dos fragmentos
             [,1]       [,2]        [,3]
 [1,]  3.23558008  0.5996640 -0.62470123
 [2,]  1.60165088  0.4215704 -1.39551863
 [3,]  1.83075480  0.7253968  0.55259694
 [4,]  0.62371458 -0.6054961 -0.16854644
 [5,]  1.08798213 -0.3997709  0.72845554
 [6,] -0.65531390  1.4328634 -0.72124901
 [7,] -0.49483235  0.6599947 -0.37600595
 [8,] -0.73407704 -0.6084852 -2.71739435
 [9,] -0.21731221 -0.2876025  0.22173028
[10,] -0.43130268  1.4874293  0.86779860
[11,] -0.10199325 -1.0681602  0.28069243
[12,] -0.03636814 -0.6708010  1.00812234
[13,] -0.38252638  0.7558714  0.86328714
[14,] -0.74239186 -0.4988185 -2.05034848
[15,] -0.17769850 -0.5442266  0.79018990
[16,] -0.30923976 -0.7034586  0.20387926
[17,] -0.10127221 -1.1223122  0.94206634
[18,] -0.99187144  1.5617373 -0.93859382
[19,] -0.33932455 -1.4412063 -0.33191617
[20,] -0.35305747 -0.7977037  0.27374755
[21,] -0.65229070  1.5297332  0.95232229
[22,] -0.22949922 -1.4316773  0.89028378
[23,] -0.60271800 -0.4317551 -0.01653333
[24,] -0.82659283  1.4372139  0.76563502

$Fs #Correlações entre variáveis canônicas dos roedores e abundâncias das espécies dos roedores
              [,1]          [,2]        [,3]
rrattus -0.2607352 -5.275054e-01 -0.45438755
mmus    -0.2512870  4.783745e-01 -0.56533023
pcalif   0.7296323 -2.694857e-01  0.19212401
perem    0.7932048  4.349564e-02 -0.08477698
rmegal   0.6741737 -3.364700e-01  0.31011186
nfusc    0.7537739 -2.101649e-01  0.29114924
nlepid   0.6159802  1.206470e-06  0.09032640
pfallax  0.4681222 -2.003904e-02  0.31518707
mcalif   0.8646334  1.972227e-01  0.09416046

$Gs #Correlações entre as variáveis canônicas dos fragmentos e as variáveis dos fragmentos
            [,1]      [,2]        [,3]
area   0.9654196 0.1776720 -0.19078193
distx -0.1728344 0.1059498 -0.97923589
age   -0.3823700 0.9187633 -0.09832173

$Fp  #Correlações entre variáveis canônicas dos fragmentos e variáveis dos roedores (abundância dos roedores nesse caso_
              [,1]          [,2]        [,3]
rrattus -0.2484703 -4.027950e-01 -0.28390412
mmus    -0.2394665  3.652794e-01 -0.35322179
pcalif   0.6953105 -2.057751e-01  0.12004026
perem    0.7558926  3.321260e-02 -0.05296918
rmegal   0.6424607 -2.569233e-01  0.19375979
nfusc    0.7183165 -1.604787e-01  0.18191183
nlepid   0.5870045  9.212415e-07  0.05643649
pfallax  0.4461018 -1.530150e-02  0.19693081
mcalif   0.8239612  1.505962e-01  0.05883203

$Gp #Correlações entre variáveis canônicas dos roedores e variáveis dos fragmentos
            [,1]       [,2]        [,3]
area   0.9200064 0.13566758 -0.11920172
distx -0.1647043 0.08090161 -0.61183258
age   -0.3643833 0.70155352 -0.06143202

$fitRxy #Autovalores da matriz de correlação canônica
          [,1]      [,2]      [,3]
lamb 0.9081331 0.5830614 0.3903827
frac 0.4826446 0.3098791 0.2074763
cumu 0.4826446 0.7925237 1.0000000

$fitXs #Proporção da variação dos roedores explicada por suas variáveis canônicas
           [,1]       [,2]       [,3]
AdeX  0.4068346 0.08647758 0.09638558
cAdeX 0.4068346 0.49331218 0.58969776

$fitXp #Proporção da variação dos parâmetros dos roedores explicados pos suas variáveis can}onicas
           [,1]       [,2]       [,3]
RedX  0.3694599 0.05042174 0.03762726
cRedX 0.3694599 0.41988169 0.45750896

$fitYs #Proporção da variação dos fragmentos explicada pelas suas variáveis canônicas  # Essa soma dá um porque foram geradas 3 variáveis canônicas,e são 3 variáveis medidas (área, idade e distância)
           [,1]      [,2]      [,3]
AdeY  0.3693711 0.2956396 0.3349893
cAdeY 0.3693711 0.6650107 1.0000000

$fitYp #Proporção da variação dos fragmentos explicada pelas variáveis canônnicas dos roedores
           [,1]      [,2]      [,3]
RedY  0.3354382 0.1723760 0.1307740
cRedY 0.3354382 0.5078142 0.6385882

cor(ccora.bolger$U[,1],ccora.bolger$V[,1])
plot(cor(ccora.bolger$U[,1],ccora.bolger$V[,1]))
 
##Análise de redundância
#...............

install.packages("vegan",dependencies=T)
library(vegan)
especies<-bolger[,5:13]
ambientes<-bolger[,2:4]

bolger.rda<-rda(especies~area+distx+age,ambientes) #diz a proporção da variação explicada
summary(bolger.rda)

sum(diag(cov(especies)))

Call:
rda(formula = especies ~ area + distx + age, data = ambientes) 

Partitioning of variance: #dá a variância explicada e não explicada pelo modelo
              Inertia Proportion
Total          1015.0     1.0000
Constrained     547.9     0.5399
Unconstrained   467.0     0.4601

Eigenvalues, and their contribution to the variance 

Importance of components:
                          RDA1     RDA2    RDA3      PC1      PC2     PC3
Eigenvalue            527.0849 19.72285 1.13755 314.4636 111.6700 22.3245
Proportion Explained    0.5193  0.01943 0.00112   0.3098   0.1100  0.0220
Cumulative Proportion   0.5193  0.53874 0.53986   0.8497   0.9597  0.9817
                           PC4     PC5     PC6     PC7     PC8     PC9
Eigenvalue            11.71459 3.66700 2.39675 0.43059 0.35389 0.01212
Proportion Explained   0.01154 0.00361 0.00236 0.00042 0.00035 0.00001
Cumulative Proportion  0.99324 0.99685 0.99922 0.99964 0.99999 1.00000

Accumulated constrained eigenvalues #Da variação explicada pelo modelo, o primeiro componente explicou  ~96% da variação
Importance of components:
                          RDA1     RDA2    RDA3
Eigenvalue            527.0849 19.72285 1.13755
Proportion Explained    0.9619  0.03599 0.00208
Cumulative Proportion   0.9619  0.99792 1.00000

Scaling 2 for species and site scores
* Species are scaled proportional to eigenvalues
* Sites are unscaled: weighted dispersion equal on all dimensions
* General scaling constant of scores:  12.36079 


Species scores #As RDA1 e RDA2 seriam as coordenadas de onde as setas de cada uma das espécies e ambientes ficariam no gráfico

            RDA1     RDA2     RDA3       PC1      PC2      PC3
rrattus  0.07988 -0.07880  0.19725 -0.080380  0.01562 -0.11629
mmus     0.57056  0.70957  0.09537  0.037338 -0.23580  0.22904
pcalif  -7.29980 -0.65314  0.11829  6.755613  0.05786 -0.28983
perem   -4.23198  1.38025 -0.01228  0.126485  3.78060  0.54527
rmegal  -1.08938 -0.25134 -0.01981  0.803901  0.41885  0.39708
nfusc   -2.37688 -0.19987 -0.20664  1.002511 -1.05480  1.41736
nlepid  -0.64587  0.11616 -0.06966 -0.014479  0.69413  0.06516
pfallax -0.69705 -0.03979 -0.24025  0.154618 -0.82892  0.85514
mcalif  -0.27641  0.09688 -0.05854 -0.002588  0.03280  0.10576


Site scores (weighted sums of species scores)

         RDA1     RDA2     RDA3      PC1      PC2     PC3
row1  -7.3942  17.4030   3.0263 -1.89682  5.63820  0.5606
row2  -1.5748  -6.7985  11.1934 -0.60556 -5.43257 -6.4236
row3  -4.0310 -11.8844 -29.1030  1.04486 -7.35372  4.5220
row4  -5.5266  -4.6866  17.7369  4.88298  1.92188  1.4263
row5  -4.7967   5.8446   6.9814  1.08020  5.07918  0.4559
row6   2.0847   3.5694   1.3153  0.73149  0.15569  0.8167
row7   2.1206   4.7643   4.0999 -0.48965 -0.09848  0.4231
row8   2.0661   2.7727   3.1002  0.28134  0.14796  1.5899
row9   2.0209   1.3918  -2.5452 -2.31132 -0.27071 -1.8256
row10  2.0703   3.0914   0.2014 -0.61338  0.05766 -0.7905
row11  1.3257  -0.1568 -32.1567 -2.68765 -1.65719  6.8587
row12 -1.9681  -7.8297   9.4141  2.39511 -0.99014 -0.4702
row13  2.0281   1.6308  -1.9883 -1.33492  0.12480 -1.4233
row14  2.0847   3.5694   1.3153  0.07587  0.27713  1.6437
row15  2.0343   1.8964  -2.5833 -2.89708 -0.21384 -2.0454
row16  2.0086   0.8608  -1.3553 -2.38593  0.02389 -1.7105
row17 -5.7480 -17.5835  26.5216  7.96750 -0.80686 -1.1546
row18  1.9136   0.7204  -4.1202  2.00025  1.02537  1.2636
row19 -0.5288  -4.6141  17.2245  1.22448  0.37357 -3.5600
row20  2.0148   1.1263  -1.9503 -2.37175  0.14122 -1.4858
row21  2.0271   1.6574  -3.1402 -0.01687  0.67253 -0.4559
row22  1.7209   0.2057 -17.5023 -3.30341 -0.44497  1.9174
row23  2.0066   0.9138  -3.6591 -1.25112  0.61392 -0.5051
row24  2.0415   2.1354  -2.0263  0.48138  1.01546  0.3725


Site constraints (linear combinations of constraining variables)

          RDA1    RDA2     RDA3      PC1      PC2     PC3
row1  -7.59028  4.1124  0.05230 -1.89682  5.63820  0.5606
row2  -3.34935  3.9802  2.02453 -0.60556 -5.43257 -6.4236
row3  -4.37828  1.6635 -2.41924  1.04486 -7.35372  4.5220
row4  -1.82565 -0.5421  1.25760  4.88298  1.92188  1.4263
row5  -3.16326 -1.2626 -0.92179  1.08020  5.07918  0.4559
row6   2.61873  3.5340 -0.77470  0.73149  0.15569  0.8167
row7   1.70324  1.5794 -0.25304 -0.48965 -0.09848  0.4231
row8   2.37637  2.6191  6.52579  0.28134  0.14796  1.5899
row9   0.32355 -1.0413  0.02729 -2.31132 -0.27071 -1.8256
row10  1.58525  1.2888 -4.09409 -0.61338  0.05766 -0.7905
row11 -0.39260 -2.5902  1.14394 -2.68765 -1.65719  6.8587
row12 -0.57822 -2.9121 -0.96665  2.39511 -0.99014 -0.4702
row13  1.08139 -0.1052 -2.92195 -1.33492  0.12480 -1.4233
row14  2.24424  1.7843  5.00207  0.07587  0.27713  1.6437
row15 -0.08933 -2.4091 -0.71652 -2.89708 -0.21384 -2.0454
row16  0.34155 -1.8824  0.73396 -2.38593  0.02389 -1.7105
row17 -0.63136 -3.7311 -0.10773  7.96750 -0.80686 -1.1546
row18  3.59747  3.9223 -0.51404  2.00025  1.02537  1.2636
row19  0.19947 -2.5046  2.99699  1.22448  0.37357 -3.5600
row20  0.37987 -2.2026  0.74639 -2.37175  0.14122 -1.4858
row21  2.13407  1.1053 -4.31514 -0.01687  0.67253 -0.4559
row22 -0.45599 -4.3328  0.50058 -3.30341 -0.44497  1.9174
row23  1.28820 -1.1839  0.76961 -1.25112  0.61392 -0.5051
row24  2.58091  1.1109 -3.77616  0.48138  1.01546  0.3725


Biplot scores for constraining variables

         RDA1   RDA2     RDA3 PC1 PC2 PC3
area  -0.8784 0.4778  0.01027   0   0   0
distx  0.3093 0.6345  0.70833   0   0   0
age    0.5702 0.6669 -0.47971   0   0   0

> 
coef(bolger.rda)
plot(bolger.rda)
plot(bolger.rda,scaling=1)
anova.cca(bolger.rda,step=1000) #teste de significância do modelo com aleatorização (a aleatorização é necessária)
anova.cca(bolger.rda,by="axis",step=1000)
anova.cca(bolger.rda,by="terms",step=1000)
m.nula<-rda(especies~1,ambientes)
m.complete<-rda(especies~area+distx+age,ambientes)
add1(m.nula,scope=m.complete,test="perm",pstep=1000)
m.nula<-update(m.nula,.~.+area)
add1(m.nula,scope=m.complete,test="perm",pstep=1000)
m.nula<-update(m.nula,.~.+age)
add1(m.nula,scope=m.complete,test="perm",pstep=1000)

ordistep(m.nula,scope=formula(m.complete),direction="forward",pstep=1000)
ordistep(m.nula,scope=formula(m.complete),direction="both",pstep=1000)

25/01/2013

#Aula 14 - Análise de Correspondência Canônica (CCA)

bolger<-read.table("Bolgeretal1997.txt",h=T)
head(bolger)
str(bolger)

# Examina os dados
pairs(bolger[,2:13],pch=1,col="blue")
biplot(prcomp(bolger[,2:13])) #efeito da escala das variaveis
biplot(prcomp(bolger[,2:13],scale=T))


## Analise de Correspondencia Canonica
#  **********************
install.packages("vegan",dependencies=T)
library(vegan)
especies<-bolger[,5:13]
ambiente<-bolger[,2:4]

bolger.cca<-cca(especies~area+distx+age,ambiente)
bolger.cca
summary(bolger.cca)

sum(diag(cov(species)))

# Coeficientes canonicos para cada X
coef(bolger.cca)

# Faz grafico
plot(bolger.cca)
plot(bolger.cca,scaling=1)

# Teste da significancia global da cca
anova.cca(bolger.cca,step=1000)

# Teste da significancia dos eixos da cca
anova.cca(bolger.cca,by="axis",step=1000)

# Teste da significancia dos Xs da cca
anova.cca(bolger.cca,by="terms",step=1000)

# Variance inflation factors (colinearidade)
#proporcao da variancia de um coeficiente de regressao que e inflada na presenca de 
outras variaveis independentes
vif.cca(bolger.cca)

# Selecao de modelos
m.nulo<-cca(especies~1,ambiente)
m.completo<-cca(especies~area+distx+age,ambiente)
add1(m.nulo,scope=m.completo,test="perm",pstep=1000)
m.nulo<-update(m.nulo,.~.+area)
add1(m.nulo,scope=m.completo,test="perm",pstep=1000)
m.nulo<-update(m.nulo,.~.+age)
drop1(m.nulo,test="perm",pstep=1000)
add1(m.nulo,scope=m.completo,test="perm",pstep=1000)
m.nulo<-cca(especies~1,ambiente)
ordistep(m.nulo,scope=formula(m.completo),direction="forward",pstep=1000)
ordistep(m.nulo,scope=formula(m.completo),direction="both",pstep=1000)

ordirgl(bolger.cca,type="t",scaling=1)
orglspider(bolger.cca,scaling=1,color="cyan")
ordirgl(bolger.cca,type="t",scaling=1)
orgltext(bolger.cca,display="species",type="t",scaling=2,col="purple")

#Hotelling test (Teste t mulivariado)
## Carregar pacote Hotelling
fit<-hotelling.test(cc+ecto~preservacao, data = slil, perm = T,B = 10000) #Hotelling com permutações #data=aos dados que eu estou testando
 