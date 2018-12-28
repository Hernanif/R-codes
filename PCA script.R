#### An?lise de Componentes Principais
#**********************************

#Carrega os dados
library(MASS)
data(crabs)
attach(crabs)
head(crabs)

#Visualiza??o de Dados Multivariados
pairs(crabs[,4:8],pch=19,col="red")

#Matrizes de correla??o e covariancia
cor(crabs[,4:8])
cor(crabs[,c(4,5,8)]) #correlaciona vari?veis espec?ficas

#Matrizes de correla??o e covari?ncia
cov.crabs<-cor(crabs[,4:8])
round(cov.crabs,2)
sum(diag(cov.crabs)) #a diagonal principal tem as vari?ncias e os outros dados s?o as covari?ncias.
sscp.crabs<-cov.crabs*199
round(sscp.crabs,2)
sum(sscp.crabs)
sum(diag(sscp.crabs))
round(cor(crabs[,4:8]),2) #numa matriz de 5 x 5, existem 10 correla??es importantes a serem verificadas.
diag(sscp.crabs)

#Faz PCA
pca.crabs<-prcomp(crabs[,4:8])
pca.crabs
summary(pca.crabs) #autovaloes
round(pca.crabs$rotation,2) #autovetores #o coeficiente n?o significa 
diag(cov.crabs)
plot(pca.crabs)
biplot(pca.crabs) #os scores est?o padronizados
biplot(pca.crabs,pch=1,col=as.number)
abline(h=0,lwd=1,lty=2,col="blue")
pca.crabs$rotation[,1]
pca.crabs$rotation[1]
pca.crabs$x       #os scores n?o est?o padronizados aqui

#Faz PCA com matriz de correla??o
pca.crabs<-prcomp(crabs[,4:8], scale=T)  #resultados independentes da escala
pca.crabs
summary(pca.crabs) #autovaloes
round(pca.crabs$rotation,2) #autovetores #o coeficiente n?o significa 
diag(cov.crabs)
plot(pca.crabs)
biplot(pca.crabs) #os scores est?o padronizados
biplot(pca.crabs,pch=1,col=as.number)
abline(h=0,lwd=1,lty=2,col="blue")
pca.crabs$rotation[,1]
pca.crabs$rotation[1]
pca.crabs$x       #os scores n?o est?o padronizados aqui
cor(pca.crabs$x)

#Calcula os scores
autovetores<-pca.crabs$rotation
autovetores
class(autovetores)
dados<-as.matrix(crabs[,4:8])
dados
scores.pca<-dados%*%autovetores #o sinal de % ? devido ? multiplica??o de matrizes
var(scores.pca)  #os scores representam a varia??o total dos dados, mas a varia??o est? redistribu?da
sum(diag(var(scores.pca)))
cor(scores.pca)
plot(scores.pca[,1],scores.pca[,2])
plot(scale(scores.pca[,1]),scale(scores.pca[,2]))

