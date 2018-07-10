############################################################################
#                                                                          # 
#    SCRIPT PARA ESTIMAR A SIGNIFIC??NCIA DE METRICAS DE REDES COMPLEXAS   #
#                                                                          # 
############################################################################

##### Laboratorio de Sintese Ecologica (SintECO)
##### www.marcomello.org
##### Autora 1: Renata Muylaert
##### E-mail: renatamuy@gmail.com
##### Autor 2: Pavel Dodonov
##### E-mail: pdodonov@gmail.com
##### Nome: Script para estimar a significancia de metricas de redes complexas
##### Formula para citacao do script: Muylaert RL & Dodonov P. 2016. Script para
##### estimar a significancia de metricas de redes complexas. 
##### Disponivel em www.marcomello.org. Acesso em DD-MM-20XX
##### Publicado em 16/07/2016.
##### Rodado em R 3.3.1 Bug in your hair.
##### Colaboradores: Marco Mello, Gabriel Felix.
##### Aviso: este script pode ser usado ?? vontade, por??m n??o nos responsabilizamos
##### pela forma como for usado por terceiros, nem por eventuais problemas causados
##### pelo seu uso. A fonte deve ser sempre citada. Caso encontre algum bug, por 
##### favor informe os autores. Se este script ajudar voc?? em algum trabalho 
##### acad??mico (TCC, monografia, disserta????o, tese, artigo, livro, cap??tulo, p??ster,
##### palestra etc.), por favor, mencione isso nos agradecimentos.


#############################################################


####SUMARIO##################################################


#1. Preparativos
#2. Estimativa de significancia para uma rede (Dormann)
#3. Estimativa de significancia para uma rede (Muylaert & Dododonov)
#4. Estimativa de significancia da modularidade QuanBiMo
#5. Comparacao de uma mesma metrica entre duas redes
#6. Comparacao de modularidade QuanBiMo entre duas redes
#7. Referencias e leituras sugeridas


#############################################################


####1. PREPARATIVOS##########################################

#Limpe os objetos criados anteriormente
rm(list= ls())

#Carregue os pacotes necessarios. Na pratica, bipartite e vegan sao suficientes.
library(bipartite)
library(reshape2)
library(sna)
library(igraph)
library(network)
library(tcltk)
library(vegan)
library(network)

#Defina o diretorio de trabalho
setwd("caminho da pasta com os seus arquivos")
#setwd("/Users/Marco/Desktop/Dropbox/Publicacoes/Trabalhando/Isabel Rede Noturna/R/Renata Monte Carlo")
#Crie o objeto a ser analisado. 
#Substitua "rede" pelo nome do arquivo que deseja analisar.
dados<-read.table("rede1.txt", head=TRUE)
dados


##############################################################


####2. ESTIMATIVA DE SIGNIFICANCIA PARA UMA REDE (DORMANN)####


#Rede binaria

#Transforme sua rede em binaria
dados<- ifelse(dados==0,0,1)
dados

#Crie o objeto a ser analisado
data=dados
data

#Calcule a metrica desejada para a rede real (observada)
obs <- unlist(networklevel(data, index="nestedness"))
obs

#Crie as redes aleatorizadas de acordo com o modelo nulo (method) de sua escolha,
#escolhendo o numero de permutacoes (N). Pode demorar.
nulls <- nullmodel(data, N=10, method=5, autotransform="equiprobable")

#Mande calcular essa mesma metrica para todas as redes aleatorizadas
null <- unlist(sapply(nulls, networklevel, index="nestedness")) 
null

#Plote o histograma com os valores da metrica escolhida para as
#redes aleatorizadas e a sua rede
plot(density(null), xlim=c(min(obs, min(null)), max(obs, max(null))), 
     main="Comparacao da rede observada com o modelo nulo", xlab = "Sua metrica escolhida")
abline(v=obs, col="red", lwd=2)    

#Calcule o valor de P, ou seja, a significancia da metrica escolhida
mean(null)
sd(null)
obs
praw <- sum(null>obs) / length(null)
ifelse(praw > 0.5, 1-praw, praw)    # P-value


#########


#Rede ponderada

#Limpe os objetos criados anteriormente
rm(list= ls())

#Crie o objeto a ser analisado
dados<-read.table("laselvanet9.12.2016.txt", head=TRUE)
data=dados
data

#Calcule a m??trica desejada para a rede real (observada)
obs <- unlist(networklevel(data, index="number of compartments"))


#Crie as redes aleatorizadas de acordo com o modelo nulo (method) de sua escolha,
#escolhendo o numero de permutacoes (N)
nulls <- nullmodel(data, N=10, method=2) #demora
nulls

#Mande calcular esse mesmo i?ndice para todas as redes aleatorizadas
null <- unlist(sapply(nulls, networklevel, index="number of compartments")) #demora
null

#Plote o histograma com os valores da m??trica escolhida para as
#redes aleatorizadas e a sua rede
plot(density(null), xlim=c(min(obs, min(null)), max(obs, max(null))), 
     main="Comparacao da rede observada com o modelo nulo", xlab = "Sua metrica escolhida")
abline(v=obs, col="red", lwd=2)    

#Calcule o valor de P, ou seja, a significancia da metrica escolhida
mean(null)
sd(null)
obs
praw <- sum(null>obs) / length(null)
ifelse(praw > 0.5, 1-praw, praw)    # P-value


#########################################################################


####3. ESTIMATIVA DE SIGNIFICANCIA PARA UMA REDE (MUYLAERT & DODONOV)####

#Limpe os objetos criados anteriormente
rm(list= ls())

#Rede binaria
dados<-read.table("rede1.txt", head=TRUE)
dados

#Se a sua rede for ponderada, transforme-a em binaria
dados<- ifelse(dados==0,0,1)

dados_neo=as.matrix(dados)

dados_neo

#Modelo nulo 2 de Bascompte et al. (2003)

#Voce pode trocar por uma metrica especifica. Por exemplo: metrics=c("H2")
metrics=c("ALLBUTDD")

#Salve os valores da rede original
orig=networklevel(dados_neo, index=metrics)

orig

real<-data.frame(orig)

real

#Se quiser, exporte os valores para um arquivo TXT
write.table(real, file=paste("Real_web3.txt",sep=""), 
            sep=" ",row.names=TRUE,col.names=FALSE)

#Crie as redes aleatorizadas com base no modelo nulo.
#Pode trocar pelo numero de permutacoes que quiser. Costuma fica confi??vel a partir de 999 
Nperm = 9
randomized.basc=matrix(nrow=length(orig),ncol=Nperm+1)
row.names(randomized.basc)=names(orig)
randomized.basc[,1]=orig #O valor original e incluido entre os aleatorizados.

#Crie um loop para repetir o procedimento de calculo
i=1
while(i <= Nperm){ 

	dados_aleat=permatfull(dados_neo,fixedmar="both",mtype="prab",times=1)

	dados_aleat=dados_aleat$perm[[1]]

	linha<-networklevel(dados_aleat, index=metrics)

	randomized.basc[,i+1]=linha
	
	print(i)
	
	i<-i+1
  }

randomized.basc

#####Plote e exporte as figuras com a comparacao entre valores reais e aleatorizados
niveis<-row.names(randomized.basc)
for(k in niveis)
	{
		if(any(is.na(randomized.basc[k,]) == TRUE)) {
			print(c(k,"metrica tem NA"))
			} else {
	nome.arq<- paste("Bascompte_null_",k,".png", sep="")
	png(filename= nome.arq, res= 300, height= 15, width=21, unit="cm")
	plot(density(randomized.basc[k,]), main="Comparacao entre o valor real
	     e os aleatorizados",)
	abline(v=orig[k], col="red", lwd=2, xlab="")
	dev.off()
	print(k)
	nome.arq<- paste("Bascompte_Null_mean_sd_",k,".txt", sep="")
	write.table(cbind(mean(randomized.basc[k,]),sd(randomized.basc[k,])), file=paste(nome.arq,sep=""), 
      sep=" ",row.names=TRUE,col.names=FALSE)
		}
	}

  
#Calcule a proporcao de valores aleatorizados que cairam
#acima ou abaixo do valor real => valor de P
significance.basc=matrix(nrow=nrow(randomized.basc),ncol=3)
row.names(significance.basc)=row.names(randomized.basc)
colnames(significance.basc)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

#Estime a significancia bicaudal
signif.sup=function(x) sum(x>=x[1])/length(x)
signif.inf=function(x) sum(x<=x[1])/length(x)
signif.two=function(x) ifelse(min(x)*2>1,1,min(x)*2)

significance.basc[,1]=apply(randomized.basc,1,signif.inf)
significance.basc[,2]=apply(randomized.basc,1,signif.sup)
significance.basc[,3]=apply(significance.basc[,-3],1,signif.two)

significance.basc

#Exporte os valores de P para um arquivo TXT
write.table(significance.basc, file=paste("Significance_Bascompte2.txt",sep=""), 
            sep=" ",row.names=TRUE,col.names=TRUE)


#########


#Rede ponderada

#Modelo nulo de Patefield (1981)

#Limpe os objetos criados anteriormente
rm(list= ls())

#Carregue a rede original ponderada
dados<-read.table("rede1.txt", head=TRUE)
dados_neo=as.matrix(dados)
dados_neo

#Voce pode trocar por uma metrica especifica. Por exemplo: metrics=c("H2")
metrics=c("ALLBUTDD") #todas!

#Salve os valores da rede original
orig=networklevel(dados_neo, index=metrics)
orig
real<-data.frame(orig)
real

#Se quiser, exporte os valores para um arquivo TXT
write.table(real, file=paste("Real_web_w.txt",sep=""), 
            sep=" ",row.names=TRUE,col.names=FALSE)

#Crie as redes aleatorizadas com base no modelo nulo.
#Pode trocar pelo numero de permuta????es que quiser. Costuma ficar confiavel a partir de 999.
Nperm = 9
randomized.patef=matrix(nrow=length(orig),ncol=Nperm+1)
row.names(randomized.patef)=names(orig)
randomized.patef[,1]=orig 

#Crie um loop para repetir o procedimento de calculo
i<-1
while(i<=Nperm){ 

  dados_aleat=permatfull(dados_neo,fixedmar="both",mtype="count",times=1)
  
  dados_aleat=dados_aleat$perm[[1]]
  
  linha<-networklevel(dados_aleat, index=metrics)
  
  randomized.patef[,i+1]=linha
  print(i)
	i=i+1
}
 randomized.patef

#####Plote e exporte as figuras com a comparacao entre valores reais e aleatorizados
niveis<-row.names(randomized.patef)
for(k in niveis)
	{
		if(any(is.na(randomized.patef[k,]) == TRUE))
			{
			print(c(k, "metrica tem NA"))
			} else {
	nome.arq<- paste("Patefield_null_",k,".png", sep="")
	png(filename= nome.arq, res= 300, height= 15, width=21, unit="cm")
	plot(density(randomized.patef[k,]), main="Comparacao entre o valor real e
	     os aleatorizados",)
	abline(v=orig[k], col="red", lwd=2, xlab="")
	dev.off()
	print(k)
	nome.arq<- paste("Patefield_Null_mean_sd_",k,".txt", sep="")
	write.table(cbind(mean(randomized.patef[k,]),sd(randomized.patef[k,])), file=paste(nome.arq,sep=""), 
      sep=" ",row.names=TRUE,col.names=FALSE)
		}
	}

#Calcule a proporcao de valores aleatorizados que cairam
#acima ou abaixo do valor real => valor de P
significance.patef=matrix(nrow=nrow(randomized.patef),ncol=3)
row.names(significance.patef)=row.names(randomized.patef)
colnames(significance.patef)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

signif.sup=function(x) sum(x>=x[1])/length(x)
signif.inf=function(x) sum(x<=x[1])/length(x)
signif.two=function(x) ifelse(min(x)*2>1,1,min(x)*2)

significance.patef[,1]=apply(randomized.patef,1,signif.inf)
significance.patef[,2]=apply(randomized.patef,1,signif.sup)
significance.patef[,3]=apply(significance.patef[,-3],1,signif.two)

significance.patef

#Exporte os valores de P para um arquivo TXT
write.table(significance.patef, file=paste("Significance_Patefield.txt",sep=""), 
            sep=" ",row.names=TRUE,col.names=TRUE)


##############################################################


####4. ESTIMATIVA DE SIGNIFICANCIA DA MODULARIDADE QUANBIMO####

#Dormann & Strauss (2014)

#Limpe os objetos criados anteriormente
rm(list= ls())

#Carregue a rede original ponderada
dados<-read.table("rede1.txt", head=TRUE)
dados

#Rode o algoritmo QuanBiMo
mod <- computeModules(web=dados, steps=1E6)
mod

#Calcule e mostre o valor de M
mod@likelihood



#Plote a matriz com os modulos e exporte o arquivo como imagem
file_name= paste("Modulos_Quanbimo.png", sep="")
png(filename= file_name, res= 300, height= 25, width=30, unit="cm")
plotModuleWeb(mod)
dev.off()


#Calcule os modulos hierarquicos
modn <- computeModules(dados, steps=1E6, deep=T) 

#Estime a significancia de M com o algoritmo Patefield
#Modelo nulo de Patefiled (1981)
nulls <- nullmodel(dados, N=10, method="r2d") #Pode trocar por quantas permutacoes quiser
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
(z <- (mod@likelihood - mean(like.nulls))/sd(like.nulls))
czvalues(mod) # for all species
czvalues(mod, level="lower") # for lower trophic level
czvalues(mod, level="lower", weighted=TRUE) #based on strength

#Calcule a proporcao de valores aleatorizados que cai?ram
#acima ou abaixo do valor real

#Plote e exporte a distribuicao da metrica aleatorizada
plot(density(like.nulls), xlim=c(min((mod@likelihood), min(like.nulls)), max((mod@likelihood), max(like.nulls))), 
     main="comparison of observed with null model Patefield")
abline(v=(mod@likelihood), col="red", lwd=2)    

#Calcule a proporcao de valores aleatorizados que cai?ram
#acima ou abaixo do valor real => valor de P
mean(like.nulls)
sd(like.nulls)
mod@likelihood
praw <- sum(like.nulls>(mod@likelihood)) / length(like.nulls)
ifelse(praw > 0.5, 1-praw, praw)


##########################################################


####5. COMPARACAO DE UM MESMA METRICA ENTRE DUAS REDES####

#Diferenca entre redes binarias, com base no modelo nulo 2 de Bascompte et al. (2003)
#ALERTA: pode demorar a rodar.

#Limpe os objetos criados anteriormente
rm(list= ls())

#Carregue as duas redes que serao comparadas
dados<-read.table("rede1.txt", head=TRUE)
dados2<-read.table("rede2.txt", head=TRUE)
dados
dados2
dados_bin<-ifelse(dados==0,0,1)
dados2_bin = ifelse(dados2==0,0,1)

dados_neo=as.matrix(dados_bin) 

dados2_neo=as.matrix(dados2_bin)

#Escolha a metrica a ser usada para a comparacao entre as redes.
#Pode trocar por uma metrica especifica. Por exmeplo, metrics=c("nestedness")
metrics=c("ALLBUTDD")

#Calcule a diferenca na metrica escolhida entre as redes originais
orig=abs(networklevel(dados_neo,index=metrics)-networklevel(dados2_neo,index=metrics))
orig

#Calcule as diferencas na metrica escolhida entre as redes aleatorizadas.
#Geralmente fica confiavel a partir de 999 permutacoes.
Nperm = 9
randomized.basc=matrix(nrow=length(orig),ncol=Nperm+1)
row.names(randomized.basc)=names(orig)
randomized.basc[,1]=orig 

#Calcule a diferenca na metrica escolhida para todas as redes aleatorizadas.
i<-1
while(i <=Nperm){ 
	dados_aleat=permatfull(dados_neo,fixedmar="both",mtype="prab",times=1)
	dados_aleat=dados_aleat$perm[[1]]
	dados2_aleat=permatfull(dados2_neo,fixedmar="both",mtype="prab",times=1)
	dados2_aleat=dados2_aleat$perm[[1]]
	linha<-abs(networklevel(dados_aleat, index=metrics)-networklevel(dados2_aleat, index=metrics))
	randomized.basc[,i+1]=linha
	print(i)
	i<-i+1
	} 
   
randomized.basc

#Plote e exporte a comparacao entre o valor observado e a distribuicao da metrica aleatorizada
niveis<-row.names(randomized.basc)
for(k in niveis)
	{
		if(any(is.na(randomized.basc[k,]) == TRUE))
			{
			print("K tem NA")
			} else {
	nome.arq<- paste("Hist_DIFERENCES_basc_null_",k,".png", sep="")
	png(filename= nome.arq, res= 300, height= 15, width=21, unit="cm")
	plot(density(randomized.basc[k,]), main="Comparison between observed and
     	randomized values",)
	abline(v=orig[k], col="red", lwd=2, xlab="")
	dev.off()
	print(k)
	nome.arq<- paste("DIFERENCES_Basc_Null_mean_sd_",k,".txt", sep="")
	write.table(cbind(mean(randomized.basc[k,]),sd(randomized.basc[k,])), file=paste(nome.arq,sep=""), 
      sep=" ",row.names=TRUE,col.names=FALSE)
		}
	}

#Calcule a proporcao das diferencas aleatorizadas que ficou maior do que
#a diferencas entre as redes originais
significance.basc=matrix(nrow=nrow(randomized.basc),ncol=3)
row.names(significance.basc)=row.names(randomized.basc)
colnames(significance.basc)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

signif.sup=function(x) sum(x>=x[1])/length(x)
signif.inf=function(x) sum(x<=x[1])/length(x)
signif.two=function(x) ifelse(min(x)*2>1,1,min(x)*2)

significance.basc[,1]=apply(randomized.basc,1,signif.inf)
significance.basc[,2]=apply(randomized.basc,1,signif.sup)
significance.basc[,3]=apply(significance.basc[,-3],1,signif.two)

significance.basc


#Exporte os resultados
file_name= paste("Bascompte2_2_networks_",i,"_Nperm.txt", sep="")
write.table(significance.basc, file=file_name, 
            sep=" ",row.names=TRUE,col.names=TRUE)


##########
	
	
#Diferenca entre redes ponderadas, com base no modelo nulo de Patefield (1981)
#ALERTA: demora bastante a rodar

#Limpe os objetos criados anteriormente
rm(list= ls())

dados<-read.table("rede1.txt", head=TRUE)
dados
dados2<-read.table("rede2.txt", head=TRUE)
dados2
dados_neo=as.matrix(dados)
dados_neo
dados2_neo=as.matrix(dados2)
dados2_neo

#Escolha a metrica a ser usada para a comparacao entre as redes

metrics=c("H2")
metrics

#Calcule a diferencas na metrica entre as redes originais
orig=abs(networklevel(dados_neo,index=metrics)-networklevel(dados2_neo,index=metrics))
orig

#Calcule as diferencas na metrica escolhida entre as redes aleatorizadas.
#Geralmente fica confiavel a partir de 999 permutacoes.
Nperm = 9
i=1
randomized.patef=matrix(nrow=length(orig),ncol=Nperm+1)
row.names(randomized.patef)=names(orig)
randomized.patef[,1]=orig

while(i <=Nperm){ 
  
  dados_aleat=permatfull(dados_neo,fixedmar="both",mtype="count",times=1)
  dados_aleat=dados_aleat$perm[[1]]
  dados2_aleat=permatfull(dados2_neo,fixedmar="both",mtype="count",times=1)
  dados2_aleat=dados2_aleat$perm[[1]]
  linha<-abs(networklevel(dados_aleat, index=metrics)-networklevel(dados2_aleat, index=metrics))
  randomized.patef[,i+1]=linha
  print(i)
  i=i+1
  
} 

randomized.patef

#Plote e exporte a comparacao entre o valor observado e a distribuicao da metrica aleatorizada
niveis<-row.names(randomized.patef)
for(k in niveis)
	{
		if(any(is.na(randomized.patef[k,]) == TRUE))
			{
			print("k tem NA")
			} else {
	nome.arq<- paste("Hist_DIFERENCES_patef_null_",k,".png", sep="")
	png(filename= nome.arq, res= 300, height= 15, width=21, unit="cm")
	plot(density(randomized.patef[k,]), main="Comparison between observed and
     	randomized values",)
	abline(v=orig[k], col="red", lwd=2, xlab="")
	dev.off()
	print(k)
	nome.arq<- paste("DIFERENCES_patef_Null_mean_sd_",k,".txt", sep="")
	write.table(cbind(mean(randomized.patef[k,]),sd(randomized.patef[k,])), file=paste(nome.arq,sep=""), 
      sep=" ",row.names=TRUE,col.names=FALSE)
		}
	}


#Calcule a proporcao das diferencas aleatorizadas que ficou maior do que
#a diferen??a entre as redes originais
significance.patef=matrix(nrow=nrow(randomized.patef),ncol=3)
row.names(significance.patef)=row.names(randomized.patef)
colnames(significance.patef)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

signif.sup=function(x) sum(x>=x[1])/length(x)
signif.inf=function(x) sum(x<=x[1])/length(x)
signif.two=function(x) ifelse(min(x)*2>1,1,min(x)*2)

significance.patef[,1]=apply(randomized.patef,1,signif.inf)
significance.patef[,2]=apply(randomized.patef,1,signif.sup)
significance.patef[,3]=apply(significance.patef[,-3],1,signif.two)

significance.patef

#Exporte os resultados
file_name= paste("Diferencas_Patefiled",i,"_Nperm.txt", sep="")
write.table(significance.patef, file=file_name, 
            sep=" ",row.names=TRUE,col.names=TRUE)



###################

#Diferenca entre redes ponderadas, com base no modelo nulo baseado no algoritmo swap
#ALERTA: demora bastante a rodar

#Limpe os objetos criados anteriormente
rm(list= ls())

dados<-read.table("rede1.txt", head=TRUE)
dados
dados2<-read.table("rede2.txt", head=TRUE)
dados2
dados_neo=as.matrix(dados)
dados_neo
dados2_neo=as.matrix(dados2)
dados2_neo

#Escolha a metrica a ser usada para a comparacao entre as redes

metrics=c("H2")
metrics

#Calcule a diferencas na metrica entre as redes originais
orig=abs(networklevel(dados_neo,index=metrics)-networklevel(dados2_neo,index=metrics))
orig

#Calcule as diferencas na metrica escolhida entre as redes aleatorizadas.


#Calcule as diferencas na metrica escolhida entre as redes aleatorizadas.
#Geralmente fica confiavel a partir de 999 permutacoes.
Nperm = 1000
i=1
randomized.patef=matrix(nrow=length(orig),ncol=Nperm+1)
row.names(randomized.patef)=names(orig)
randomized.patef[,1]=orig

while(i <=Nperm){ 
  
  dados_aleat=permatswap(dados_neo,fixedmar="both", method="quasiswap", mtype="count",times=1)
  dados_aleat=dados_aleat$perm[[1]]
  dados2_aleat=permatswap(dados2_neo,fixedmar="both", method="quasiswap", mtype="count",times=1)
  dados2_aleat=dados2_aleat$perm[[1]]
  linha<-abs(networklevel(dados_aleat, index=metrics)-networklevel(dados2_aleat, index=metrics))
  randomized.patef[,i+1]=linha
  print(i)
  i=i+1
  
} 

randomized.patef

#Plote e exporte a comparacao entre o valor observado e a distribuicao da metrica aleatorizada
niveis<-row.names(randomized.patef)
for(k in niveis)
{
  if(any(is.na(randomized.patef[k,]) == TRUE))
  {
    print("k tem NA")
  } else {
    nome.arq<- paste("Hist_DIFERENCES_patef_null_",k,".png", sep="")
    png(filename= nome.arq, res= 300, height= 15, width=21, unit="cm")
    plot(density(randomized.patef[k,]), main="Comparison between observed and
         randomized values",)
    abline(v=orig[k], col="red", lwd=2, xlab="")
    dev.off()
    print(k)
    nome.arq<- paste("DIFERENCES_patef_Null_mean_sd_",k,".txt", sep="")
    write.table(cbind(mean(randomized.patef[k,]),sd(randomized.patef[k,])), file=paste(nome.arq,sep=""), 
                sep=" ",row.names=TRUE,col.names=FALSE)
  }
}


#Calcule a proporcao das diferencas aleatorizadas que ficou maior do que
#a diferen??a entre as redes originais
significance.patef=matrix(nrow=nrow(randomized.patef),ncol=3)
row.names(significance.patef)=row.names(randomized.patef)
colnames(significance.patef)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

signif.sup=function(x) sum(x>=x[1])/length(x)
signif.inf=function(x) sum(x<=x[1])/length(x)
signif.two=function(x) ifelse(min(x)*2>1,1,min(x)*2)

significance.patef[,1]=apply(randomized.patef,1,signif.inf)
significance.patef[,2]=apply(randomized.patef,1,signif.sup)
significance.patef[,3]=apply(significance.patef[,-3],1,signif.two)

significance.patef

#Exporte os resultados
file_name= paste("Diferencas_Patefiled",i,"_Nperm.txt", sep="")
write.table(significance.patef, file=file_name, 
            sep=" ",row.names=TRUE,col.names=TRUE)



##############################################################


####6. COMPARACAO DE MODULARIDADE QUANBIMO ENTRE DUAS REDES####

#Redes ponderadas, aleatorizadas com base no modelo nulo de Patefield (1981)
#ALERTA: demora a rodar 

#Limpe os objetos criados anteriormente
rm(list= ls())

#Crie os objetos a serem analisados
dados<-read.table("rede1.txt", head=TRUE)
dados2<-read.table("rede2.txt", head=TRUE)
dados_neo=as.matrix(dados)
dados_neo
dados2_neo=as.matrix(dados2)
dados2_neo

#Calcule a modularidade QuaBiMo com valores de Q padronizados (z)
mod1 <- computeModules(web=dados, steps=1E6)
mod2 <- computeModules(web=dados2, steps=1E6)

#Padronize o valor de Q da rede 1 e escolha o numero de permutacoes
nulls1 <- nullmodel(dados, N=9, method="r2d")
modules.nulls1 <- sapply(nulls1, computeModules)
like.nulls1 <- sapply(modules.nulls1, function(x) x@likelihood)
mod_val1<- (mod1@likelihood - mean(like.nulls1))/sd(like.nulls1)
mod_val1

#Padronize o valor de Q da rede 2 e escolha o numero de permutacoes
nulls2 <- nullmodel(dados2, N=9, method="r2d")
modules.nulls2 <- sapply(nulls2, computeModules)
like.nulls2 <- sapply(modules.nulls2, function(x) x@likelihood)
mod_val2<- (mod2@likelihood - mean(like.nulls2))/sd(like.nulls2)
mod_val2

#O valor de Q padronizado e dado em desvios-padrao.
#Ou seja, o valor que resultar desta an??lise mostra o quanto a rede observada
#difere das aleatorizadas.
#Em geral, valores maiores do que 2 costumam ser significativos.
#Para estimar a significancia, faca uma contagem ou rode um teste Z

 
#Calcule a diferen??a no valor de Q padronizado entre as duas redes 
orig = abs(mod_val1-mod_val2)
orig

names(orig)<- "Diferenca em Q padronizado"

#Fa??a o mesmo para as vers??es aleatorizadas das redes
Nperm = 9 #Costuma ficar confiavel a partir de 999

randomized.patef=matrix(nrow=length(orig),ncol=Nperm+1)
row.names(randomized.patef)=names(orig)
randomized.patef[,1]=orig #Porque os valores originais est?o entre os que podem ser obtidos pelo acaso,

randomized.patef<- as.matrix(randomized.patef)
i<-1

while (i<=Nperm){ 
	#Matriz 1 aleatorizada
	dados_aleat=permatfull(dados_neo,fixedmar="both",mtype="count",times=1)
	dados_aleat=dados_aleat$perm[[1]]
	dados2_aleat=permatfull(dados2_neo,fixedmar="both",mtype="count",times=1)
	dados2_aleat=dados2_aleat$perm[[1]]
	#dados_aleat<- as.matrix(dados_aleat)
	#dados2_aleat<- as.matrix(dados2_aleat)
	print("passou")
	#Computando valores de Q em matrizes aleatorizadas
	mod1_aleat <- try(computeModules(web=dados_aleat, steps=1E6)) #1E6 no minimo quando for pra valer
	print("passou mod 1")
	print(i)
	mod2_aleat <- try(computeModules(web=dados2_aleat, steps=1E6)) #1E6
	
	print("passou mod 2 GO GO GO")
	print(i)
	nulls1_aleat <- nullmodel(dados_aleat, N=9, method="r2d")
 	modules.nulls1_aleat <- sapply(nulls1_aleat, computeModules)
 	print("passou nulls aleat1")
	like.nulls1_aleat <- sapply(modules.nulls1, function(x) x@likelihood)

	mod_val1_aleat<- (mod1@likelihood - mean(like.nulls1_aleat))/sd(like.nulls1_aleat)
	#Rede 2
 	nulls2 <- nullmodel(dados2_aleat, N=9, method="r2d")
 	modules.nulls2_aleat <- sapply(nulls2, computeModules)
	print("passou nulls aleat2") 
	like.nulls2_aleat <- sapply(modules.nulls2_aleat, function(x) x@likelihood)
	mod_val2_aleat<- (mod2@likelihood - mean(like.nulls2_aleat))/sd(like.nulls2_aleat)
	
	print("passou TRY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
		
	linha<-abs(mod_val1_aleat-mod_val2_aleat)

	randomized.patef[,i+1]=linha
	
	print(i)
	print(Sys.time())
	i<-i+1
	nome<- paste("RANDOMIZED_values_MOD_patef_sink","_",i,".txt")
	sink(nome)
	cat(randomized.patef)
	sink()
	} 

nometxt<- paste("RANDOMIZED_values_MOD_Patefield_TXT","_",i,".txt")
write.table(randomized.patef,nometxt, sep="\t", quote=F)
nometxt



#Plote e exporte a comparacao entre o valor observado e a distribuicao aleatorizada de QuanBiMo
niveis<-row.names(randomized.patef)
for(k in niveis)
	{
		if(any(is.na(randomized.patef[k,]) == TRUE))
			{
			print("k tem NA")
			} else {
	nome.arq<- paste("Hist_DIFERENCES_Quanbimo_null_",k,".png", sep="")
	png(filename= nome.arq, res= 300, height= 15, width=21, unit="cm")
	plot(density(randomized.patef[k,]), main="Comparacao entre o valor observado
	     e os valores aleatorizados",)
	abline(v=orig[k], col="red", lwd=2, xlab="")
	dev.off()
	print(k)
	nome.arq<- paste("DIFERENCES_patef_Null_mean_sd_QuanBiMo",k,".txt", sep="")
	write.table(cbind(mean(randomized.patef[k,]),sd(randomized.patef[k,])), file=paste(nome.arq,sep=""), 
      sep=" ",row.names=TRUE,col.names=FALSE)
		}
	}

#Estime a significancia da diferenca
randomized.patef
#Caso queira fechar o script e trabalhar em outro momento, 
#leia o TXT exportado anteriormente, que comeca com
#RANDOMIZED_values_MOD_Patefield... e continue o script
#randomized.patef<-read.csv(file.choose(), sep="\t")

#Calcule a proporcao de valores abaixo, acima e iguais a diferenca observada
significance.patef=matrix(nrow=nrow(randomized.patef),ncol=3)
row.names(significance.patef)=row.names(randomized.patef)
colnames(significance.patef)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

signif.sup=function(x) sum(x>=x[1])/length(x) #unicaudal
signif.inf=function(x) sum(x<=x[1])/length(x) #unicaudal
signif.two=function(x) ifelse(min(x)*2>1,1,min(x)*2)#bicaudal

significance.patef[,1]=apply(randomized.patef,1,signif.inf)
significance.patef[,2]=apply(randomized.patef,1,signif.sup)

significance.patef2<-data.frame(significance.patef)
significance.patef2[,3]=apply(significance.patef2[,-3],1,signif.two)
colnames(significance.patef2)=c("p (rand <= orig)", "p (rand >= orig)", "p (rand=orig)")

significance.patef2

#Exporte os resultados
file_name= paste("Pvalue_Mod_Patefield_",i,"_Nperm.txt", sep="")
write.table(significance.patef2, file=file_name, 
            sep="\t",row.names=TRUE,col.names=TRUE, quote=F)


#########################################################################
#######################BE HAPPY :)#######################################
	
####7. REFERENCIAS & LEITURAS SUGERIDAS##################################

#Barabasi, A.-L. (2016) Network Science, 1st ed. Cambridge University
 #Press, Cambridge.

#Bascompte, J. & Jordano, P. (2014) Mutualistic Networks, 1st ed. Princeton
 #University Press, Princeton.

#Bascompte, J., Jordano, P., Melian, C.J. & Olesen, J.M. (2003) The nested assembly
	#of plant-animal mutualistic networks. Proceedings of the National Academy
	#of Sciences of the United States of America, 100, 9383???9387.

#Bezerra, E.L.S., Machado, I.C. & Mello, M.A.R. (2009) Pollination networks of
  #oil-flowers: a tiny world within the smallest of all worlds. Journal of Animal
  #Ecology, 78, 1096???101.
  #Fonte da rede1 do exemplo (Catimbau).

#Blondel, V.D., Guillaume, J.-L., Lambiotte, R. & Lefebvre, E. (2008) Fast 
	#unfolding of communities in large networks. Journal of Statistical Mechanics:
	#Theory and Experiment, 2008, P10008.

#Dormann, C.F. & Strauss, R. (2014) A method for detecting modules in quantitative
	#bipartite networks (ed P Peres-Neto). Methods in Ecology and Evolution, 5, 90???98.

#Patefield, W.M. 1981. Algorithm AS159. An efficient method of generating
	#r x c tables with given row and column totals. Applied Statistics 30, 91-97.

#Vazquez, D.P. & Simberloff, D. 2003. Changes in interaction biodiversity
  #induced by an introduced ungulate. Ecology Letters, 6, 1077-1083.
  #Fonte da rede2 do exemplo (Safariland, parcial).

