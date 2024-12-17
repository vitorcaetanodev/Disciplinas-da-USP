mydata <- read.table("./MQA2022 – grupo01 – dataset_clusters_ativ5.csv", header=TRUE,
   sep=",")

colnames(mydata)

datasetsemtratar <- mydata

quartiles <- quantile(mydata[,c(4)], probs=c(.25, .75), na.rm=TRUE)
IQR <- IQR(mydata[,c(4)], na.rm=TRUE)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
mydata <- subset(mydata, mydata[,c(4)] > Lower & mydata[,c(4)] < Upper)

dim(mydata)
dim(datasetsemtratar)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#filtrar para o ano mais recente

mydata <- mydata[mydata$Year == '2015',]
dim(mydata)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#subset dos dados

valores_relevantes <- mydata[,c(1,2,3,4,11,22)]
valores_relevantes$Status <- as.factor(valores_relevantes$Status)

colnames(valores_relevantes)

plot(valores_relevantes,pch=16, col=valores_relevantes$Status)
str(unique(valores_relevantes$Status))
legend(x = "top",inset = 0,legend = unique(valores_relevantes$Status), lwd=5, cex=.5, horiz = TRUE)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#normalização, visualização depois dela
valores_relevantes[,c(4,5,6)] <- scale(valores_relevantes[,c(4,5,6)])

plot(valores_relevantes,pch=16, col=valores_relevantes$Status)
legend(x = "top",inset = 0,legend = unique(valores_relevantes$Status), lwd=5, cex=.5, horiz = TRUE)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#modelo hierarquico


numeros <- valores_relevantes[,c(4,5,6)]

rownames(numeros) <- valores_relevantes[,c(1)]

#método 'complete'
modelo <- hclust(dist(numeros))

plot(modelo, cex=0.3)
plot(numeros, pch=16, col=cutree(modelo, 2))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# A semelhança entre os modelos
# a ordem das cores padrão é 1-black, 2-red
cm <- as.matrix(table(Actual = valores_relevantes[,c(3)], Predicted = cutree(modelo, 2)))

#precisão invertida pois as cores estão invertidas
accuracy <- 1 - (sum(diag(cm)) / sum(cm))

print(accuracy)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#agora fazendo o mesmo, com kmeans

modelo2 <- kmeans(na.omit(dist(numeros)), 2, algorithm="Hartigan-Wong")

plot(numeros, col = modelo2$cluster)
points(modelo2$center,col=1:2,pch=8,cex=1)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# semelhança entre os modelos v2
cm <- as.matrix(table(Actual = valores_relevantes[,c(3)], Predicted = modelo2$cluster))

#sem inversão, porém pode ser necessário dependendo de quando você roda o algoritmo
accuracy <- sum(diag(cm)) / sum(cm)
print(accuracy)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# semelhança entre os modelos v3

cm <- as.matrix(table(Actual =  cutree(modelo, 2), Predicted = modelo2$cluster))

#pode ser necessário inverter, pode não, devido a aleatoriedade do kmeans de escolher as cores.
accuracy <- sum(diag(cm)) / sum(cm)
print(accuracy)
