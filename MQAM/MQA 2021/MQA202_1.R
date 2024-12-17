#Carregando bibliotecas
library(readr)
library(FactoMineR)
library(factoextra)
library(cluster)
#carregando base de dados
MQA <- read_csv("MQA.csv")
View(MQA)

#CLUSTER HIERARQUICO

#normalizando a base
base_clus <- as.data.frame(scale(MQA))
View(base_clus)

#Dendrograma
notasclust <- hclust(d = dist(base_clus, method = 'euclidean'), method = 'ward.D')
plot(notasclust)
##baseado no grafico, escolhemos 4 clusters 

##pegando os grupos previstos, com corte da árvore em 4 grupos
previsao = cutree(notasclust, 4)
##Gráficos
plot(base_clus, col=previsao)
clusplot(base_clus, previsao, color=T, lines = F, labels = 4)


#guardando informação na base
base_clus <- cbind(MQA, previsao)
#salvando a base em arquivo csv
write.csv2(base_clus, file = "resultado_MQA_hierarquico.csv")

###############################################################################

#CLUSTER NAO HIERARQUICO

#normalizando a base
base_clus <- as.data.frame(scale(MQA))
View(base_clus)

###numero otimo de clusters
fviz_nbclust(base_clus, kmeans, method = "silhouette")
###resultado 2 clusters

#gerando clusters com 2 grupos
resp_clus <- kmeans(base_clus, 2)
##gráfico clusters
fviz_cluster(resp_clus, data = base_clus)
clusplot(base_clus, resp_clus$cluster, color=T, lines = F, labels = 4)

#guardando informação na base
base_clus <- cbind(MQA, resp_clus$cluster)
#salvando a base em arquivo csv
write.csv2(base_clus, file = "resultado_MQA_N_hierarquico_2C.csv")

###############################################################################

#CLUSTER NAO HIERARQUICO COM 4 CLUSTERS

#normalizando a base
base_clus <- as.data.frame(scale(MQA))
View(base_clus)

#gerando clusters com 4 grupos
resp_clus <- kmeans(base_clus, 4)
##gráfico clusters
fviz_cluster(resp_clus, data = base_clus)
clusplot(base_clus, resp_clus$cluster, color=T, lines = F, labels = 4)


#guardando informação na base
base_clus <- cbind(MQA, resp_clus$cluster)
view(base_clus)
#salvando a base em arquivo csv
write.csv2(base_clus, file = "resultado_MQA_N_hierarquico_4C.csv")


