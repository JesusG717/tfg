#Aqui voy a ir probando las distintas metricas de validacion

#cargar paquetes
source("1_setup.R")
#Quiero leer el archivo de datos primero
dataset<-'data/artificial/2d-4c-no4.arff'
X<-read.arff(dataset)

#Indice CH

CH_index <- X %>% select(,c(1,2)) %>% NbClust(, method = "kmeans", index = "ch")
CH_index$Best.nc
CH_index$Best.partition
X <- mutate(X, C_ch = factor(CH_index$Best.partition))
plot_ch <- ggplot(data = X) + geom_point( mapping = aes(x = X[,1], y = X[,2], color = C_ch)) + 
  labs(title = "Datos agrupados con Kmeans", colour = "Clusters") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_ch

#Para pintar una grafica de distintos valores de k solo se me ocurre hacer CH fijando el min-nc y el max.nc al mismo
CH <- c()
k <- c(2:8)
for (i in 2:8) {
  CH_value <- X %>% select(,c(1,2)) %>% NbClust(,min.nc = i, max.nc = i, method = "kmeans", index = "ch")
  CH <- cbind(CH,CH_value$Best.nc)
}
CHplotting <- data.frame(k,CH[2,])
CH_graph <- ggplot(data = CHplotting) + geom_point(mapping = aes( x = CHplotting[,1], y = CHplotting[,2]), size = 5, color = "red") + 
  geom_line( mapping = aes( x = CHplotting[,1], y = CHplotting[,2]), color = "blue", size = 1) +
  labs(title = "CH en función del número de clusters") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
grid.arrange(CH_graph,plot_ch, ncol=2, top = "Valores del índice CH asociados a una cierta muestra de datos")



#Silhouette

Silh <- X %>% select(,c(1,2)) %>% NbClust(,method = "kmeans", index = "silhouette")
Silh$Best.nc
#Esto es para asignar a cada dato su valor de silueta
Silh2 <- silhouette(Silh$Best.partition, dist(select(X,c(1,2))))
s <- Silh2[,3]
X <- mutate(X, s = s)
S_index <- nlevels(X$C_sh)
S <- c()
summary(X)
for (i in 1:S_index){
  S <- c(S,sum(select(filter(X, C_sh == i),c(4))))
  }

X <- mutate(X, C_sh = factor(Silh$Best.partition))
plot_sh <- ggplot(data = X) + geom_point( mapping = aes(x = X[,1], y = X[,2], color = C_sh)) + 
  labs(title = "Datos agrupados con Kmeans", colour = "Clusters") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_sh
S

#plot de valores de SH
SH <- c()
k <- c(4:14)
for (i in 4:14) {
  SH_value <- X %>% select(,c(1,2)) %>% NbClust(,min.nc = i, max.nc = i, method = "kmeans", index = "silhouette")
  SH <- cbind(SH,SH_value$Best.nc)
}
SHplotting <- data.frame(k,SH[2,])
SH_graph <- ggplot(data = SHplotting) + geom_point(mapping = aes( x = SHplotting[,1], y = SHplotting[,2]), size = 5, color = "red") + 
  geom_line( mapping = aes( x = SHplotting[,1], y = SHplotting[,2]), color = "blue", size = 1) +
  labs(title = "Silhouette en función del número de clusters") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
grid.arrange(SH_graph,plot_sh, ncol=2, top = "Valores del índice Silh asociados a una cierta muestra de datos")






