#voy a tratar de implementar la distancia del hdbscan para el jerarquico
#De momento no se como hacerlo, no entiendo bien el kNN()
source("1_setup.R")

#Partimos de la matriz de distancias de X
dataset<-'data/artificial/diamond9.arff'
X<-read.arff(dataset)

summary(X)#variables que tienen los datos
#en los de 2-d tiene las dos variables y una tercera variable con la clase de cada elemento

#Elijo un numero de clusters (voy a coger directamente el numero de clases que dan los datos)
k<-nlevels(X$class)
k

X_d <- dist(X)
#la distancia entre dos elementos es el maximo entre sus distance y la core distance
#tengo que calcular la core distance
X_d
C <- kNN(select(X,c(1,2)),4)
source("hclust.R")

method<-'single' #metodos: complete, single, average, centroids
C_h <- C$dist %>% hclust(method)
#dendrograma
dendrogram <- plot(C_h)
#conseguir una particion concreta
partition_h<-cutree(C_h,k)
X <- mutate(X, hc = factor(partition_h))

#solucion del jerarquico
plot_h_s <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hc))
+ labs(colour = "Clusters", title = "Single Link") + theme(axis.title = element_blank())