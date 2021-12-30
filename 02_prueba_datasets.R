library('tidyverse')
library('RWeka')
library('fclust')
library('NbClust')
#paquete para el kmeanspp
library('LICORS')
#cluster me a?ade daisy() para usar distancias para clustering categorico (ver jerarquico)
library('cluster')
#dbscan y hdbscan
library('dbscan')


#elegimos el data aqui (mirar los nombres en la carpeta)
dataset<-'data/artificial/3-spiral.arff'
data<-read.arff(dataset)
summary(data)
#clasificacion real de los datos
ggplot(data = data)+ geom_point(mapping = aes(x = data[,1], y = data[,2], color=class))




#Me insteresan las dos primeras columnas
X<-select(data, c(1,2))
#Visualizaci?n inicial de los datos
ggplot(data = X)+ geom_point(mapping = aes(x = X[,1], y = X[,2]))
#Elijo un numero de clusters (voy a coger directamente el numero de clases que dan los datos)
k<-nlevels(data$class)
k
#Ahora voy a hacer un kmeans clustering
#puedo definir los centorides iniciales o darle un numero de clusters y los coge al azar
C_km<- X %>% select(c(1,2)) %>% kmeans(k)
#lo mismo pero con kmeans++
C_kpp<- X %>% select(c(1,2)) %>% kmeanspp(k)
#Voy a pintar el clustering que me da el algoritmo
#primero creo una variable que asigne la clase de cada cluster en X
X <- mutate(X, km = factor(C_km$cluster), kpp = factor(C_kpp$cluster))
#solucion del kmeans
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = km))
#solucion kmeanspp
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = kpp))



#fuzzy clustering
type<-'gk' #algoritmos: FKM, gk
C_f<- X %>% select(c(1,2)) %>% Fclust(k,type)
#mirar todas las opciones del paquete fclust para ver como pintar cosas diferente
plot.fclust(C_f,umin=0.3,ucex = FALSE, pca = FALSE)





#jerarquico
method<-'complete' #metodos: complete, single, average, centroids
C_h <- X %>% select(c(1:2)) %>% dist() %>% hclust(method)
#dendrograma
dendrogram <- plot(C_h)
#conseguir una particion concreta
partition_h<-cutree(C_h,k)
X <- mutate(X, hc = factor(partition_h))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hc))
#voy a probar el jerarquico con densidad
#kNNdist es la mutual reachability distance
#el numero de puntos que se suele usar es la cantidad de variables * 2
dist_hdbs<- X %>% select(c(1,2)) %>% dist() 
C_hdbs<-hdbscan(select(X,c(1,2)),4)
X <- mutate(X, hdbs = factor(C_hdbs$cluster))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hdbs))

#prueba de optics
opt <- X %>% select(c(1,2)) %>% optics(,minPts = 4)
C_opt<-extractDBSCAN(opt,5)
X <- mutate(X, opt = factor(C_opt$cluster))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = opt))

#banana me da error
#3D: golfball,atom, insect, pmf, simplex, xor
#birch no se por donde cogerlo

