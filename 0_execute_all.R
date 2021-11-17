#cargar paquetes
source("1_setup.R")
#Quiero leer el archivo de datos primero
dataset<-'data/artificial/3-spiral.arff'
X<-read.arff(dataset)

summary(X)#variables que tienen los datos
#en los de 2-d tiene las dos variables y una tercera variable con la clase de cada elemento

#Elijo un numero de clusters (voy a coger directamente el numero de clases que dan los datos)
k<-nlevels(X$class)
k

#Hago partición kmeans y kmea++
source("km-kmpp.R")
X <- km_clustering(X,k)
#solucion del kmeans
plot_km <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = km))
#solucion kmeanspp
plot_kmpp <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = kpp))


#Hago partición para el jerarquico
source("hclust.R")

method<-'single' #metodos: complete, single, average, centroids
X <- h_clustering(X,method)
#solucion del jerarquico
plot_h <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hc))


#Hago el fuzy clustering 
#este no lo hice en un script separado porque es diferente a los otros y no se muy bien como hacerlo

#fuzzy clustering
type<-'gk' #algoritmos: FKM, gk
C_f<- X %>% select(c(1,2)) %>% Fclust(k,type)
#mirar todas las opciones del paquete fclust para ver como pintar cosas diferente
plot.fclust(C_f,umin=0.3,ucex = TRUE, pca = FALSE)


#voy a probar el jerarquico con densidad (un desastre todo)
#kNNdist es la mutual reachability distance
#el numero de puntos que se suele usar es la cantidad de variables * 2
dist_hdbs<- X %>% select(c(1,2)) %>% dist() 
C_hdbs<-hdbscan(select(X,c(1,2)),4)
X <- mutate(X, hdbs = factor(C_hdbs$cluster))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hdbs))

#prueba de optics (funciona pero no se como representarlo como los otros)
opt <- X %>% select(c(1,2)) %>% optics(,minPts = 4)
C_opt<-extractDBSCAN(opt,5)
X <- mutate(X, opt = factor(C_opt$cluster))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = opt))
