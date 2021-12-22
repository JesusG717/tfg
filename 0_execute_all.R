#Aqui escribo/pruebo los programas antes de hacer un script específico


#cargar paquetes
source("1_setup.R")
#Quiero leer el archivo de datos primero
dataset<-'data/artificial/cure-t2-4k.arff'
X<-read.arff(dataset)

summary(X)#variables que tienen los datos
#en los de 2-d tiene las dos variables y una tercera variable con la clase de cada elemento

#Elijo un numero de clusters (voy a coger directamente el numero de clases que dan los datos)
k<-nlevels(X$class)
k

#Hago partición kmeans y kmea++
source("km-kmpp.R")
X <- km_clustering(X,k,1)
#solucion del kmeans
plot_km <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = km))
plot_km
#solucion kmeanspp
plot_kmpp <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = kpp))
plot_kmpp

#Hago partición para el jerarquico
source("hclust.R")

method<-'single' #metodos: complete, single, average, centroids
X <- h_clustering(X,method)
#solucion del jerarquico
plot_h <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hc))
plot_h

#Hago el fuzy clustering 
#este no lo hice en un script separado porque es diferente a los otros y no se muy bien como hacerlo

#fuzzy clustering
type<-'gk' #algoritmos: FKM, gk
C_f<- X %>% select(c(1,2)) %>% Fclust(k,type)
#mirar todas las opciones del paquete fclust para ver como pintar cosas diferente
plot.fclust(C_f,umin=0.3,ucex = TRUE, pca = FALSE)

#dbscan (ahora funciona pero no se muy bien que valores de MinPts y epsiloin poner)
#estos numeros son para complex9 (12,4)
epsilon <- 0.08 #bastante arbitrario de momento
MinPts <- 15 #numero variables*2
C_dbs <- X %>% select(c(1,2)) %>% dist() %>% dbscan(epsilon,MinPts)
X <- mutate(X, dbs = factor(C_dbs$cluster))
plot_dbs <- ggplot(data = X) + geom_point(mapping = aes(x = X[,1], y = X[,2], colour = dbs))
plot_dbs

X_Filter <- filter(X,dbs != 0, .preserve = FALSE)
summary(X_Filter)
plot_dbs_filtro <- ggplot(data = X_Filter) + geom_point(mapping = aes(x = X_Filter[,1], y = X_Filter[,2], colour = dbs))
plot_dbs_filtro
X_Filter <- km_clustering(X_Filter,k-1,1)
plot_km_filtro <- ggplot(data = X_Filter)+geom_point(mapping = aes(x = X_Filter[,1], y = X_Filter[,2], color = km))
plot_km_filtro
#prueba de optics (aun tengo que ver como representarlo diferente al resto)
opt <- X %>% select(c(1,2)) %>% optics(,minPts = 4)
C_opt<-extractDBSCAN(opt,0.1)
X <- mutate(X, opt = factor(C_opt$cluster))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = class))

#el dscan parece mas sensible al epsilon que el optics, tengo que mirarlo con calma

#voy a probar el jerarquico con densidad (un desastre todo)
#kNNdist es la mutual reachability distance
#el numero de puntos que se suele usar es la cantidad de variables * 2
dist_hdbs<- X %>% select(c(1,2)) %>% dist() 
C_hdbs<-hdbscan(select(X,c(1,2)),4)
X <- mutate(X, hdbs = factor(C_hdbs$cluster))
ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = hdbs))







#prueba con el data set que hice
source('uwu_data.R')
source("km-kmpp.R")
k<- 3
UwU <- km_clustering(UwU,k)
#solucion del kmeans
plot_km <- ggplot(data = UwU)+geom_point(mapping = aes(x = UwU[,1], y = UwU[,2], color = km))
plot_km
#solucion kmeanspp
plot_kmpp <- ggplot(data = UwU)+geom_point(mapping = aes(x = UwU[,1], y = UwU[,2], color = kpp))
plot_kmpp