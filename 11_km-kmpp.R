#Voy a definir una función que haga kmeans y kmeans++
#X un data frame con variables a estudiar en las dos primeras columnas y k numero de clusters
km_clustering <- function(X,k,num_km){
  C_km<- X %>% select(c(1,2)) %>% kmeans(k,nstart = num_km)
  #lo mismo pero con kmeans++
  
  C_kpp<- X %>% select(c(1,2)) %>% kmeanspp(k)
  #Voy a pintar el clustering que me da el algoritmo
  #primero creo una variable que asigne la clase de cada cluster en X
  X <- mutate(X, km = factor(C_km$cluster), kpp = factor(C_kpp$cluster))

  
  return(X)
}

km_clustering_custom <- function(X,c){
  C_km<- X %>% select(c(1,2)) %>% kmeans(c)
  
  #primero creo una variable que asigne la clase de cada cluster en X
  X <- mutate(X, km = factor(C_km$cluster))
  
  
  return(X)
}

