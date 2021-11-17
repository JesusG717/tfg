#jerarquico
h_clustering <- function(X,method){
  C_h <- X %>% select(c(1:2)) %>% dist() %>% hclust(method)
  #dendrograma
  dendrogram <- plot(C_h)
  #conseguir una particion concreta
  partition_h<-cutree(C_h,k)
  X <- mutate(X, hc = factor(partition_h))
  return(X)
}
