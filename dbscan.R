#funciones para hacer el clutering por densidad
#primero el dbscan
function(X,epsilon,MinPts){
  C_d <- X %>% select(c(1,2)) %>% dbscan(epsilon,MinPts)
  X <- mutate(X, dbs = factor(C_dbs$cluster))
  return(X)
    
  
}


#funcion para hacer el optics (no me va a dar el diagrama caracteristico del optics)
#Tengo que entenderlo mejor para saber como quiero hacer el input y output y la visualizacion