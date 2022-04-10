#CLASIFICATION
source("01_setup.R")
load("track_features.RData")

glimpse(track_features)


#primero hago un clustering a todos los datos
#cojo los datos y les hago un kmeans, un jerarquico, un dbscan y 
#tambien hago un kmeans a los datos fitlrados por dbscan
#kemans
clust1 <- track_features %>% select(-c("track.id","genres")) %>% NbClust(method = "kmeans")
#jerarquico
clust2 <- track_features %>% select(-c("track.id","genres")) %>% NbClust(method = "complete")
#Problema: como determino epsilon y MinPts para dbscan?
# rule of thumb: MinPts = 2*dimension of data. This data has a lot of duplicates so we will do 3*dim
MinP <- 3*ncol(track_features %>% select(-c("track.id","genres")))
#' para el epsilon quiero estudiar la distancia a primeros vecinos de los datos
#' y que el epsilon sea el valor a partir del cual la distancia a primeros vecinos
#' crece mucho. Seria hacer una grafica con los datos ordenados en el eje x y su dist
#' a primeros vecinos en el y y ver donde hay un cambio de pendiente brusco
#' de momento he hecho algo que medio funciona pero probablemente el valor que tomo
#' es totalmente arbitrario
neigh <- track_features %>% select(-c("track.id","genres")) %>% kNN(2)
d <- neigh$dist[,2]
ep <- 0.95*max(d)

clust3 <- track_features %>% select(-c("track.id","genres")) %>% dbscan(,eps = ep, minPts = MinP)
#me quedo con los datos que el dbscan no considera ruido t hago un kmeans
aux <- track_features %>% mutate(dbs=factor(clust3$cluster), N = c(1:nrow(track_features))) %>% filter(dbs != 0)
clust4 <- aux %>% select(-c("dbs","track.id","genres","N")) %>% NbClust(method = "kmeans",min.nc = 2,max.nc = 4,index = "ch")
aux <- aux %>% mutate(km_filter = factor(clust4$Best.partition))


#creo un dataframe para tener los resultados guardados
analysis_alldata <- track_features %>% select(c("track.id","genres"))
analysis_alldata <- analysis_alldata %>% mutate(N = c(1:nrow(track_features)))
#' tengo que hacer bastantes malabares para meter los datos del kmeans sin el ruido del dbscan
#' meto un variable N auxiliar en ambos data frames para poder hacer un left join y ademas
#' tengo que añadir un dato artificial para poder cambiar los NAs por 0s y luego quitarlo
#' 
#' Variables extra
aux2 <- aux %>% select("N","km_filter")
analysis_alldata <- left_join(analysis_alldata,aux2, by = c("N"))
analysis_alldata <- analysis_alldata %>% select(-c("N"))
analysis_alldata <- analysis_alldata %>% mutate(dbs = factor(clust3$cluster),km = factor(clust1$Best.partition), hc = factor(clust2$Best.partition))
#dato extra para cambiar NAs a 0s
a <- rep(0,ncol(analysis_alldata))
analysis_alldata <- add_row(analysis_alldata,km_filter = "0")
analysis_alldata <- analysis_alldata %>% mutate(km_filter = as.factor(analysis_alldata$km_filter))
analysis_alldata$km_filter %>% replace_na(0)
analysis_alldata <- analysis_alldata[-nrow(analysis_alldata),]
summary(analysis_alldata)
# track.id            genres            km_filter  dbs     km      hc     
# Length:743         Length:743         0   :  0   0: 12   1: 43   1:684  
# Class :character   Class :character   1   : 50   1:704   2:429   2: 28  
# Mode  :character   Mode  :character   2   :681   2: 27   3:271   3: 31  
#                                       NA's: 12                          


#guardo el archivo
save(analysis_alldata, file = "analysis_alldata.RData")


#' me di cuenta que hay mucho mas metal y rock que el resto
#' voy a hacer lo mismo que antes pero tomando una sample aleatoria de 50 elementos de rock y metal
#' creo las muestras de rock y metal
r <- track_features %>% filter(genres %in% c("rock")) %>% slice_sample(n = 50)
m <- track_features %>% filter(genres %in% c("metal")) %>% slice_sample(n = 50)
#creo el dataframe con los generos mas balanceados
sub_data <- track_features %>% filter(genres != "rock") %>% filter(genres != "metal")
sub_data <- sub_data %>% rbind(r,m)
#creo un data frame solo con las variables para el clustering
s_features <- sub_data %>% select(-c("track.id","genres"))
#hago los mismos clusterings que antes
km_ch <- s_features %>% NbClust(method = "kmeans")
# km_sh <- s_features %>% NbClust(method = "kmeans",index = "silhouette")

h_ch <- s_features %>% NbClust(method = "complete")
# h_sh <- s_features %>% NbClust(method = "kmeans",index = "silhouette")



# rule of thumb: MinPts = 2*dimension of data. This data has a lot of duplicates so we will do 3*dim
MinP <- 3*ncol(s_features)
neigh <- s_features %>% kNN(2)
d <- neigh$dist[,2]
ep <- 0.95*max(d)

db <- s_features %>% dbscan(,eps = ep, minPts = MinP)
#do a kmeans after filtering noise points
aux_s <- sub_data %>% mutate(dbs=factor(db$cluster), N = c(1:nrow(sub_data))) %>% filter(dbs != 0)
km_db <- aux_s %>% select(-c("dbs","track.id","genres","N")) %>% NbClust(method = "kmeans")
aux_s <- aux_s %>% mutate(km_filter = factor(km_db$Best.partition))


#creo un dataframe analogo pero para estos datos en el que guardar los resultados
analysis_sub1 <- sub_data %>% select(c("track.id","genres"))
analysis_sub1 <- analysis_sub1 %>% mutate(N = c(1:nrow(sub_data)))
#añado la aprte del kmeans despues de dbscan como antes
aux2_s <- aux_s %>% select("N","km_filter")
analysis_sub1 <- left_join(analysis_sub1,aux2_s, by = c("N"))
analysis_sub1 <- analysis_sub1 %>% select(-c("N"))
analysis_sub1 <- analysis_sub1 %>% mutate(dbs = factor(db$cluster),km = factor(km_ch$Best.partition), 
                                          hc = factor(h_ch$Best.partition))
#Nas a 0s
a <- rep(0,ncol(analysis_sub1))
analysis_sub1 <- add_row(analysis_sub1,km_filter = "0")
analysis_sub1 <- analysis_sub1 %>% mutate(km_filter = as.factor(analysis_sub1$km_filter))
analysis_sub1$km_filter %>% replace_na(0)
analysis_sub1 <- analysis_sub1[-nrow(analysis_sub1),]
summary(analysis_sub1)

#guardo el archivo
save(analysis_sub1, file = "analysis_sub1.RData")


