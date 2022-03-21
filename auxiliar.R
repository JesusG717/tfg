#Juntar al archivo que tengo en casa

load("tracks_genre_features.RData")
tracks_features <- tracks_genre_features %>% select(c(1,3,5:15))
#pca sin las variables no escaladas
t_min <- min(tracks_features$tempo)
t_max <- max(tracks_features$tempo)
tracks_features <- tracks_features %>% mutate(tempo = (tempo-t_min)/(t_max-t_min))
l_min <- min(tracks_features$loudness)
l_max <- max(tracks_features$loudness)
tracks_features <- tracks_features %>% mutate(loudness = (loudness-l_min)/(l_max-l_min))

#de momento quito mode y key
#key da el pitch de 0 a 11 siendo cada entero una escala
#mode da 0 si esta en menor y 1 si esta en mayor

aux <- tracks_features %>% select(-c("mode","key"))

pca_aux <- aux %>% select(-c("track.id","genres")) %>% prcomp()
summary(pca_aux)
as.tibble(pca_aux$x[,1:6])
