#Data preparation and classification
#Loading and setting the data obtained in Data_collection
source("01_setup.R")
load("tracks_genre_features.RData")
#DATA FILTERING/WRANGLING
#' creo el archivo track_features para meter ahi las track id, generos y las variables para el clustering

track_features <- tracks_genre_features
#' me quito los tracks repetidos con mismo genero (esto pasa cuando un track tiene dos artistas
#' y los dos artistas tienen los mismos generos asciados en spotify)
track_features <- track_features %>% distinct(track.id,genres,.keep_all = TRUE)

#miro por encima la distribucion de los generos
track_features %>% group_by(genres) %>% distinct() %>% count() %>% arrange(n)
#filtro los generos con menos de 30 canciones
a <- track_features %>% group_by(genres) %>% distinct() %>% count() %>% arrange(n) %>% filter(n<30)

#funcion auxiliar
'%!in%' <- function(x,y)!('%in%'(x,y))

track_features <- track_features %>% filter(genres %!in% a$genres)

#voy uniendo generos para tener categorias con suficientes datos
filtro <- c("progressive house","tropical house","electro house")
a <- track_features$genres %>% replace(which(track_features$genres %in% filtro),"house") 
filtro <- c("dutch edm")
a <- a %>% replace(which(track_features$genres %in% filtro),"edm") 
filtro <- c("nu metal", "rap metal","alternative metal","birmingham metal","glam metal","old school thrash",
            "power metal","thrash metal","funk metal")
a <- a %>% replace(which(track_features$genres %in% filtro),"metal") 
filtro <- c("trap latino","trap triste")
a <- a %>% replace(which(track_features$genres %in% filtro),"trap") 
filtro <-c("latin", "latin hip hop")
a <- a %>% replace(which(track_features$genres %in% filtro),"reggaeton") 

filtro <-c("hard rock", "alternative rock","funk rock","album rock")
a <- a %>% replace(which(track_features$genres %in% filtro),"rock") 

filtro <-c("urbano espanol", "drill espanol")
a <- a %>% replace(which(track_features$genres %in% filtro),"trap") 

filtro <-c("pop dance", "dance pop")
a <- a %>% replace(which(track_features$genres %in% filtro),"pop") 

filtro <-c("urbano espanol", "drill espanol")
a <- a %>% replace(which(track_features$genres %in% filtro),"trap") 

track_features <- track_features %>% mutate(genres = a) 
track_features %>% group_by(genres) %>% distinct() %>% count() %>% arrange(desc(n))




track_features <- track_features %>% distinct(track.id,genres,.keep_all = TRUE)

#compruebo como me ha quedado la dsitribución de generos
track_features %>% group_by(genres) %>% distinct() %>% count() %>% arrange(desc(n))



#transformo als variables tempo y loudness a variables entre 0 y 1
t_max <- max(track_features$tempo)
t_min <- min(track_features$tempo)
track_features <- track_features %>% mutate( tempo = (track_features$tempo-t_min)/(t_max-t_min))

l_max <- max(track_features$loudness)
l_min <- min(track_features$loudness)
l <- (track_features$loudness-l_min)/(l_max-l_min)
track_features <- track_features %>% mutate(loudness = (track_features$loudness-l_min)/(l_max-l_min))


#me quito las variables key y mode porque no se como expresarlas de forma numérica
track_features <- track_features %>% select(-c("key","mode"))

#guardo los datos
save(track_features,file = "track_features.RData")









# DATOS VIEJOS
# 
# 
# #trap latino, trap triste, trap argentino are repeated, so we remove them
# track_features <- track_features %>% filter(!grepl("trap latino|trap triste|trap argentino",genres))
# #remove all varibles with less than 15 elements
# 
# 
# 
# low <- c("trance mexicano|west australian hip hop|electro|latin|latin hip hop|reggaeton|deep groove house|brazilian edm|brostep|complextro|mexican edm|pop rap|rawstyle|canadian metal|industrial metal|pop|rap rock|frenchcore|hardstyle|filter house|full on|post-teen pop|trance|alt z|alternative r&b|candy pop|dark pop|electropop|metropopolis|spanish indie pop|swedish electropop|swedish synthpop|viral pop|australian dance|melbourne bounce|melbourne bounce international")
# track_features <- track_features %>% filter(!grepl(low,genres))
# 
# 
# 
# #standarize all varaibles
# track_features <- track_features %>% mutate(, tempo = (tempo-min(tempo))/(max(tempo)-min(tempo)))
# track_features <- track_features %>% mutate(, loudness = (loudness-min(loudness))/(max(loudness)-min(loudness)))
# 
# #removing mode and key variables for now
# #mode is 0 if in minor, 1 if in major
# #key is an integer 1 to 11 according to the pitch
# 
# features <- track_features %>% select(-c("key","mode"))
# 
# #save a general file with all decently populated genres together
# save(track_features,file = "track_features.RData")
# 
# #SUBGROUPS OF THE DATA TO CLASSIFY
# #create subgroups with a few amount of genres for more accurate interpretations
# 
# track_features %>% group_by(genres) %>% count() %>% arrange(desc(n))
# #mix genres that can be considered the same
# #creating genre: lo-fi
# lofi <- track_features$genres %>% replace(which(track_features$genres =="lo-fi jazzhop"
#                                                 |track_features$genres =="christian lo-fi"
#                                                 |track_features$genres =="chillhop"
#                                                 |track_features$genres =="lo-fi beats"),"lo-fi")
# filtro1 <- track_features %>% mutate(genres = lofi)
# tracks_and_genres %>% group_by(track.id,genres) %>% count() %>% arrange(desc(n))
# #MIRAR PORQUE NO VA
# prueba <- filtro1 %>% distinct(track.id,genres,.keep_all = TRUE)
# prueba %>% group_by(genres) %>% count() %>% arrange(desc(n))
# filtro1 %>% group_by(genres) %>% count() %>% arrange(desc(n))
# 
# df <- tibble(x= sample(10,100,rep=TRUE),y= sample(10,100,rep=TRUE))
# df %>% distinct(x,y)
# 
# 
# 
# #PCA
# pca_f <- features %>% select(-c("track.id","genres")) %>% prcomp()
# summary(pca_f)
# as.tibble(pca_aux$x[,1:6])