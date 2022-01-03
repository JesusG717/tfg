#Pruebas con datos de spotify
source('01_setup.R')

#paquete para la api de spoti
library('spotifyr')

#conseguir token de la api
Sys.setenv(SPOTIFY_CLIENT_ID = '44855249207d40619183b073132a6407')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2296850888224ff0bb4e35ad23962578')

access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario

aut <- get_spotify_authorization_code()
                                      

#prueba 1: te saca en orden las escalas mas usadas del artista
yo <- get_artist_audio_features('Queen')

sub <- select(yo, c('energy','danceability','key_mode','loudness'))
summary(sub)
sub %>% count(key_mode, sort = TRUE) %>% head(5) %>% kable()


#prueba 2: sacar las canciones más escuchadas de un usuario
                                      
top_tracks <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, time_range = "long_term")
names(top_tracks)
top_tracks <- top_tracks %>% select(,c('name','id','popularity'))
top_tracks
audio_features <- get_track_audio_features(top_tracks$id)
names(audio_features)
audio_features <- audio_features %>% select(,c('danceability','acousticness','instrumentalness','loudness','tempo','speechiness'))
audio_features
top_tracks <- top_tracks %>% mutate(,audio_features)
names(top_tracks)
top_tracks
#OUTPUT top_tracks es un data.frame con nombre id popularidad y 6 audio features del top50 tracks del usuario




#prueba de hacer algun clustering
source('11_km-kmpp.R')
D <- top_tracks %>% select(,c(4:9))
C <- NbClust(data = D, distance = 'euclidean',method = 'kmeans', index = c('silhouette'))
C
Resultados <- select(top_tracks,'name')
Resultados <- mutate(Resultados, class = factor(C$Best.partition))   
Tabla <- mutate(Resultados, n= filter(Resultados, class == factor(i) )
#QUIERO UNA MANERA DE PONER LA AGRUPACION BONITA
