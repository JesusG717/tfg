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
source('10_hclust.R')
source('11_km-kmpp.R')
source('12_dbscan.R')
D <- select(audio_features,c('danceability','key'))
KM <- km_clustering(D,3,20)
C <- NbClust(data = D, distance = 'euclidean',method = 'kmeans', index = c('ch'))
C
Resultados <- select(top_tracks,'name')
Resultados <- mutate(Resultados, KM)   

Resultados <- mutate(Resultados, class = factor(KM$kpp))   
Resultados


#QUIERO UNA MANERA DE PONER LA AGRUPACION BONITA




u <- get_playlists()



#Pablo
u <- get_user_audio_features('9v2h1n2bq7qf80yz9cwb3kfmb')


Sys.setenv(SPOTIFY_CLIENT_ID = '71fc7704316842e0beca8a2a40abe091')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b218329af8a34dec872949e34082fee0')

access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario

aut <- get_spotify_authorization_code()
No
