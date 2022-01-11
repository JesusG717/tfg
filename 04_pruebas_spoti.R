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
#ESTO NO VA AHORA MISMO, NO SE POR QUE NO ME DEJA PILLAR MAS CANCIONES CON EL OFFSET
top_tracks1 <- get_my_top_artists_or_tracks(type = "tracks", limit = 49, time_range = "long_term")
top_tracks2 <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, offset = 49, time_range = "long_term")
top_tracks3 <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, offset = 49, time_range = "long_term")
top_tracks4 <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, offset = 49, time_range = "long_term")
top_tracks5 <- get_my_top_artists_or_tracks(type = "tracks", limit = 49, offset = 50, time_range = "long_term")
top_tracks6 <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, offset = 250, time_range = "long_term")



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
D <- select(audio_features,c('danceability','key','instrumentalness'))
KM <- km_clustering(D,10,20)
C <- NbClust(data = D, distance = 'euclidean',method = 'kmeans', index = c('ch'))
C
Resultados <- select(top_tracks,'name')
Resultados <- mutate(Resultados, KM)   

Resultados <- mutate(Resultados, class = factor(KM$kpp))   
Resultados


#QUIERO UNA MANERA DE PONER LA AGRUPACION BONITA



#pruebas a coger cosas de una playlist
u <- get_my_playlists(limit=20,offset=10,)
names(u)
select(u,c('id','name'))
#id Pablo y yo 37i9dQZF1EJC0RPFMlH6eu
#id Tene y yo  37i9dQZF1EJC0RPFMlH6eu
#id Mapi y yo 37i9dQZF1EJAqLHGHdMMaa
#id EDM intenso 1ybnNmpES2UCb8ewXx53AP
#id EDM suave 5acI3aZUMKMp2uct2BDlas
#id desquiciados 7yl2L6SF5B1tjbp8vqb5wh
#id top 100 2021 37i9dQZF1EUMDoJuT8yJsl
#Con las playlist fusion da error



#voy a probar a juntar varias playlist de estado de animo
#sad  https://open.spotify.com/playlist/37i9dQZF1DWZqdNJSufucb?si=b9f5f21e80da4a2d
#happy  https://open.spotify.com/playlist/37i9dQZF1DX9Dh2wgiAwVX?si=9e3b22a4c5c344d9
#pump up  https://open.spotify.com/playlist/37i9dQZF1DX35X4JNyBWtb?si=84c8a491cf6c42f2
#jazz https://open.spotify.com/playlist/37i9dQZF1DX0SM0LYsmbMT?si=81057d7ef74a4c07
#gym  https://open.spotify.com/playlist/37i9dQZF1DX76Wlfdnj7AP?si=f5b5180739284a73

#EJEMPLO 1: mezclo playlist happy, sad y de gym intensa y les hago clustering

plist1 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1DWZqdNJSufucb')
plist2 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1DX9Dh2wgiAwVX')
plist3 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1DX76Wlfdnj7AP')

plist <- rbind(plist1,plist2,plist3)


plist <- select(plist, c(2,'track.name',6:13,15,16))
names(plist)
#aqui he hecho clustering de las variables que me parecieron mas distintivas: ludness, energy, tempo y key
C <- NbClust(data =  select(plist,c(3,4,11,12)), distance = 'euclidean', method = 'kmeans', index = c('silhouette'))
C
results <- select(plist,c('track.name','playlist_name'))
results <- mutate(results, class = factor(C$Best.partition))
results <- results %>% arrange(,class)
#creo subgrupos con cada cluster
c1 <- results %>% filter(,class =='1') %>% print(,n = Inf)
c2 <- results %>% filter(,class =='2') %>% print(,n = Inf)
c3 <- results %>% filter(,class =='3') %>% print(,n = Inf)
c4 <- results %>% filter(,class =='4') %>% print(,n = Inf)

#veo la proporcion de cada playlist en cada cluster
c1 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c2 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c3 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c4 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
#CONCLUSIONES: En c1 estan la mayoria de las del gym intensas, pero las de happy y sad no las ha diferenciado

#AMPLIACION: ahora que se que debo usar 3 clusters, voy a usar mis proprios algoritmos de clustering.

source('10_hclust.R')
source('11_km-kmpp.R')
source('12_dbscan.R')
#empezamos con jerarquico
k <- 3 #ya se que hay 3 grupos por NbClust
c <- km_clustering(select(plist,c(4:6,12)), k, 20)
names(c)
results <- mutate(results, clustering = factor(c$kpp))

#creo subgrupos con cada cluster
c1 <- results %>% filter(,clustering =='1') %>% print(,n = Inf)
c2 <- results %>% filter(,clustering =='2') %>% print(,n = Inf)
c3 <- results %>% filter(,clustering =='3') %>% print(,n = Inf)
#veo la proporcion de cada playlist en cada cluster
c1 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c2 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c3 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))






#EJEMPLO 2: juntamos playlist de generos variados
#musica clasica: 37i9dQZF1DWWEJlAGA9gs0
#metal clasico: 37i9dQZF1DX2LTcinqsO68
#pop internacional 37i9dQZF1DX1ngEVM0lKrb

plist1 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1DWWEJlAGA9gs0')
plist2 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1DX2LTcinqsO68')

plist3 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1DX1ngEVM0lKrb')

plist <- rbind(plist1,plist2,plist3)

plist <- select(plist, c(2,'track.name',6:13,15,16))
names(plist)
plist <- select(plist, c(1,2,3,4,6,9,10,11,12))
plist <- select(plist, c(1,2,3,4,6,7,8,9))

#voy a medir la correlacion entre las variables primero
M <- cor(select(plist,3:8))
corrplot(M)
names(plist)

#aqui he hecho clustering de las variables que me parecieron mas distintivas: energy,dance, instrumentalness
C <- NbClust(data =  select(plist,c(3,4,6,7)), distance = 'euclidean', method = 'ward.D')
C
results <- select(plist,c('track.name','playlist_name'))
results <- mutate(results, class = factor(C$Best.partition))
results <- results %>% arrange(,class)
#creo subgrupos con cada cluster
c1 <- results %>% filter(,class =='1') %>% print(,n = Inf)
c2 <- results %>% filter(,class =='2') %>% print(,n = Inf)
c3 <- results %>% filter(,class =='3') %>% print(,n = Inf)
#veo la proporcion de cada playlist en cada cluster
c1 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c2 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))
c3 %>% group_by(playlist_name) %>%summarise(no_rows = length(playlist_name))



#Conclusiones: veo como usar variables a la misma escala y ademas observar cuales estan relacionadas antes
#para no coger todas las relacionadas entre sí es útil. Ahora separa la musica clasica del metal y el pop, pero
#no disitngue metal y pop


#Trabajar otras cantidades para utilizar en el clustering (tempo, loudness ???, key o mode)





#Voy a ver la silueta de la aprticion anterior para ver por que no da un valor de 3

#plot de valores de SH
SH <- c()
nclust <- c(2:6)
for (i in 2:6) {
  SH_value <- plist %>% select(,c(3,4,6,7)) %>% NbClust(,min.nc = i, max.nc = i, method = "kmeans", index = "silhouette")
  SH <- cbind(SH,SH_value$Best.nc)
}
SHplotting <- data.frame(nclust,SH[2,])
SH_graph <- ggplot(data = SHplotting) + geom_point(mapping = aes( x = SHplotting[,1], y = SHplotting[,2]), size = 5, color = "red") + 
  geom_line( mapping = aes( x = SHplotting[,1], y = SHplotting[,2]), color = "blue", size = 1) +
  labs(title = "Silhouette en función del número de clusters") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
SH_graph
SHplotting











#Pablo
u <- get_user_audio_features('9v2h1n2bq7qf80yz9cwb3kfmb')


Sys.setenv(SPOTIFY_CLIENT_ID = '71fc7704316842e0beca8a2a40abe091')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b218329af8a34dec872949e34082fee0')

access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario

aut <- get_spotify_authorization_code()
