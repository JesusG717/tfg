#Intento de primer proyecto con spoti
#La idea es tomar una gran cantidad de datos, con genero de cancion, playlist o usuario del que toma el track
#y crear samples de esa poblacion para comparar la efectividad de los algoritmos de clustering en ejemplos reales
#NOTA: abajo del todo hay una prueba con mis playlist
source('01_setup.R')

#paquete para la api de spoti
library('spotifyr')

#conseguir token de la api
Sys.setenv(SPOTIFY_CLIENT_ID = '44855249207d40619183b073132a6407')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2296850888224ff0bb4e35ad23962578')

access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario

aut <- get_spotify_authorization_code()



#lo primero es saber dejar los datos listos para analizar
#ahora quiero coger una muestra y sacar de ahi las variables para hacer clustering y los generos
#mis daily mixes (7/2):
#37i9dQZF1E37k7BOils0mb
#37i9dQZF1E3920mzHHtk0B
#37i9dQZF1E38JwgfTAgiXb
#37i9dQZF1E357mYJL8BeUV
#37i9dQZF1E37MgbROqdOgm
#37i9dQZF1E37MgbROqdOgm


plist1 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1E37k7BOils0mb')
plist2 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1E3920mzHHtk0B')
plist3 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1E38JwgfTAgiXb')
plist4 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1E357mYJL8BeUV')
plist5 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1E37MgbROqdOgm')
plist6 <- get_playlist_audio_features(playlist_uris = '37i9dQZF1E37MgbROqdOgm')

plist <- rbind(plist1,plist2,plist3,plist4,plist5,plist6)
#veo que variables tengo
names(plist)

#tengo que seguir buscando donde está el genero del track

#creo que solo da genero a los artistas, no a los tracks
#rhcp 0L8ExT028jH3ddEcZwqJJ5
#mandragora 2AasvmwafZPTgQANaoLoQY
#trueno 2x7PC78TmgqpEIjaGAZ0Oz
arti <- get_artist('0L8ExT028jH3ddEcZwqJJ5')


#efectivamente, solo da genero a los artistas
#voy a mirar como sacar el nombre del artista, porque viene en una lista dentro del data frame
artist <- plist$track.artists
artist <- lapply(artist, '[[',2) #esto me da la segunda columna de la lista, con sus elementos como string
#puedo cambiar '[[' por '[' y me da esa columna pero con otro formato
#la columna 2 es la que tiene las id de artista, que es lo que usare para sacar el genero

#ahora tengo un problema, tengo una lista con ids, pero las canciones con dos artistas tienen 2 strings en un elemento
#esto da error al usar get_artist

#la solucion es vovler a usar lapply como antes y te saca la columna que pidas. de momento voy a coger solo la 1
artist <- lapply(artist, '[[',1)
#get artists solo va de 50 en 50 T.T toca ir paso a paso
aux <- get_artists(artist[1:50])
aux <- rbind(aux,get_artists(artist[51:100]))
aux <- rbind(aux,get_artists(artist[101:150]))
aux <- rbind(aux,get_artists(artist[151:200]))
aux <- rbind(aux,get_artists(artist[201:250]))
aux <- rbind(aux,get_artists(artist[251:300]))

aux$genres

#le meto los generos a plist
plist <- mutate(plist,aux$genre)
names(plist)

#ya seguire









#Prueba con mis playlist y eso

#Primero voy a probar a sacar una serie de canciones de playlist de genero diferentes
#mi playlist de EDM chilling: 5KeK4yPMHYYsSslZtD1Eib
#una playlist de metal: 27gN69ebwiJRtXEboL12Ih
#mi playlist de rap tranquilo/trap: 0EF7zKkIrwPhrZsNr1koa0  
#mi playlist de hardstyle desquiciados: 7yl2L6SF5B1tjbp8vqb5wh

#necesito mas canciones de edm/house, asi que añado la playlist de tehcno chilling de pablo:
#5acI3aZUMKMp2uct2BDlas

plist1 <- get_playlist_audio_features(playlist_uris = '5KeK4yPMHYYsSslZtD1Eib')
plist2 <- get_playlist_audio_features(playlist_uris = '27gN69ebwiJRtXEboL12Ih')
plist3 <- get_playlist_audio_features(playlist_uris = '0EF7zKkIrwPhrZsNr1koa0')
plist4 <- get_playlist_audio_features(playlist_uris = '7yl2L6SF5B1tjbp8vqb5wh')
plist5 <- get_playlist_audio_features(playlist_uris = '5acI3aZUMKMp2uct2BDlas')

#antes de juntar los datos vamos a reducir el numero de datos de las playlist mas grandes de forma aleatoria:

plist2 <- plist2[sample(nrow(plist2),70), ]
plist3 <- plist3[sample(nrow(plist3),70), ]


#ahora contamos con aprox: 70 metal, 70 rap, 67 techno/house y 60 hardstyle
plist <- rbind(plist1,plist2,plist3,plist4,plist5)


names(plist)
#nos interesa solo las audio features y el nombre de la playlist y de la cancion

plist <- select(plist, c(2,'track.name',6:13,15,16))
names(plist)

#Ahora buscamos muestrear de esta poblacion grupos mas pequeños
muestra <- plist[sample(nrow(plist),120), ]
#eso es una muestra de 120 canciones random, ahora haremos clustering con varios metodos y veremos cual funciona mejor
#primero sera interesante pensar que variables seran las significativas y que es lo que busco encontrar

