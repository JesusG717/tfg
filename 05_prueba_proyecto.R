#Intento de primer proyecto con spoti
#La idea es tomar una gran cantidad de datos, con genero de cancion, playlist o usuario del que toma el track
#y crear samples de esa poblacion para comparar la efectividad de los algoritmos de clustering en ejemplos reales

source('01_setup.R')

#paquete para la api de spoti
library('spotifyr')

#conseguir token de la api
Sys.setenv(SPOTIFY_CLIENT_ID = '44855249207d40619183b073132a6407')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2296850888224ff0bb4e35ad23962578')

access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario

aut <- get_spotify_authorization_code()


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

