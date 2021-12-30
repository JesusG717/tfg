#Pruebas con datos de spotify
source('01_setup.R')

#paquete para la api de spoti
library('spotifyr')

#conseguir token de la api
Sys.setenv(SPOTIFY_CLIENT_ID = '44855249207d40619183b073132a6407')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2296850888224ff0bb4e35ad23962578')

access_token <- get_spotify_access_token()


#prueba 1: te saca en orden las escalas mas usadas del artista
yo <- get_artist_audio_features('Sepultura')

sub <- select(yo, c('energy','danceability','key_mode','loudness'))
summary(sub)
sub %>% count(key_mode, sort = TRUE) %>% head(5) %>% kable()


#prueba 2: datos de un usuario

u <- get_user_playlists('ez74nswjvds93w2btv59sqwb4')

