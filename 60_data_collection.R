#DATA GATHERING
#example from my own most listened tracks
source('01_setup.R')
library('spotifyr')
Sys.setenv(SPOTIFY_CLIENT_ID = '44855249207d40619183b073132a6407')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2296850888224ff0bb4e35ad23962578')
access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario
aut <- get_spotify_authorization_code()

#palylist tfg 6ZoGtHnhyHsXXYmFriZgyS
df <- get_playlist_audio_features(playlist_uris = "6ZoGtHnhyHsXXYmFriZgyS")
names_col <- names(df)
df <- df %>% select(c(which(names_col== "danceability"):which(names_col == "track.id"),"track.artists"))
df <- df[,c(12,13,1:11)]

tracks_and_artist <- df %>% select(track.id, track.artists) %>% unnest(track.artists) %>% 
  select(track.id, id) %>%  rename(artist.id = id)
#get_artists limit is 50
artists_info <- get_artists(tracks_and_artist[1:50,2]) %>% pull(artist.id) %>% unique() %>% as_tibble()
for (i in 2:floor(nrow(tracks_and_artist)/50)) {
  aux <- get_artists(tracks_and_artist[(i-1)*50:(i-1)*50+50,] %>% pull(artist.id) %>% unique()) %>% as_tibble()
  artists_info <- bind_rows(artists_info,aux)
}
save(artists_info, file ="artists_info.RData")

artists_info <- artists_info %>% select(genres, id) %>%  rename(artist.id = id)
# artists_info %>% drop_na() %>% unnest(genres)
# glimpse(artists_info)
tracks_and_genres <- inner_join(tracks_and_artist, artists_info)  %>% drop_na() %>% unnest(genres)
# tracks_and_genres %>% group_by(genres) %>% count() %>% arrange(desc(n))
# para filtrar algunos generos
# tracks_and_genres %>% filter(genres %in% c("rock", "alternative rock"))
# para hacer que cada genero sea una columna
# tracks_and_genres %>% select(-artist.id) %>% distinct() %>% mutate(value = TRUE) %>% pivot_wider(names_from = genres, values_from = value, values_fill = FALSE)


#tengo en tracks_and_genres los tracks con su genero asociado, mientras que en tracks tengo los tracks con sus ids
tracks_genre_features <- left_join(tracks_and_genres,tracks,)
tracks_genre_features <- select(tracks_genre_features,c(1:15))
save(tracks_genre_features, file = "tracks_genre_features.RData")










#DATOS VIEJOS
# 
# 
# 
# #selecting my own playlists
# plists <- get_my_playlists()
# plists <- rbind(plists,get_my_playlists(offset = 19))
# #get each playlist's tracks
# first <- get_playlist_audio_features(playlist_uris = plists$id[1])
# tracks <- tibble(select(first,c(1,6:17,28)))
# #I skip playlists taht cant be collected (fusion playlists and daily mics most likely)
# for (i in c(3:7,9,10,13:29)) {
#   aux <- get_playlist_audio_features(playlist_uris = plists$id[i])
#   aux <- select(aux,c(1,6:17,28))
#   tracks <- add_row(tracks,aux)
# }
# tracks <- tracks[,c(1,13,2:12,14)]
# glimpse(tracks)
# #get genre
# tracks_and_artist <- tracks %>% 
#   select(track.id, track.artists) %>% 
#   unnest(track.artists) %>% 
#   select(track.id, id) %>%
#   rename(artist.id = id)
# #get_artists limit is 50
# artists_info <- get_artists(tracks_and_artist[1:50,] %>% pull(artist.id) %>% unique()) %>% as_tibble()
# for (i in 2:floor(nrow(tracks_and_artist)/50)) {
#   aux <- get_artists(tracks_and_artist[(i-1)*50:(i-1)*50+50,] %>% pull(artist.id) %>% unique()) %>% as_tibble()
#   artists_info <- bind_rows(artists_info,aux)
# }
# save(artists_info, file ="artists_info.RData")
# glimpse(artists_info)
# artists_info <- artists_info %>% select(genres, id) %>%  rename(artist.id = id)
# artists_info %>% drop_na() %>% unnest(genres)
# glimpse(artists_info)
# tracks_and_genres <- inner_join(tracks_and_artist, artists_info)  %>% drop_na() %>% unnest(genres)
# tracks_and_genres %>% group_by(genres) %>% count() %>% arrange(desc(n))
# # para filtrar algunos generos
# tracks_and_genres %>% filter(genres %in% c("rock", "alternative rock"))
# # para hacer que cada genero sea una columna
# tracks_and_genres %>% select(-artist.id) %>% distinct() %>% mutate(value = TRUE) %>% pivot_wider(names_from = genres, values_from = value, values_fill = FALSE)
# 
# 
# #tengo en tracks_and_genres los tracks con su genero asociado, mientras que en tracks tengo los tracks con sus ids
# tracks_genre_features <- left_join(tracks_and_genres,tracks,)
# tracks_genre_features <- select(tracks_genre_features,c(1:15))
# save(tracks_genre_features, file = "tracks_genre_features.RData")
