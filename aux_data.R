tracks_genre_features %>% select(!colnames(tracks_genre_features %in% c("playlist_id")))

min <- min(tracks_genre_features$danceability)

range(tracks_and_artist$danceability)
tracks_genre_features %>%
  mutate(danceability = (danceability-min)/(max-min))

hc <- hclust(dist(tracks_genre_features[,6:7]))
clusters <- cutree(hc, k = 3)

resultados <- tibble(id = tracks_genre_features$track.id,
                     predict = clusters)

resultados
