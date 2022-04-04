#variable analysis

load('track_features.RData')
groups <- names(track_features %>% select(-c("track.id","genres")))
track_features %>% select(-c("track.id","genres")) %>% bartlett.test(,groups)
ks.test(track_features$tempo,pnorm)

#PCA
pca_f <- track_features %>% select(-c("track.id","genres")) %>% prcomp()
summary(pca_f)
as.tibble(pca_f$x[,1:6])
pca_f$rotation
