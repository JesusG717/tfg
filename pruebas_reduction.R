#variable analysis
source('01_setup.R')
load('track_features.RData')
features <- track_features %>% select(-c("track.id","genres"))
ggcorr(features)

#PCA
pca_f <- features %>% prcomp()
summary(pca_f)
as.tibble(pca_f$x[,1:6])
pca_f$rotation

#voy a prbar quedandome con 3 y 4 componentes
#3 componentes (73% de la varianza explicada)

pca_f$x %>% as_tibble()
