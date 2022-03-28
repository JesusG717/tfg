#Data preparation and classification
#Loading and setting the data obtained in Data_collection
source("01_setup.R")
load("tracks_genre_features.RData")
#get rid of unnecessary variables (artist.id...)
track_features <- select(tracks_genre_features,c(1,3,5:15))
#filter genres
track_features %>% group_by(genres) %>% count() %>% arrange(desc(n))

#trap latino, trap triste, trap argentino are repeated, so we remove them
track_features <- track_features %>% filter(!grepl("trap latino|trap triste|trap argentino",genres))
#remove all varibles with less than 15 elements


#DUDA como hacer esto de forma mecanica


low <- c("trance mexicano|west australian hip hop|electro|latin|latin hip hop|reggaeton|deep groove house|brazilian edm|brostep|complextro|mexican edm|pop rap|rawstyle|canadian metal|industrial metal|pop|rap rock|frenchcore|hardstyle|filter house|full on|post-teen pop|trance|alt z|alternative r&b|candy pop|dark pop|electropop|metropopolis|spanish indie pop|swedish electropop|swedish synthpop|viral pop|australian dance|melbourne bounce|melbourne bounce international")
track_features <- track_features %>% filter(!grepl(low,genres))


#standarize all varaibles
track_features <- track_features %>% mutate(, tempo = (tempo-min(tempo))/(max(tempo)-min(tempo)))
track_features <- track_features %>% mutate(, loudness = (loudness-min(loudness))/(max(loudness)-min(loudness)))

#removing mode and key variables for now
#mode is 0 if in minor, 1 if in major
#key is an integer 1 to 11 according to the pitch

features <- track_features %>% select(-c("key","mode"))

#classification
#first test
clust1 <- track_features %>% select(-c("track.id","genres")) %>% NbClust(method = "kmeans",min.nc = 2,max.nc = 4,index = "ch")
clust2 <- track_features %>% select(-c("track.id","genres")) %>% NbClust(method = "complete",min.nc = 2,max.nc = 4,index = "ch")
#for dbscan epsilon and minpts have to be determined
# rule of thumb: MinPts = 2*dimension of data. This data has a lot of duplicates so we will do 3*dim
MinP <- 3*ncol(track_features %>% select(-c("track.id","genres")))
neigh <- track_features %>% select(-c("track.id","genres")) %>% kNN(2)

ep <- 0.95*max(d)

clust3 <- track_features %>% select(-c("track.id","genres")) %>% dbscan(,eps = ep, minPts = MinP)
#do a kmeans after filtering noise points
aux <- track_features %>% mutate(dbs=factor(clust3$cluster), N = c(1:nrow(track_features))) %>% filter(dbs != 0)
clust4 <- aux %>% select(-c("dbs","track.id","genres","N")) %>% NbClust(method = "kmeans",min.nc = 2,max.nc = 4,index = "ch")
aux <- aux %>% mutate(km_filter = factor(clust4$Best.partition))


#add the clustering results to the main data
analysis <- track_features %>% select(c("track.id","genres"))
analysis <- analysis %>% mutate(N = c(1:nrow(track_features)))
#to add the km_filter data
aux2 <- aux %>% select("N","km_filter")
analysis <- left_join(analysis,aux2, by = c("N"))
analysis <- analysis %>% select(-c("N"))
analysis <- analysis %>% mutate(dbs = factor(clust3$cluster),km = factor(clust1$Best.partition), hc = factor(clust2$Best.partition))
#Swap Nas to 0 in km_filter
a <- rep(0,ncol(analysis))
analysis <- add_row(analysis,km_filter = "0")
analysis <- analysis %>% mutate(km_filter = as.factor(analysis$km_filter))
analysis$km_filter %>% replace_na(0)
analysis <- analysis[-4103,]
summary(analysis)
#seems like grp 2 of km is grp 1 of hc
#a analizar bro


#PCA
pca_f <- features %>% select(-c("track.id","genres")) %>% prcomp()
summary(pca_f)
as.tibble(pca_aux$x[,1:6])