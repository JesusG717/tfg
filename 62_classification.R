#CLASIFICATION
source("01_setup.R")
load("track_features.RData")
#REPASAR CON LOS NUEVOS DATOS
glimpse(track_features)


clust1 <- track_features %>% select(-c("track.id","genres")) %>% NbClust(method = "kmeans")
clust2 <- track_features %>% select(-c("track.id","genres")) %>% NbClust(method = "complete")
#for dbscan epsilon and minpts have to be determined
# rule of thumb: MinPts = 2*dimension of data. This data has a lot of duplicates so we will do 3*dim
MinP <- 3*ncol(track_features %>% select(-c("track.id","genres")))
neigh <- track_features %>% select(-c("track.id","genres")) %>% kNN(2)
d <- neigh$dist[,2]
ep <- 0.95*max(d)

clust3 <- track_features %>% select(-c("track.id","genres")) %>% dbscan(,eps = ep, minPts = MinP)
#do a kmeans after filtering noise points
aux <- track_features %>% mutate(dbs=factor(clust3$cluster), N = c(1:nrow(track_features))) %>% filter(dbs != 0)
clust4 <- aux %>% select(-c("dbs","track.id","genres","N")) %>% NbClust(method = "kmeans",min.nc = 2,max.nc = 4,index = "ch")
aux <- aux %>% mutate(km_filter = factor(clust4$Best.partition))


#add the clustering results to the main data
analysis_alldata <- track_features %>% select(c("track.id","genres"))
analysis_alldata <- analysis_alldata %>% mutate(N = c(1:nrow(track_features)))
#to add the km_filter data
aux2 <- aux %>% select("N","km_filter")
analysis_alldata <- left_join(analysis_alldata,aux2, by = c("N"))
analysis_alldata <- analysis_alldata %>% select(-c("N"))
analysis_alldata <- analysis_alldata %>% mutate(dbs = factor(clust3$cluster),km = factor(clust1$Best.partition), hc = factor(clust2$Best.partition))
#Swap Nas to 0 in km_filter
a <- rep(0,ncol(analysis_alldata))
analysis_alldata <- add_row(analysis_alldata,km_filter = "0")
analysis_alldata <- analysis_alldata %>% mutate(km_filter = as.factor(analysis_alldata$km_filter))
analysis_alldata$km_filter %>% replace_na(0)
analysis_alldata <- analysis_alldata[-4103,]
summary(analysis_alldata)

#save the file with the clusterings
save(analysis_alldata, file = "analysis_alldata.RData")

