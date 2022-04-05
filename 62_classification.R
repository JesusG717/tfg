#CLASIFICATION
source("01_setup.R")
load("track_features.RData")

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
analysis_alldata <- analysis_alldata[-nrow(analysis_alldata),]
summary(analysis_alldata)

#save the file with the clusterings
save(analysis_alldata, file = "analysis_alldata.RData")


#trying to have equal data from each genre so thre is no overrepresentation

#pick around 50 rock and metal songs so there is no overrepresentation
r <- track_features %>% filter(genres %in% c("rock")) %>% slice_sample(n = 50)
m <- track_features %>% filter(genres %in% c("metal")) %>% slice_sample(n = 50)

sub_data <- track_features %>% filter(genres != "rock") %>% filter(genres != "metal")
sub_data <- sub_data %>% rbind(r,m)

s_features <- sub_data %>% select(-c("track.id","genres"))

km_ch <- s_features %>% NbClust(method = "kmeans")
# km_sh <- s_features %>% NbClust(method = "kmeans",index = "silhouette")

h_ch <- s_features %>% NbClust(method = "complete")
# h_sh <- s_features %>% NbClust(method = "kmeans",index = "silhouette")


#for dbscan epsilon and minpts have to be determined
# rule of thumb: MinPts = 2*dimension of data. This data has a lot of duplicates so we will do 3*dim
MinP <- 3*ncol(s_features)
neigh <- s_features %>% kNN(2)
d <- neigh$dist[,2]
ep <- 0.95*max(d)

db <- s_features %>% dbscan(,eps = ep, minPts = MinP)
#do a kmeans after filtering noise points
aux_s <- sub_data %>% mutate(dbs=factor(db$cluster), N = c(1:nrow(sub_data))) %>% filter(dbs != 0)
km_db <- aux_s %>% select(-c("dbs","track.id","genres","N")) %>% NbClust(method = "kmeans")
aux_s <- aux_s %>% mutate(km_filter = factor(km_db$Best.partition))


#add the clustering results to the main data
analysis_sub1 <- sub_data %>% select(c("track.id","genres"))
analysis_sub1 <- analysis_sub1 %>% mutate(N = c(1:nrow(sub_data)))
#to add the km_filter data
aux2_s <- aux_s %>% select("N","km_filter")
analysis_sub1 <- left_join(analysis_sub1,aux2_s, by = c("N"))
analysis_sub1 <- analysis_sub1 %>% select(-c("N"))
analysis_sub1 <- analysis_sub1 %>% mutate(dbs = factor(db$cluster),km = factor(km_ch$Best.partition), 
                                          hc = factor(h_ch$Best.partition))
#Swap Nas to 0 in km_filter
a <- rep(0,ncol(analysis_sub1))
analysis_sub1 <- add_row(analysis_sub1,km_filter = "0")
analysis_sub1 <- analysis_sub1 %>% mutate(km_filter = as.factor(analysis_sub1$km_filter))
analysis_sub1$km_filter %>% replace_na(0)
analysis_sub1 <- analysis_sub1[-nrow(analysis_sub1),]
summary(analysis_sub1)

#save the file with the clusterings
save(analysis_sub1, file = "analysis_sub1.RData")


