#variable analysis
source('01_setup.R')
load('data_pca70.RData')
features <- data_pca70 %>% select(-c("track.id","genres"))
ggcorr(features)

#PCA
pca_f <- features %>% prcomp()
summary(pca_f)
as.tibble(pca_f$x[,1:6])
pca_f$rotation

#voy a prbar quedandome con 3 y 4 componentes
#3 componentes (73% de la varianza explicada)

data_pca <- pca_f$x %>% as_tibble()
data_pca70 <- data_pca %>% select(c(1:3))

#4 componenetes (>80% varianza explicada)
data_pca80 <- data_pca %>% select(c(1:4))

km_pca70 <- data_pca70 %>% NbClust(method = "kmeans")
km_pca80 <- data_pca80 %>% NbClust(method = "kmeans")

results_pca <- track_features %>% select("track.id","genres") %>% mutate(km70 = factor(km_pca70$Best.partition),
                                                          km80 = factor(km_pca80$Best.partition))
h_pca70 <- data_pca70 %>% NbClust(method = "complete")
h_pca80 <- data_pca80 %>% NbClust(method = "complete")

results_pca <- results_pca %>% mutate(h70 = factor(h_pca70$Best.partition),
                                      h80 = factor(h_pca80$Best.partition))
summary(results_pca)

# track.id            genres          km70    km80    h70     h80    
# Length:743         Length:743         1:312   1:118   1:381   1:706  
# Class :character   Class :character   2:223   2:625   2:178   2: 37  
# Mode  :character   Mode  :character   3: 77           3: 58          
#                                       4: 94           4: 89          
#                                       5: 37           5: 37          
tr_df <- results_pca %>% filter(km70==1) %>% group_by(genres) %>% count()
km70_1_tree <- ggplot(data = tr_df) + geom_treemap(mapping = aes(area = n,fill = genres))

tr_df <- results_pca %>% filter(km70==2) %>% group_by(genres) %>% count()
km70_2_tree <- ggplot(data = tr_df) + geom_treemap(mapping = aes(area = n,fill = genres))

tr_df <- results_pca %>% filter(km70==3) %>% group_by(genres) %>% count()
km70_3_tree <- ggplot(data = tr_df) + geom_treemap(mapping = aes(area = n,fill = genres))

tr_df <- results_pca %>% filter(km70==4) %>% group_by(genres) %>% count()
km70_4_tree <- ggplot(data = tr_df) + geom_treemap(mapping = aes(area = n,fill = genres))

tr_df <- results_pca %>% filter(km70==5) %>% group_by(genres) %>% count()
km70_5_tree <- ggplot(data = tr_df) + geom_treemap(mapping = aes(area = n,fill = genres))

tr_df <- results_pca  %>% group_by(genres) %>% count()
data_0_tree <- ggplot(data = tr_df) + geom_treemap(mapping = aes(area = n,fill = genres))

grid.arrange(data_0_tree,km70_1_tree,km70_2_tree,km70_3_tree,km70_4_tree,km70_5_tree,ncol = 2,nrow = 3)
