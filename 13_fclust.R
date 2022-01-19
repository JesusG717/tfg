#cosas del fuzzy

source('01_setup.R')

#elegimos el data aqui (mirar los nombres en la carpeta)
dataset<-'data/artificial/3MC.arff'
X<-read.arff(dataset)


C_FKM<- X %>% select(c(1,2)) %>% Fclust(k,'FKM')
#mirar todas las opciones del paquete fclust para ver como pintar cosas diferente
plot_fkm <- plot.fclust(C_FKM,ucex = TRUE, pca = FALSE)
C_GK<- X %>% select(c(1,2)) %>% Fclust(k,'gk')
plot_gk <- plot.fclust(C_GK,ucex = TRUE, pca = FALSE)
layout <- c(area(1,1),area(1,2))
plot.fclust(C_FKM,ucex = TRUE, pca = FALSE)
plot.fclust(C_GK,ucex = TRUE, pca = FALSE)

X <- mutate(X, gk = factor(C_GK$clus[,1]))
X <- mutate(X, U_gk = C_GK$clus[,2])

X <- mutate(X, fkm = factor(C_FKM$clus[,1]))
X <- mutate(X, U_fkm = C_FKM$clus[,2])

plot_fkm <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = fkm,size = U_fkm)) + 
  labs(title = "Fuzzy k-means", colour = "Clusters", size = "Membership") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_gk <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = gk,size = U_gk)) + 
  labs(title = "Gustafson Kessel", colour = "Clusters", size = "Membership") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_real <- ggplot(data = X)+geom_point(mapping = aes(x = X[,1], y = X[,2], color = class)) + 
  labs(title = "Real Data Distribution", colour = "Clusters") + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
grid.arrange(plot_fkm,plot_gk,plot_real,ncol = 3,top = 'Comparacion del metodo GK con el FCM')
