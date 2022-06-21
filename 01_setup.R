#Aqui pongo todos los paquetes que voy a usar

library('tidyverse')
library('RWeka')
library('fclust')
library('NbClust')
#paquete para el kmeanspp
library('LICORS')
#cluster me a?ade daisy() para usar distancias para clustering categorico (ver jerarquico)
library('cluster')
#dbscan y hdbscan
library('dbscan')
#pintar graficas una al lado de otra
library('gridExtra')
library('grid')
#para poner graficas juntas
library('patchwork')
#para meter optics al ggplot
library('opticskxi')
#mas mierda de hacer tablitas y eso
library(knitr)
#cosas para spoti
library('httpuv')
#paquete para mejor visualizacion
library('factoextra')
#extension a ggplot
library('GGally')
#hacer treemaps
library('treemapify')
#para el rand y otros comadnos cluster.stats
library('fpc')



'%!in%' <- function(x,y)!('%in%'(x,y))


#SPOTIFY
library('spotifyr')
Sys.setenv(SPOTIFY_CLIENT_ID = '44855249207d40619183b073132a6407')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '2296850888224ff0bb4e35ad23962578')
access_token <- get_spotify_access_token()
#autorizar a coger datos de usuario
aut <- get_spotify_authorization_code()