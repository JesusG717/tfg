library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
# load(analysis_alldata)
# load(tracks_genre_features)

############################

# Para ver cuantas hay de cada key
ggplot(tracks_genre_features %>%
         distinct(track.id, key, mode), # quitar repetidas
       aes(as.factor(key))) +
  xlab("Key") + # para que no salga el as.factor
  geom_bar(fill = "lightblue", color = "black") +
  theme_bw() # esto no hace falta, es porque a mi no me gusta el fondo gris
# les cambié en color del interior (fill) y el borde (color) para que veas
# como se hace si te apetece hacerlo. Ponlo como quieras

# el mismo de antes pero dividiendo key y mode
ggplot(tracks_genre_features %>%
         distinct(track.id, key, mode), # quitar repetidas
       aes(as.factor(key), fill = factor(mode))) +
  geom_bar(position = "dodge") + # dodge para que salgan una al lado de otro
  labs(x = "Key", fill = "Mode") + # cambiar eje x y tit leyenda
  theme_bw()
# ahora el fill va dentro de aes porque rellenamos en función de una variable
# del conjunto de datos, no es colorear para que quede guapo

# el mismo de antes pero cambiando etiquetas y colores
ggplot(tracks_genre_features %>%
         distinct(track.id, key, mode), # quitar repetidas
       aes(as.factor(key), fill = factor(mode))) +
  geom_bar(position = "dodge") + # dodge para que salgan una al lado de otro
  labs(x = "Key", fill = "Mode") + # cambiar eje x y tit leyenda
  scale_fill_brewer(palette = "Dark2") + # para que veas como se hace por si te apetece
  # hay más paletas aquí https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
  scale_x_discrete(labels = c("Do",
                              "Do#/Reb",
                              "Re",
                              "Re#/Mib",
                              "Mi", 
                              "Fa", 
                              "Fa#/Solb",
                              "Sol",
                              "Sol#/Lab",
                              "La",
                              "La#/Sib",
                              "Si")) +
  theme_bw()

# Orden de sostenidos: fa, do, sol, re, la, mi, si
# Orden de bemoles (el inverso): si, mi, la, re, sol, do, fa
# La escala más básica que hay es Do M.
# La escala mayor tiene un num de sostenidos igual a quitar las dos primeras
# notas del orden de sostenidos y contar el num de notas restantes hasta
# la que da nombre a la escala. Cuando sepamos el num empezamos desde el
# principio otra vez para saber qué sostenidos tiene
# Por ejemplo, Re M (que como es M mayúscula se lee Re Mayor)
# Quitamos fa, do. Ahora contamos hasta Re, es decir: la escala tiene dos sostenidos
# Volvemos al principio de la lista para saber los sostenidos: son fa y do
# Con esta técnica. Sol M un sostenido, Re M dos sostenidos, La M 3, Mi M 4...
# Para los bemoles. Las escalas mayores solo y tienen bemoles si su key tiene un b
# a excepción de Fa M, que tiene el si b. 
# Para saber el num de bemoles se cuenta desde el principio pero añadiendo el fa delante
# por ejemplo, La b M. Fa (1), Si (2), Mi (3), La (4). Tiene 4b
# ahora empezamos la lisa para saber qué bemoles: si, mi, la, re
# ESCALAS MENORES:
# el num de alteraciones se determina por su relativo mayor. 
# El relativo mayor tiene es la escala mayor cuya key es la nota un tono y medio
# por encima de la key del relativo mejor
# Por ejemplo: La. Un tono y medio por encima tiene la nota Do 
# (porque hay 1 tono de la a si, medio tono de si a do)
# Por lo tanto si consideramos la escala La m (menor porque la m es minúscual), 
# ésta tendrá las mismas alteraciones que su relativo mayor, es decir, 
# que Do M. O sea, ninguna
# Todo esto se llama ciclo de quintas. En esta imagen se ve bien cada tono,
# sus alteracions y su relativo
# https://images.app.goo.gl/EVjHG4jdz4dqE73RA

# Ahora el plot anterior para que quede más claro con las etiquetas
mayores <- 
  ggplot(tracks_genre_features %>%
         distinct(track.id, key, mode) %>%
         filter(mode == 1), 
       aes(as.factor(key))) +
  geom_bar(position = "dodge", fill = "darkgreen") + 
  labs(x = "Key", fill = "Mode") + 
  # hay más paletas aquí https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
  scale_x_discrete(labels = c("Do M",
                              "Reb M\n(5b)",
                              "Re M\n(2#)",
                              "Mib M\n(3b)",
                              "Mi M\n(4#)", 
                              "Fa M\n(1b)", 
                              "Fa# M/Solb M\n(6#/6b)",
                              "Sol M\n(1#)",
                              "Lab M\n(4b)",
                              "La M\n(3#)",
                              "Sib M\n(2b)",
                              "Si M\n(5#)")) +
  theme_bw()
menores <- 
  ggplot(tracks_genre_features %>%
           distinct(track.id, key, mode) %>%
           filter(mode == 0), 
         aes(as.factor(key))) +
  geom_bar(position = "dodge", fill = "orange") + 
  labs(x = "Key", fill = "Mode") + 
  # hay más paletas aquí https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
  scale_x_discrete(labels = c("Do m\n(3b)",
                              "Do# m\n(4#)",
                              "Re m\n(1b)",
                              "Re# m/Mib m\n(6#/6b)",
                              "Mi m\n(1#)", 
                              "Fa m\n(4b)", 
                              "Fa# m\n(3#)",
                              "Sol m\n(2b)",
                              "Sol# m\n(5#)",
                              "La m",
                              "Sib m\n(5b)",
                              "Si m\n(2#)")) +
  theme_bw()
mayores + menores

# para hacer scatterplots: energy and valence
ggplot(tracks_genre_features %>%
         distinct(track.id, energy, valence), aes(energy, valence)) +
  geom_point() +
  theme_bw()

# te pongo ejemplos de más cosas, hazlo con las variables que quieras

# coloreando con otra variable
ggplot(tracks_genre_features %>%
         distinct(track.id, energy, valence, mode), 
       aes(energy, valence, color = as.factor(mode))) +
  geom_point() +
  theme_bw()

# distribuciones
ggplot(track_features, aes(danceability, 
                                  group = factor(genres), 
                                  color = factor(genres))) +
  geom_boxplot() +
  theme_bw()

# densidades
ggplot(tracks_genre_features, aes(energy, 
                                  group = factor(mode), 
                                  color = factor(mode))) +
  geom_density() +
  theme_bw()

# Gráfico de barras para ver de cada género cuantas hay
ggplot(analysis_alldata, aes(genres)) +
  geom_bar() + # una barra contando cuantos hay de cada género
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + # poner num
  theme_bw() # quitar el fondo gris

analysis_alldata %>% nrow() # 742 filas
analysis_alldata %>% distinct(track.id) %>% nrow() #solo 355 canciones distintas

# crear un dataframe que se quede solo con las canciones rock y metal.
# para cada cancion tiene estructura track.id rock  metal value   
# rock y metal son true o false si la cancion tiene ese genero
# value es la combinacion de rock y metal para poder hacer un plot de las que
# tienen uno, otro, o los dos
ggplot(
  analysis_alldata %>%
    select(track.id, genres) %>%
    filter(genres %in% c("rock", "metal")) %>%
    mutate(is = TRUE) %>%
    pivot_wider(names_from = genres, values_from = is, values_fill = FALSE) %>%
    mutate(value = paste(rock, metal)),
  aes(value)) +
  scale_x_discrete(labels = c("rock", "metal", "ambas")) + # cambiar texto x
  xlab("") + # quitar etiqueta x
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + # poner num
  theme_bw()
