#Data preparation and classification
#Loading and setting the data obtained in Data_collection
source("01_setup.R")
load("tracks_genre_features.RData")
#DATA FILTERING/WRANGLING
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

#save a general file with all decently populated genres together
save(track_features,file = "track_features.RData")

#SUBGROUPS OF THE DATA TO CLASSIFY
#create subgroups with a few amount of genres for more accurate interpretations

track_features %>% group_by(genres) %>% count() %>% arrange(desc(n))
#mix genres that can be considered the same
#creating genre: lo-fi
lofi <- track_features$genres %>% replace(which(track_features$genres =="lo-fi jazzhop"
                                                |track_features$genres =="christian lo-fi"
                                                |track_features$genres =="chillhop"
                                                |track_features$genres =="lo-fi beats"),"lo-fi")
filtro1 <- track_features %>% mutate(genres = lofi)

#MIRAR PORQUE NO VA
prueba <- filtro1 %>% distinct(track.id,genres,.keep_all = TRUE)
prueba %>% group_by(genres) %>% count() %>% arrange(desc(n))
filtro1 %>% group_by(genres) %>% count() %>% arrange(desc(n))

df <- tibble(x= sample(10,100,rep=TRUE),y= sample(10,100,rep=TRUE))
df %>% distinct(x,y)



#PCA
pca_f <- features %>% select(-c("track.id","genres")) %>% prcomp()
summary(pca_f)
as.tibble(pca_aux$x[,1:6])