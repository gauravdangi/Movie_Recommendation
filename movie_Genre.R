#This code will convert all the genres into seperate columns and will give 1 or 0 depending on whether
#a particular genre is present or not

library(tm)

head(movies)
#####create a dataframe having the the columns of movies
movieG<-data.frame(movieId=movies$movieId,title=movies$title,genres=as.character(movies$genres))
head(movieG)


#####Separating genre to each column(1=true and 0=false)
movieG$Adventure<-ifelse(grepl("Adventure",movies$genres),1,0)
movieG$Animation<-ifelse(grepl("Animation",movies$genres),1,0)
movieG$Children<-ifelse(grepl("Children",movies$genres),1,0)
movieG$Comedy<-ifelse(grepl("Comedy",movies$genres),1,0)
movieG$Fantasy<-ifelse(grepl("Fantasy",movies$genres),1,0)
movieG$Romance<-ifelse(grepl("Romance",movies$genres),1,0)
movieG$Thriller<-ifelse(grepl("Thriller",movies$genres),1,0)
movieG$Cirme<-ifelse(grepl("Crime",movies$genres),1,0)
movieG$Drama<-ifelse(grepl("Drama",movies$genres),1,0)
movieG$Action<-ifelse(grepl("Action",movies$genres),1,0)
movieG$FilmNoir<-ifelse(grepl("Film",movies$genres),1,0)
movieG$Horror<-ifelse(grepl("Horror",movies$genres),1,0)
movieG$SciFi<-ifelse(grepl("Sci",movies$genres),1,0)
movieG$War<-ifelse(grepl("War",movies$genres),1,0)
movieG$Western<-ifelse(grepl("Western",movies$genres),1,0)
movieG$Mystery<-ifelse(grepl("Mystery",movies$genres),1,0)
movieG$Fantasy<-ifelse(grepl("Fantasy",movies$genres),1,0)
movieG$Romance<-ifelse(grepl("Romance",movies$genres),1,0)
movieG$nogenreslisted<-ifelse(grepl("listed",movies$genres),1,0)
movieG$Documentary<-ifelse(grepl("Documentary",movies$genres),1,0)
movieG$Musical<-ifelse(grepl("Musical",movies$genres),1,0)
movieG$genres<-NULL




