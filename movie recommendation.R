library(ggplot2)
library(dplyr)
library(sqldf)
library(recommenderlab)
library(reshape2)
#Merging AvergeRating dataset to movieG data using sql
movies_fi=sqldf("SELECT * from AvgRating a LEFT JOIN movieG m ON a.movieId=m.movieId")
movies_fi<-movies_fi[-3]

 #Now we need to create a correlation matrix
#We need to find correlation between to movies
mov=movies_fi[,-3]
cor(mov$averageRating,mov)   #Drama movies have higher average rating compared to other genre followed by war and FilmNoir
#We need a data that contains userID, movie names and their ratings bu particular user
user_movies<-sqldf("select userId,title,rating 
                  From ratings a,movies_fi b
                  where a.movieId = b.movieId
                  ORDER BY userId")



attach(movies_fi)
ggplot(movies_fi,aes(x=movieId,y=averageRating))+geom_boxplot()
#Computing inter quartile range
#It will measure variability of averageRating, based on dividing a data set into quartiles
IQR(movies_fi$averageRating)   #0.6931551




##-----------------------------------------------------------------------------------------



G<-as(user_movie,"realRatingMatrix")

#view g in matrix form
head(as(G,"matrix"))

rec<-Recommender(G,method="UBCF")  #UCBF: User based collabarating filtering

rec<-Recommender(G,method="IBCF")  #ICBF: Item based collabarating filtering

#now recommend top 5 movies to user 3 using user based collorating filtering
recom.user<-predict(rec,G[3,],n=5)
as(recom.user,"list")
