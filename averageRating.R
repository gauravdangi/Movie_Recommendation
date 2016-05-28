library(ggplot2)
library(dplyr)
library(sqldf)

movies <- read.csv("F:/Projects/R Projects/Recommender system/ml-20m/movies.csv")
ratings <- read.csv("F:/Projects/R Projects/Recommender system/ml-20m/ratings.csv")


#Calculating average user ratings
a=ratings$userId
user_c = data.frame(table(a))
colnames(user_c)[1]<-"userId"
colnames(user_c)[2]<-"no_of_reviews"
head(user_c)
mean(user_c$no_of_reviews)    #144.4135
ggplot(data=user_c,aes(x=user_c$userId,y=user_c$no_of_reviews))+geom_point()

ratingMatrix<-ratings%>%select(movieId,rating)%>%arrange(movieId)

#how many user rated particular movie
b=ratings$movieId
movie_c = data.frame(table(b))
colnames(movie_c)[1]<-"movieId"
colnames(movie_c)[2]<-"no_of_reviews"
head(movie_c)
#Select movies which has more than 200 reviews
c=movie_c$no_of_reviews>200
movie_cNew=movie_c[c,]  
#Now movie_cNew contain movies with more than 100 reviews

#Now we need to find average ratings of movies
ratingMatrix<-ratings%>%select(movieId,rating)%>%arrange(movieId)
head(ratingMatrix)
AvgRating<-sqldf('select movieId, avg(rating) as averageRating FROM ratingMatrix group by movieId')
AvgRating<-AvgRating[c,]
#Now average movie rating dataset contains average rating of movies which are reviewed by more than 100 users



