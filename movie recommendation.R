library(ggplot2)
library(dplyr);library(data.table)
library(sqldf)
library(recommenderlab)
library(reshape2)
#Merging AvergeRating dataset to movieG data using sql
movies_fi=sqldf("SELECT * from AvgRating as a LEFT JOIN movieG as m ON a.movieId=m.movieId")
movies_fi<-movies_fi[-3]
head(movies_fi)
 #Now we need to create a correlation matrix
#We need to find correlation between to movies

cor(movies_fi$averageRating,movies_fi)   #Drama movies have higher average rating compared to other genre followed by war and FilmNoir
#We need a data that contains userID, movie names and their ratings bu particular user
user_movies<-sqldf("select userId,title,movieId,rating 
                  From ratings a,movies_fi b
                  where a.movieId = b.movieId
                  ORDER BY userId")

user_movies <- merge(ratings,movies_fi,all.x = T)

user_movie <- select(ratings$userId,ratings$title,rating)
user_movie <- merge()

##-----------------------------------------------------------------------------------------

attach(ratings)
matrix <- dcast(ratings,userId~movieId,value.var = "rating", na.rm=FALSE)
# Error: cannot allocate vector of size 27.6 Gb

detach(ratings)
G<-as(user_movies,"realRatingMatrix")   #this is matrix of size = 144.3 Mb

UBCF<-Recommender(G[1:nrow(G)],method="UBCF")  #UCBF: User based collabarating filtering

IBCF<-Recommender(G,method="IBCF")  #ICBF: Item based collabarating filtering

PBCF<-Recommender(G,method="POPULAR")

print(UBCF)

#now recommend top 5 movies to user 3 using user based collorating filtering
recom.user<-predict(UBCF,G[3,],n=5)
as(recom.user,"list")

#recommend movies to user
recom.user_PBCF<-predict(PBCF,G,n=5)
ratings$timestamp <- NULL

#---------------Lets try something different-------------------------
counting <- function(vect){
  return(data.frame(table(vect)))
}


# CountOccurrences function
CountOccurrences <- function(vect){
  count = 1
  occ <- NULL
  j=1
  for(i in 1:(length(vect))){
    if(i == length(vect)){
      occ[j] <- count
    }
    else if(clust[i] == clust[i+1]){
      count = count + 1
    }
    else {
      
      occ[j] <- count
      count = 1
      j = j+1
    }
  }
  return(data.frame(Vector = unique(vect),Occurrence = occ))
  
}
rating <- as.data.table(ratings)

# Recommending function
recommending <- function(Id)
{
  rating <- subset(ratings,ratings$userId==Id & ratings$rating>=4)
  if(nrow(rating)<10){
    rating <- NULL
    rating <- subset(ratings,ratings$userId==Id & ratings$rating>=3)
  }

# So these are the movies that user really liked
clust<-moviesandcluster[moviesandcluster$movieId %in% rating$movieId,]$cluster

#clust.info <- CountOccurrences(clust) 
clust.info <- counting(clust) 
names(clust.info) <- c("Cluster","Occurrences")
clust.info <- clust.info[order(clust.info$Occurrence,decreasing = T),]
recomm <- subset(moviesandcluster,!moviesandcluster$movieId %in% rating$movieId)
recomm <- subset(moviesandcluster,moviesandcluster$cluster %in% c(clust.info[1,1],clust.info[1,2],clust.info[1,3]))
return(head(recomm[order(recomm$averageRating,decreasing = T),],10)$movieId)
}

recommending(2)
# [1]  2019  7502  1193  1203 58559   926  1207  1204  1254  1172
recommending(3)
# [1]  1198  3030  5618   260  1196  4993  2859  1201 93040  5952
recommending(1)
# [1]  1148  1197   969  3000  7099  1223 31658  6350 60069 78499
recommending(9)
# [1] 2959 3435 4973 5291  296  905 2203 2186  951  903

# Let's recommend to all users
recommendation <- data.frame()
users<-unique(user_c$userId)
a <- NULL

# Recommending to first 50 users
for(i in 1:50){
  recommendation[i,1] <- users[i]
  a = recommending(i)
  for(j in 2:11){
    recommendation[i,j] <- a[j-1]
  }
  
}
write.csv(recommendation,"Recommendation_to_50_users.csv")
