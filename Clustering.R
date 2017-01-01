library(cluster)
library(stats)

sample <- movies_fi[,-c(1,3)];View(head(sample))
dim(sample)
dist <- daisy(sample,metric = "gower")
summary(dist)

PCA <- prcomp(dist)
screeplot(PCA,type = "line")
movies_fi.PCA <- data.frame(PCA$x)
movies_fi.PCA <- movies_fi.PCA[,c(1:7)]
head(movies_fi.PCA)

object.size(movies_fi.PCA)
object.size(movies_fi)

library(fpc)
library(cluster)

# Now we can apply clustering methods here

(nrow(movies_fi.PCA)/2)^(1/2)  # 58.06892
#Then number of cluster should be near 58 (most probably less than 58)

# cluster <- pamk(movies_fi.PCA,krange = 50:53,criterion = "asw")  #<-- toooo slow

# applying kmeans

Myscreeplot <- function(data, n1 = 1,n2 =15, seed=4495){
  if(n1 == 1){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:n2){
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:n2, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares",main = "Scree Plot")
  }
  wss <- NULL
  j = 1
  for (i in n1:n2){
    set.seed(seed)
    wss[j] <- sum(kmeans(data, centers=i)$withinss)
    j = j+1
    }
  plot(n1:n2, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",main = "Scree Plot")}

Myscreeplot(movies_fi.PCA, n1=4,n2=50) 
# 19 clusters seems to be good deal
set.seed(4495)
movies.cluster <- kmeans(movies_fi.PCA,centers=19)
unique(movies.cluster$cluster)
head(movies.cluster$cluster)
summary(movies.cluster)

clusplot(movies_fi.PCA,movies.cluster$cluster,color = T,main = "19 clusters")
plotcluster(movies_fi.PCA,movies.cluster$cluster)

moviesandcluster <- data.frame(movieId=movies_fi$movieId,cluster = movies.cluster$cluster,
                               averageRating=AvgRating$averageRating)
moviesandcluster<-moviesandcluster[order(moviesandcluster$cluster,moviesandcluster$averageRating),]

# I don't think we should recommend movies with average rating less than 2 to our user
# Therefore, let's remove this movies 

moviesandcluster <- subset(moviesandcluster,moviesandcluster$averageRating>=2)


