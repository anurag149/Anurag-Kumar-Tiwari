install.packages("plyr")
library(plyr)

x <-  runif(25) # generating 50 random numbers
x

y <-  runif(25) # generating 50 random numbers 
y

data <- cbind(x,y) 
data

plot(data)

plot(data, type="n")
text(data, rownames(data))

km <- kmeans(data,24) #kmeans clustering
str(km)

install.packages("animation")
library(animation)

km <- kmeans.ani(data, 4)
km$centers


#EastWestAirlines. = read.csv(file.choose())
View(EastWestAirlines.)
normalized_data <- scale(EastWestAirlines.[2:12])
fit <- kmeans(normalized_data, 3) # 4 cluster solution
str(fit)
final2<- data.frame(EastWestAirlines., fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(EastWestAirlines.[,2:12], by=list(fit$cluster), FUN=mean)

#elbow curve & k ~ sqrt(n/2) to decide the k value

#wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
twss = c()
for (i in 2:15) twss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:15, twss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
View(twss)
km$withinss



# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
xcl <- clara(xds, 2, sample = 100)
clusplot(xcl)




#Partitioning around medoids
xpm <- pam(xds, 2)
clusplot(xpm)

