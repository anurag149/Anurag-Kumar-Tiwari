# Load the data set
input = read.csv("E:/Day Wise/Day 18 Data Mining - Unsupervised (Clustering)/Data/Universities_Clustering.csv")
summary(input)


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(input[,2:7],normalize))
summary(normalized_data)

normalized_data <- scale(input[,2:7]) #excluding the university name columnbefore normalizing
summary(normalized_data)

d <- dist(normalized_data, method = "euclidean")# distance matrix
fit <- hclust(d, method="complete")
?hclust
?dist
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=3) # cut tree into 3 clusters

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust

membership<-as.matrix(groups)

final <- data.frame(input, membership)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

aggregate(input[,2:7], by=list(final$membership), FUN=mean)

?write.xlsx

write.csv(final1, file="final.csv")

getwd()
