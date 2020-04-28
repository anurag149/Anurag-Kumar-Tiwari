# Loading Universities wine
wine<-read.csv("E:\\Bokey\\Excelr wine\\R Codes\\PCA\\Universities.csv") ## use read.csv for csv files
View(wine)

help(princomp) ## to understand the api for princomp

attach(wine)
cor(wine)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(wine, cor = TRUE) not_same_as prcomp(wine, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole wine

# cbind used to bind the wine in column wise
# Considering top 3 principal component scores and binding them with wine
wine<-cbind(wine,pcaObj$scores[,1:3])
View(wine)

# preparing wine for clustering (considering only pca scores as they represent the entire wine)
clus_wine<-wine[,8:10]

# Normalizing the wine 
norm_clus<-scale(clus_wine) # Scale function is used to normalize wine
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the wine using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,wine) # binding column wise with orginal wine
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities wine on membership_1

write.csv(final1,file="universities_clustered.csv",row.names = F,col.names = F)
getwd()
