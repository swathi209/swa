library(dplyr)
library(corrplot)
data=read.csv("file:///C:/Users/Swathi/Desktop/Third Sem/Analytics Capstone/Copy of GiftRecords.csv")
head(data)
str(data)
# data$Gift.Date <- as.Date(data$Gift.Date , "%m/%d/%y")
# data$First.Gift.Date<-as.Date(data$First.Gift.Date,"%m/%d/%y")
# data$Last.Gift.Date<-as.Date(data$Last.Gift.Date,"%m/%d/%y")
# data$Largest.Gift.Date<-as.Date(data$Largest.Gift.Date,"%m/%d/%y")
# str(data)
# 
# # D <- transform(d, fake_char = as.numeric(fake_char) for muliple column converting
# #                char_fac = as.numeric(char_fac))
# 
# data$Median.Household.Income=as.numeric(data$Median.Household.Income)
# data$Gift.Amount=as.numeric(data$Gift.Amount)
# data$First.Gift.Amount=as.numeric(data$First.Gift.Amount)
# data$Last.Gift.Amount=as.numeric(data$Last.Gift.Amount)
# data$Largest.Gift.Amount=as.numeric(data$Largest.Gift.Amount)
# data$Gift.Type=as.numeric(data$Gift.Type)
# data$Constituent.ID=as.numeric(data$Constituent.ID)
# 
# data$First.Name<-NULL
# data$Last.Name<-NULL
data$Preferred_State<-NULL

# cov(data,use='everything',method = 'pearson')


# data$Gift.Date <- as.numeric(data$Gift.Date)
# data$Last.Gift.Date <- as.numeric(data$Last.Gift.Date)
# data$Largest.Gift.Date<-as.numeric(data$Largest.Gift.Date)
# data$First.Gift.Date <- as.numeric(data$First.Gift.Date)

# library(ggplot2)
# library(tidyverse)
# ggplot(data,aes(y='Gift.Amount',x='Median.Household.Income'))+geom_point()+geom_smooth(method = 'lm')

# results<-kmeans(data,3)
# table(data$Median.Household.Income,results$cluster)
# plot(data[results$cluster==1,],col='red',
# xlim=c(min(data[,1]),max(data[,1])),
# ylim=c(min(data[,2]),max(data[,2])))
# points(data[results$cluster==2,],col='blue')
# points(data[results$cluster==3,],col='seagreen')

summary(data)
str(data)
library(ggplot2)
library(tidyverse)

pca<-prcomp(data)
plot(pca$x[,1],pca$x[,2])
biplot(pca)
dim(data)
pca.var<-pca$sdev^2
pca.var.per<-round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per,main='Scree Plot',xlab='Principal Component',
        ylab='Percent Variation')

ggplot(data,aes(x=pca$x[,1],y=pca$x[,2]))+geom_point()+
  xlab(paste("PC1-",pca.var.per[1],"%",sep=""))+
  ylab(paste("PC2-",pca.var.per[2],"%",sep=""))+
  ggtitle("My PCA Graph")

loading_scores<-pca$rotation[,1]
data_scores<-abs(loading_scores)
data_score_ranked<-sort(data_scores,decreasing = TRUE)
top10_data<-names(data_score_ranked[1:10])
top10_data
pca$rotation[top10_data,1] ##shows the scores and +/-sign)

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
kmm = kmeans(data,3,nstart = 50,iter.max = 15)

plot(data[c("Constituent.ID","Last.Gift.Date")],col=kmm$cluster)

