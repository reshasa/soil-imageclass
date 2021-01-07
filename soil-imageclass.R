#Resha Anderson
#UMGC DATA 630, Fall 2020
#Professor: Dr. Ami Gates
#Module 6, Assignment 5

#Purpose: This code will perform image classification using k-means clustering.
#The data comprises satellite imagery descriptors of soil samples.

#1. Setup
#Load cluster package
library("cluster")

#2. Set-up
#set seed to ensure reproducible results
set.seed(1234)

#Read the CSV file
setwd("C:/Users/Resha/Documents/UMGC DATA 630/Assignment 5")
soil = read.csv("SATimage.csv")

#3. Data Exploration & Preprocessing
head(soil)
table(soil$class) #No observations in class number 6
# Class Identifications 1- red soil; 2- cotton; 3- grey; 4- damp grey; 5- vegetation; 6- mix; 7- very damp grey

str(soil) #All integers, includes an id variable that needs to be dropped (class)

summary(soil) #Values are generally in the same range (0 to 255)--may not need to scale data

#Check for missing values
colSums(is.na(soil)) #Confirmed no missing values

#Create a copy of the data without class variable
mysoil<-soil
mysoil$class<-NULL

#Generate correlation matrix visualization for independent variables to look for collinearity
cor(mysoil[,1:36])

#Visualize correlation matrix
library(ggcorrplot)
corr1<-cor(mysoil[, 1:36])
ggcorrplot(corr1)+
  ggtitle("Correlation Matrix for Soil Readings")+
  theme(plot.title = element_text(hjust=0.5))

#Visualize correlation matrix for variables from same image type (i.e. Image 1)
cols <-c(1, 5, 9, 13, 17, 21, 25, 29, 33)
cor(mysoil[, cols])

corr2<-cor(mysoil[, cols])
ggcorrplot(corr2)+
  ggtitle("Correlation Matrix for Soil Readings for Image 1")+
  theme(plot.title = element_text(hjust=0.5))

#Visualize correlation matrix for all center pixel (CC1, CC2, cc3, CC4) readings

cor(mysoil[, 17:20])

corr3<-cor(mysoil[,17:20])
ggcorrplot(corr3)+
  ggtitle("Correlation Matrix for Center Pixel Soil Readings")+
  theme(plot.title = element_text(hjust=0.5))

#Visualize range and outliers for all center pixel readings
library(ggpubr)
#boxplot requires target variable to be factor
#create new dataset to allow for class to be set as factor
b<-soil
b$class <- as.factor(b$class)
#Create boxplots for the center pixel values. Use colors to match the correlation matrix
C1<-ggplot(b, aes(x=class, y=CC1)) + geom_boxplot( fill="tomato1")
C2<-ggplot(b, aes(x=class, y=CC2)) + geom_boxplot(fill="tomato3")
C3<-ggplot(b, aes(x=class, y=CC3)) + geom_boxplot(fill="steelblue1")
C4<-ggplot(b, aes(x=class, y=CC4)) + geom_boxplot(fill="steelblue3")
#plot all boxplots in the same frame with a title
fig<-ggarrange(C1, C2, C3, C4 + rremove("x.text"),
          ncol=2, nrow=2)
annotate_figure(fig, top = text_grob("Box Plots for Center Pixel Readings", face = "bold", size = 14))

#Plot bar chart of class variable
ggplot(soil, aes(x=factor(class))) + 
  geom_bar(stat="count", fill="steelblue") +
  labs(title="Frequency by Soil Class", x="Class", y="Count")

#reset plot parameters
par(mfrow=c(1,1))


#4. Running the Method
#NOTE: R may appear to be running for an extended period of time when generating plots. 
  # If the stop icon is still visible after the plot is generated in the plot window,
  # click on the finish button (top right) of the plots window.

#First Iteration: All Independent Variables

#Plot the Between Sum of Squares and Total Within Sum of Squares to identify optimal k value
#Use k = 3 through 10
twss<- integer(length(2:10))
for (i in 2:10) twss[i] <- kmeans(mysoil, centers=i)$tot.withinss
bss <- integer(length(2:10))
for (i in 2:10) bss[i] <- kmeans(mysoil, centers=i)$betweenss
plot(1:10, bss, type="b", xlab="Number of Clusters", ylab="Sum of Squares", col="steelblue",
     main="Total Within Sum of Squares & Between Sum of Squares for Soil Data (k=2 through 10)")
lines(1:10, twss, type="b", pch=2, col="tomato")
legend(x=8.5, y=30000000, c("Between SS", "Total w/in SS"), col=c("steelblue", "tomato"), pch=c(1,2))

#Run the k-means clustering method using all of the independent variables with different values of k
#k=3
#Store result in variable kc3, use k=3
kc3<-kmeans(mysoil, 3)
#output the result
print(kc3)
kc3$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc3$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc3$tot.withinss
kc3$totss
kc3$betweenss
kc3$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, k=3"
clusplot(mysoil, kc3$cluster, color=TRUE, shade=TRUE, col.p=kc3$cluster, labels=5, lines=0, main=title)

#Compare result to k-means on scaled data
scalesoil<-mysoil
scalesoil[1:36]<-scale(scalesoil[1:36])
scalekc3<-kmeans(scalesoil, 3)
print(scalekc3)
scalekc3$iter
table(soil$class, scalekc3$cluster, dnn= c("Actual Class", "Assigned Cluster"))


#k=4
#Store result in variable kc4, use k=4
kc4<-kmeans(mysoil, 4)
#output the result
print(kc4)
kc4$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc4$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc4$tot.withinss
kc4$totss
kc4$betweenss
kc4$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
  title <- "K-Means Clusters for Soil Data, k=4"
clusplot(mysoil, kc4$cluster, color=TRUE, shade=TRUE, col.p=kc4$cluster, labels=5, lines=0, main=title)

#k=5
#Store result in variable kc5, use k=5
kc5<-kmeans(mysoil, 5)
#output the result
print(kc5)
kc5$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc5$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc5$tot.withinss
kc5$totss
kc5$betweenss
kc5$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, k=5"
clusplot(mysoil, kc5$cluster, color=TRUE, shade=TRUE, col.p = kc5$cluster, labels=5, lines=0, main=title)

#k=6
#Store result in variable kc6, use k=6
kc6<-kmeans(mysoil, 6)
#output the result
print(kc6)
kc6$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc6$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc6$tot.withinss
kc6$totss
kc6$betweenss
kc6$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters, color observations by actual class
title <- "K-Means Clusters for Soil Data, k=6"
clusplot(mysoil, kc6$cluster, color=TRUE, shade=TRUE, col.p = soil$class, labels=5, lines=0, main=title)
text(5, -10, "Colors = Actual Class")
text(5, -11.5, "Shapes = Assigned Cluster")


#k=7
#Store result in variable kc7, use k=7
kc7 <- kmeans(mysoil, 7)
#output the result
print(kc7)
kc7$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc7$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc7$tot.withinss
kc7$totss
kc7$betweenss
kc7$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, k=7"
clusplot(mysoil, kc7$cluster, color=TRUE, shade=TRUE, col.p = soil$class, labels=5, lines=0, main=title)
text(5, -10, "Colors = Actual Class")
text(5, -11.5, "Shapes = Assigned Cluster")


#Try k=9 based on the bar chart of class frequencies--classes 1, 4, and 7 had larger frequencies than the others
  #perhaps they could be split into different classes

#k=9
#Store result in variable kc9, use k=9
kc9<-kmeans(mysoil, 9)
#output the result
print(kc9)
kc9$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc9$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc9$tot.withinss
kc9$totss
kc9$betweenss
kc9$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, k=9"
clusplot(mysoil, kc9$cluster, color=TRUE, shade=TRUE, col.p = kc9$cluster, labels=5, lines=0, main=title)

###
#Second Iteration: Using only the center pixel (CC1, 2, 3, 4)
###
#How many iterations should be run? Plot Between Sum of Squares and Total Within Sum of Squares.
#Use k = 2 through 10
twssb<- integer(length(2:10))
for (i in 2:10) twssb[i] <- kmeans(mysoil[, 17:20], centers=i)$tot.withinss
bssb <- integer(length(2:10))
for (i in 2:10) bssb[i] <- kmeans(mysoil[, 17:20], centers=i)$betweenss
plot(1:10, bssb, type="b", xlab="Number of Clusters", ylab="Sum of Squares", col="steelblue",
     main="Total Within & Between Sum of Squares for Soil Data, Center Pixel Only (k=2 through 10)")
lines(1:10, twssb, type="b", pch=2, col="tomato")
legend(x=8.5, y=3000000, c("Between SS", "Total w/in SS"), col=c("steelblue", "tomato"), pch=c(1,2))


#k=3
#Store result in variable kc3b, use k=3
kc3b <- kmeans(mysoil[, 17:20], 3)
#output the result
print(kc3b)
kc3b$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc3b$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc3b$tot.withinss
kc3b$totss
kc3b$betweenss
kc3b$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=3"
clusplot(mysoil, kc3b$cluster, color=TRUE, shade=TRUE, col.p = kc3b$cluster, labels=5, lines=0, main=title)

#k=4
#Store result in variable kc4b, use k=4
kc4b <- kmeans(mysoil[, 17:20], 4)
#output the result
print(kc4b)
kc4b$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc4b$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc4b$tot.withinss
kc4b$totss
kc4b$betweenss
kc4b$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=4"
clusplot(mysoil, kc4b$cluster, color=TRUE, shade=TRUE, col.p = kc4b$cluster, labels=5, lines=0, main=title)

#k=5
#Store result in variable kc5b, use k=5
kc5b <- kmeans(mysoil[, 17:20], 5)
#output the result
print(kc5b)
kc5b$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc5b$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc5b$tot.withinss
kc5b$totss
kc5b$betweenss
kc5b$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=5"
clusplot(mysoil, kc5b$cluster, color=TRUE, shade=TRUE, col.p = kc5b$cluster, labels=5, lines=0, main=title)

#k=6
#Store result in variable kc6b, use k=6
kc6b <- kmeans(mysoil[, 17:20], 6)
#output the result
print(kc6b)
kc6b$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc6b$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc6b$tot.withinss
kc6b$totss
kc6b$betweenss
kc6b$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=6"
clusplot(mysoil, kc6b$cluster, color=TRUE, shade=TRUE, col.p = kc6b$cluster, labels=5, lines=0, main=title)

#Build the cluster plot with color by class assignment and shape by cluster assignment
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=6"
clusplot(mysoil, kc6b$cluster, color=TRUE, shade=FALSE, col.p = soil$class, labels=5, lines=0, main=title)
text(5, -10, "Colors = Actual Class")
text(5, -11.5, "Shapes = Assigned Cluster")

#k=7
#Store result in variable kc7b, use k=7
kc7b <- kmeans(mysoil[, 17:20], 7)
#output the result
print(kc7b)
kc7b$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc7b$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc7b$tot.withinss
kc7b$totss
kc7b$betweenss
kc7b$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=7"
clusplot(mysoil, kc7b$cluster, color=TRUE, shade=TRUE, col.p = kc7b$cluster, labels=5, lines=0, main=title)

#k=9
#Store result in variable kc9b, use k=9
kc9b <- kmeans(mysoil[, 17:20], 9)
#output the result
print(kc9b)
kc9b$iter

#Clustering evaluation
#cluster to class evaluation
table(soil$class, kc9b$cluster, dnn = c("Actual Class", "Assigned Cluster"))

kc9b$tot.withinss
kc9b$totss
kc9b$betweenss
kc9b$withinss

#Build the cluster plot
#Suppress observation number labels, label clusters
title <- "K-Means Clusters for Soil Data, Center Pixels Only, k=9"
clusplot(mysoil, kc9b$cluster, color=TRUE, shade=TRUE, col.p = kc9b$cluster, labels=5, lines=0, main=title)


#Method Evaluation Visuals
#First Iteration

#Plot betweenss/totalss 
ssratio1<- integer(length(2:10))
for (i in 2:10) ssratio1[i] <- 100*((kmeans(mysoil, centers=i)$betweenss)/(kmeans(mysoil, centers=i)$totss))
plot(1:10, ssratio1, type="b", xlab="Number of Clusters", ylab="Percent", col="steelblue",
     main="Between Sum of Squares/Total Sum of Squares for Soil Data, All Variables (k=2 through 10)")
ssratio1

#Plot change in betweenss/totalss 
ssratio2<- integer(length(2:10))
for (i in 2:10) ssratio2[i] <- (((kmeans(mysoil, centers=i)$betweenss)/(kmeans(mysoil, centers=i)$totss))
                                - ((kmeans(mysoil, centers=(i-1))$betweenss)/(kmeans(mysoil, centers=(i-1))$totss)))*100
plot(1:10, ssratio2, type="b", xlab="Number of Clusters", ylab="Percent", col="steelblue",
     main="Change in Between Sum of Squares/Total Sum of Squares for Soil Data, All Variables (k=2 through 10)")

ssratio2

#Second Iteration, with only Center Pixel Data
#Plot betweenss/totalss 
ssratio<- integer(length(2:10))
for (i in 2:10) ssratio[i] <- 100*((kmeans(mysoil[, 17:20], centers=i)$betweenss)/(kmeans(mysoil[, 17:20], centers=i)$totss))
plot(1:10, ssratio, type="b", xlab="Number of Clusters", ylab="Percent", col="steelblue",
     main="Between Sum of Squares/Total Sum of Squares for Soil Data, Center Pixel Only (k=2 through 10)")


#Plot change in betweenss/totalss 
ssratiob<- integer(length(2:10))
for (i in 2:10) ssratiob[i] <- (((kmeans(mysoil[, 17:20], centers=i)$betweenss)/(kmeans(mysoil[, 17:20], centers=i)$totss))
                                - ((kmeans(mysoil[, 17:20], centers=(i-1))$betweenss)/(kmeans(mysoil[,17:20], centers=(i-1))$totss)))*100
plot(1:10, ssratiob, type="b", xlab="Number of Clusters", ylab="Percent", col="steelblue",
     main="Change in Between Sum of Squares/Total Sum of Squares for Soil Data, Center Pixel Only (k=2 through 10)")
