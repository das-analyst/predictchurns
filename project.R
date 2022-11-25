#our data mining objective is to find patterns from the 12,330 sessions in the dataset to 
#predict the intention of the customers to purchase the product and generate revenue 
#based on the actions they have performed in the website.
#Along with intention of the customer to generate revenue we also tend to identify the 
#target group and develop business model accordingly.

str(df)
#describing the data
#The dataset consists of 10 numerical and 8 categorical attributes
#Bounce rate: The value of "Bounce Rate" feature for a web page refers to the percentage of visitors who enter the site from that page and then leave ("bounce") without triggering any other requests to the analytics server during that session
#Exit rate:The value of "Exit Rate" feature for a specific web page is calculated as for all pageviews to the page, the percentage that were the last in the session.
#Page Value:The "Page Value" feature represents the average value for a web page that a user visited before completing an e-commerce transaction
#Special Day:The "Special Day" feature indicates the closeness of the site visiting time to a specific special day in which the sessions are more likely to be finalized with transaction
rm(list=ls())



install.packages("moments")
install.packages("corrplot")
library(moments)
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)

df <- read.csv("df.csv")

View(df)

summary(df)

df.new <- df[,-c(11,16,17,18)]
View(df.new)

boxplot(df.new)

#number of different types of pages visited by the visitor

#are the duration in minutes or seconds?

hist(df$Administrative,breaks = 30)
table(df$Administrative)
table(df$Informational)
table(df$ProductRelated)

table(df$SpecialDay,df$Month)
#may and feb are potential months that sessions are more likely to be finalized with transaction

table(df$VisitorType)
#number of visitors

table(df$VisitorType,df$Weekend) 
#probability of new visitor visiting on weekends = 0.28
#probability of old visitor visiting on weekends = 0.22

table(df$VisitorType,df$Revenue) 
#probability of new visitor purchasing = 0.24
#probability of old visitor purchasing = 0.13

table(df$VisitorType,df$Revenue,df$Weekend) #make statements here if necessary

plot <- cor(df.new)
corrplot(plot)
#exit rates and bounce rates are strongly co-related

#########################(descision tree) with 1:1

install.packages("dplyr")
library(dplyr)

false.data <- read.csv("flase_records.csv")
true.data <- read.csv("true_records.csv")

reduced.df <- rbind(false.data,true.data)
View(reduced.df)
summary(reduced.df)
table(reduced.df$revenue)

reduced.df.new <- reduced.df[,-c(7,8,9,11:15)]
#remove 15 column
View(reduced.df.new)

set.seed(1)
train.index <- sample(c(1:3800), 3800*0.5)
train.df <- reduced.df.new[train.index, ]
valid.df <- reduced.df.new[-train.index, ]
View(train.df)

#building the model
build.dt <- rpart(revenue ~ .,data=train.df,method = "class")
prp(build.dt)
?prp
library(caret)
library(e1071)

#applying training data to the built model and creating confusion matrix for accuracy
train.build <- predict(build.dt,train.df,type="class")
confusionMatrix(train.build,as.factor(train.df$revenue))

#applying validation data to the built model and creating confusion matrix for accuracy
valid.build <- predict(build.dt,valid.df,type="class")
confusionMatrix(valid.build,as.factor(valid.df$revenue))

#finding ROC andn area under curve = 0.778
library(pROC)
dt.roc <- predict(build.dt,valid.df,type = "prob")
roc.dt <- roc(valid.df$revenue,dt.roc[,1])
plot.roc(roc.dt)
auc(roc.dt)

######################applying entire old data to the built model and creating confusion matrix for accuracy
data_re_read <- read.csv("df.csv")
entire.df <- data_re_read[,-c(7,8,9,11:15)]
colnames(entire.df) <- c("administrative", "administrative_duration","informational","informational_duration","product_related","product_related_duration","special_day","visitor_type","weekend","revenue")
View(entire.df)

entire.build <- predict(build.dt,entire.df,type="class")
confusionMatrix(entire.build,as.factor(entire.df$revenue))

#finding ROC andn area under curve = 0.6763
entire.dt.roc <- predict(build.dt,entire.df,type = "prob")
entire.roc.dt <- roc(entire.df$revenue,entire.dt.roc[,1])
plot.roc(entire.roc.dt)
auc(entire.roc.dt)


#############################################################(clustering)
cluster.data <- read.csv("df.csv")
View(cluster.data)
reduced.cluster.data <- cluster.data[,-c(7:9,11:18)]
View(reduced.cluster.data)
normalize.df <- sapply(reduced.cluster.data, scale)
View(normalize.df)

set.seed(1)
km.df <- kmeans(normalize.df,4 )
km.df$cluster
km.df$centers

plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km.df$centers), max(km.df$centers)), xlim = c(0, 7))

axis(1, at = c(1:7), labels = names(reduced.cluster.data))

for (i in c(1:7))
  lines(km.df$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3, 5),"blue", "red"))

text(x = 0.2, y = km.df$centers[, 1], labels = paste("Cluster", c(1:4)))