#logit with cat variables changed
setwd("/Users/calmer/Library/CloudStorage/OneDrive-TheUniversityofTexasatDallas/Work/BA_w_R_Class/Group Project")
df=read.csv("online_shoppers_intention (1).csv")
dff <- df[,-c(7,8,9,11:15)]
dff<-subset(dff, VisitorType != "Other")
levels(dff$Revenue)<-c(0,1)#true is 1 false is 0
levels(dff$VisitorType)<-c(0,1)
table(dff$VisitorType)

#new_visitr is 1 returning is 2
dff$VisitorType <- as.numeric(as.factor(dff$VisitorType ))
dff$Weekend<-as.numeric(as.factor(dff$Weekend))
table(dff$Revenue)

set.seed(1)
train.index <- sample(c(1:12245), 12245*0.5)
train.df <- dff[train.index, ]
valid.df <- dff[-train.index, ]

View(train.df)
#build logit model
build.logit <- glm(Revenue ~ .,data = train.df,family ="binomial" )
options(scipen=999)
summary(build.logit)

#applying training data to the built model and creating confusion matrix for accuracy
logit.training <- predict(build.logit,train.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.training > 0.7, 1, 0)), as.factor(train.df$Revenue))

length(logit.training2)
logit.training2 <- predict(build.logit,dff, type = "response")
confusionMatrix(as.factor(ifelse(logit.training2 > 0.7, 1, 0)), as.factor(train.df$Revenue))


class(logit.training)
class(train.df)

#logit on balanced data
setwd("C:/Users/Vaishnavi/Downloads")
false.data <- read.csv("flase_records.csv")
true.data <- read.csv("true_records.csv")

rdf <- rbind(false.data,true.data)
rdf <- rdf[,-c(7,8,9,11:15)]
rdf<-subset(rdf, visitor_type != "Other")
levels(rdf$revenue)<-c(0,1)#true is 1 false is 0
levels(rdf$visitor_type)<-c(0,1)
table(rdf$visitor_type)

#new_visitr is 1 returning is 2
rdf$visitor_type <- as.numeric(as.factor(rdf$visitor_type ))
rdf$weekend<-as.numeric(as.factor(rdf$weekend))

table(rdf$revenue)

set.seed(1)
train.index <- sample(c(1:3800), 3800*0.5)
train.df <- rdf[train.index, ]
valid.df <- rdf[-train.index, ]

View(train.df)
#build logit model
build.logit <- glm(revenue ~ .,data = train.df,family ="binomial" )
options(scipen=999)
summary(build.logit)

#applying training data to the built model and creating confusion matrix for accuracy
logit.training <- predict(build.logit,train.df, type = "response")
confusionMatrix(as.factor(ifelse(logit.training > 0.7, 1, 0)), as.factor(train.df$revenue))

length(logit.training2)
logit.training2 <- predict(build.logit,dff, type = "response")
confusionMatrix(as.factor(ifelse(logit.training2 > 0.6, 1, 0)), as.factor(train.df$revenue))


class(logit.training)
class(train.df)

#classification without correlated columns

install.packages("dplyr")
library(dplyr)

false.data <- read.csv("flase_records.csv")
true.data <- read.csv("true_records.csv")

reduced.df <- rbind(false.data,true.data)
View(reduced.df)
summary(reduced.df)
table(reduced.df$revenue)
names(reduced.df[,c(1,3,5,7,8,9,12,14,15,16,17)])
reduced.df.new <- reduced.df[,-c(1,3,5,7,8,9,11,12,15)]
#remove 15 column
View(reduced.df.new)

set.seed(1)
dim(reduced.df.new)
train.index <- sample(c(1:3800), 3800*0.5)
train.df <- reduced.df.new[train.index, ]
valid.df <- reduced.df.new[-train.index, ]
head(train.df, 5)
View(train.df)

#building the model
build.dt <- rpart(revenue ~ .,data=train.df,method = "class")
prp(build.dt, varlen = 30)
?prp

train.build <- predict(build.dt,train.df,type="class")
confusionMatrix(train.build,as.factor(train.df$revenue))

#applying validation data to the built model and creating confusion matrix for accuracy
valid.build <- predict(build.dt,valid.df,type="class")
confusionMatrix(valid.build,as.factor(valid.df$revenue))


#without duration
reduced.df.new <- reduced.df[,-c(2,4,6,7,8,9,11,12,14)]
#remove 15 column
View(reduced.df.new)

set.seed(1)
dim(reduced.df.new)
train.index <- sample(c(1:3800), 3800*0.5)
train.df <- reduced.df.new[train.index, ]
valid.df <- reduced.df.new[-train.index, ]
head(train.df, 5)
View(train.df)

#building the model
build.dt <- rpart(revenue ~ .,data=train.df,method = "class")
prp(build.dt, varlen = 30)
?prp

train.build <- predict(build.dt,train.df,type="class")
confusionMatrix(train.build,as.factor(train.df$revenue))

#applying validation data to the built model and creating confusion matrix for accuracy
valid.build <- predict(build.dt,valid.df,type="class")
confusionMatrix(valid.build,as.factor(valid.df$revenue))

#classification with original data
df=read.csv("online_shoppers_intention (1).csv")

reduced.df.new <- df[,-c(1,3,5,7,8,9,11,15)]
#remove 15 column
View(reduced.df.new)

set.seed(1)
dim(reduced.df.new)
train.index <- sample(c(1:12330), 12330*0.5)
train.df <- reduced.df.new[train.index, ]
valid.df <- reduced.df.new[-train.index, ]
head(train.df, 5)
View(train.df)

#building the model
build.dt <- rpart(Revenue ~ .,data=train.df,method = "class")
prp(build.dt, varlen = 30)
?prp

train.build <- predict(build.dt,train.df,type="class")
confusionMatrix(train.build,as.factor(train.df$revenue))

#applying validation data to the built model and creating confusion matrix for accuracy
valid.build <- predict(build.dt,valid.df,type="class")
confusionMatrix(valid.build,as.factor(valid.df$revenue))