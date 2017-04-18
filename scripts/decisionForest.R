setwd("/Users/artur/rnd/git/c3po-cr")
library(readr)
library(rpart)
library(caret)
library(randomForest)
library(randomForestSRC)
library(reprtree)


mydata=read.csv("data/mydata_valid.csv",header=TRUE)
#mydata <- mydata[, colSums(is.na(mydata)) == 0]
#write.csv(mydata, file = "~/rnd/git/c3po-cr/data/mydata.csv")

#modfit <- train(resolveTo~.,method="rpart",data=mydata) 


train.flag <- createDataPartition(y=mydata$resolveTo,p=0.5,list=FALSE)
training <- mydata[train.flag,]
validation <- mydata[-train.flag,]

modfit <- randomForest(resolveTo ~ ., data=training)

train.cart<-predict(modfit,newdata=training)
table(train.cart,training$resolveTo)
pred.cart<-predict(modfit,newdata=validation)
table(pred.cart,validation$resolveTo)
#fit<-rpart(resolveTo ~ ., data=training, method="class")





#mydata <- read_csv("~/rnd/git/c3po-cr/data/mydata_valid.csv", na = "NA")



#mydata <- Filter(function(x) !(all(x=="")), mydata)
#write.csv(mydata, file = "~/rnd/git/c3po-cr/data/mydata.csv", row.names = FALSE)
#mydata=read.csv("data/data.csv",header=TRUE)
#mydata <- na.pass(mydata)

train.flag <- createDataPartition(y=mydata$resolveTo,p=0.5,list=FALSE)
training <- mydata[train.flag,]
validation <- mydata[-train.flag,]

training <- training[, colSums(is.na(training)) == 0]
modfit <- train(resolveTo~.,method="rpart",data=training) 

mod.rf <- randomForest(resolveTo ~ ., data=Validation)







data(iris)
summary(iris)
qplot(Petal.Length,Petal.Width,colour=Species,data=iris)
library(rpart)
library(caret)
train.flag <- createDataPartition(y=iris$Species,p=0.95,list=FALSE)
training <- iris[train.flag,]
Validation <- iris[-train.flag,]
modfit <- train(Species~.,method="rpart",data=training) 
library(rattle)
fancyRpartPlot(modfit$finalModel)
train.cart<-predict(modfit,newdata=training)
table(train.cart,training$Species)
pred.cart<-predict(modfit,newdata=Validation)
table(pred.cart,Validation$Species)
correct <- pred.cart == Validation$Species
qplot(Petal.Length,Petal.Width,colour=correct,data=Validation)



library(randomForest)
library(randomForestSRC)
library(reprtree)

modfit <- train(Species~ .,method="rf",data=training)
fancyRpartPlot(modfit$finalModel)
pred <- predict(modfit,training)
train.cart<-predict(modfit,newdata=training)
table(train.cart,training$Species)
pred.cart<-predict(modfit,newdata=Validation)
table(pred.cart,Validation$Species)














#**************************
#return the rules of a tree
#**************************
getConds<-function(tree){
  #store all conditions into a list
  conds<-list()
  #start by the terminal nodes and find previous conditions
  id.leafs<-which(tree$status==-1)
  j<-0
  for(i in id.leafs){
    j<-j+1
    prevConds<-prevCond(tree,i)
    conds[[j]]<-prevConds$cond
    while(prevConds$id>1){
      prevConds<-prevCond(tree,prevConds$id)
      conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
      if(prevConds$id==1){
        conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
        break()
      }
    }
    
  }
  
  return(conds)
}

#**************************
#find the previous conditions in the tree
#**************************
prevCond<-function(tree,i){
  if(i %in% tree$right_daughter){
    id<-which(tree$right_daughter==i)
    cond<-paste(tree$split_var[id],">",tree$split_point[id])
  }
  if(i %in% tree$left_daughter){
    id<-which(tree$left_daughter==i)
    cond<-paste(tree$split_var[id],"<",tree$split_point[id])
  }
  
  return(list(cond=cond,id=id))
}

#remove spaces in a word
collapse<-function(x){
  x<-sub(" ","_",x)
  
  return(x)
}


data(iris)
require(randomForest)
mod.rf <- randomForest(Species ~ ., data=iris)
tree<-getTree(mod.rf, k=1, labelVar=TRUE)
#rename the name of the column
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
print(rules)