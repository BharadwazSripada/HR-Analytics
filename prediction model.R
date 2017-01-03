#predictive modelling using decission tree

hrana <- read.csv("C:\\Users\\Hi\\Desktop\\Titanic\\hr analytics\\HR_comma_sep.csv",header = T,stringsAsFactors = T)
str(hrana)
hrana$left <- as.factor(hrana$left)
hrana$Work_accident <- factor(hrana$Work_accident)
hrana$number_project <- factor(hrana$number_project)
hrana$time_spend_company <- factor(hrana$time_spend_company)

#data partition
ind <- createDataPartition(hrana$left,times = 1,p=0.7,list = F)
train <- hrana[ind,]
test <- hrana[-ind,]

#Decision tree

model <- rpart(left~.,data = train,method = "class")
pred <- predict(model,test,type = "class")

conf1 <-table(test$left,pred)
acc1<-sum(diag(conf1))/sum(conf1)

test1 <- predict(model,test,type = "prob")
treepred = prediction(test1[,2], test$left)
treeperf = performance(treepred,"tpr","fpr")
plot(treeperf,main = "ROC", colorize=T)

#randomforest

m2 <- randomForest(left~.,data = train,mtry = 4,ntree=500,importance = T)
pred <- predict(m2,test)
conf2 <-table(test$left,pred)
acc2<-sum(diag(conf2))/sum(conf2)
test.forest = predict(m2,test,type = "prob")
forestpred = prediction(test.forest[,2], test$left)
forestperf = performance(forestpred,"tpr","fpr")
plot(forestperf,main = "ROC", colorize=T)


#Bagging

library(ipred)
train_bag = bagging(left ~ ., data=train, coob=T)
train_bag

pred_bag <- predict(train_bag,test)
conf3 <- table(pred_bag,test$left)
acc3 <- sum(diag(conf3))/sum(conf3)

#Prepare bagged model for ROC curve

test.bagprob = predict(train_bag, type = "prob", newdata = test)
bagpred = prediction(test.bagprob[,2], test$left)
bagperf = performance(bagpred, "tpr" , "fpr")

#comparison of decission tree,bagging and randomnforest
plot(treeperf,col = 1,ad = T)
plot(bagperf, col=2, add=TRUE)
plot(forestperf, col=3, add=TRUE)
legend(0.6, 0.6, c("decission tree 97.26","bagging 98.77", "rforest 99.22"), 1:3)
