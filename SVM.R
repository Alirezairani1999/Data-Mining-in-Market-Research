data.df=Data
data.df[,c(1,3,5,6,7,8,9,10,12:42)] <- lapply(data.df[,c(1,3,5,6,7,8,9,10,12:42)] , factor)
train.index<-sample(c(1:dim(data.df)[1]),dim(data.df)[1]*0.6)
train<-data.df[train.index,]
valid<-data.df[-train.index,]
train.svm=train[,-c(54:59,6,7,13,1,3,5,6,7,8,9,10,12:42)]
valid.svm=valid[,-c(54:59,6,7,13,1,3,5,6,7,8,9,10,12:42)]
library(e1071)
library(caret)
attach(data.df)
View(train.svm)
dim(train.svm)
model=svm(categuri~.,data = train,type="C-classification")
summary(model)
View(train)
View(train.svm)
x=train[,-37]
pred <- predict(model,x )
length(pred)
cate=as.factor(train$categuri)
length(cate)
confusionMatrix(pred,cate)
categuri=as.factor(categuri)
#valid
model.v=svm(categuri~.,data = valid.svm,type="C-classification")
summary(model)
pred <- predict(model.v, x1)
confusionMatrix(pred,categ)
categ=as.factor(valid.svm$categuri)
x1=valid.svm[,-15]
pred <- predict(model.v,x )
cate=as.factor(train.svm$categuri)
confusionMatrix(pred,cate)
categuri=as.factor(categuri)