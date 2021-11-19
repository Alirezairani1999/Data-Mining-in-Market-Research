
data.df=data_frame

######################tree
library(rpart)
library(rpart.plot)
library(caret)
library(lattice)
library(ggplot2)
library(e1071)
class.tree <- rpart( categuri~ ., data = train, method = "class",model=TRUE)
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10,
    cex=.9,box.palette=c("mediumvioletred"))

class.tree.t <- predict(class.tree,train,type = "class")
confusionMatrix(as.factor(class.tree.t),as.factor(train$categuri))

class.tree.v <- predict(class.tree,valid,type = "class")
confusionMatrix(as.factor(class.tree.v),as.factor(valid$categuri))

##deeper tree
#train
deeper.ct <- rpart(categuri ~ ., data = train, method = "class", cp = 0, minsplit = 1)
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))
class.tree.p <- predict(deeper.ct,train,type = "class")
confusionMatrix(as.factor(class.tree.p),as.factor(train$categuri))
deeper.ct <- rpart(categuri ~ ., data = train, method = "class", cp = 0, minsplit = 1)
##deeper tree
#validation
deeper.ct.v <- rpart(categuri ~ ., data = valid, method = "class", cp = 0, minsplit = 1)
class.tree.v <- predict(deeper.ct.v,valid.tree,type = "class")
confusionMatrix(as.factor(class.tree.v),as.factor(valid.tree$categuri))

deeper.ct <- rpart(categuri ~ ., data = train.tree, method = "class", cp = 0, minsplit = 1)


    ######################################################################
    deeper.tree<- rpart(categuri ~ ., data = train, method = "class",cp= .00001,minsplit =5)
    options(digits=8)
    printcp(deeper.tree)
    which.min(deeper.tree$cptable[,"xerror"])
######################################################################
library(rpart)
cv.tree<-rpart(categuri~.,data = train ,method = "class", 
               cp =0.004040404, minsplit =5,xval =5, model=T)         
prp(cv.tree,type =0,extra = 1,under = T,split.font = 2,
    varlen = -10,cex=.7,box.palette=c("mediumvioletred"))

cv.tree.p <- predict(cv.tree,train,type = "class")
confusionMatrix(as.factor(cv.tree.p),as.factor(train$categuri))

cv.tree.p.v <- predict(cv.tree,valid,type = "class")
confusionMatrix(as.factor(cv.tree.p.v),as.factor(valid$categuri))


###########################################
