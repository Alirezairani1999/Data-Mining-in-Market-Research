#knn
new.data=final
new.data[,1:27] <- lapply(new.data[,1:27] , factor)
View(new.data)
attach(new.data)
set.seed(1234)
train.row=sample(rownames(new.data),dim(new.data)[1]*0.5)
valid.row=sample(setdiff(rownames(new.data),train.row),dim(new.data)[1]*0.35)
test.row=setdiff(rownames(new.data),union(train.row,valid.row))
train.data=new.data[train.row,]
valid.data=new.data[valid.row,]
test.data=new.data[test.row,]

train.norm.df <- train.data
valid.norm.df <- valid.data
mower.norm.df <- test.data
# use preProcess() from the caret package to normalize Income and Lot_Size.
library(caret)
library(FNN)
norm.values <- preProcess(train.data[, 28:39], method=c("center", "scale"))
train.norm.df[, 28:39] <- predict(norm.values, train.data[, 28:39])
valid.norm.df[, 28:39] <- predict(norm.values, valid.data[, 28:39])
mower.norm.df[, 28:39] <- predict(norm.values, test.data[, 28:39])

class(new.data$Ostan)
#########best k
cc=as.factor(train.norm.df$categuri)
c=as.factor(valid.norm.df$categuri)
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:39], valid.norm.df[, 1:39],
                  cl =cc, k = i,prob = TRUE)
  
  
  accuracy.df[i, 2] <- confusionMatrix(knn.pred,c)$overall[1]
}
########validation
class = as.factor(train.norm.df$categuri)
cl=as.factor(valid.norm.df$categuri)
View(train.norm.df)
nn <- knn(train = train.norm.df[, 1:39], test = valid.norm.df[1:39], class, k =7 )
confusionMatrix(nn,cl)
row.names(train.data)[attr(nn, "nn.index")]
###########test
nn.t <- knn(train = train.norm.df[, 1:39], test = mower.norm.df[1:39], class, k = 7)
t=as.factor(mower.norm.df$categuri)
confusionMatrix(nn.t,t)
?knn

data(iris3)
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)
