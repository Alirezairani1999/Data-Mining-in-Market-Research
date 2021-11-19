#NN
attach(dato)
summary(Age)
Agegroup=cut(Age,breaks=c(18,25,35,45,55,100), labels=c("18-25","26-35", "36-45","46-55","56-100"))
dato=cbind(Agegroup,dato)
Gender=as.factor(Gender)
Ostan=as.factor(Ostan)
Agegroup=as.factor(Agegroup)
Faaliat=as.factor(Faaliat)
Madrak=as.factor(Madrak)
oto=as.factor(oto)
Pc=as.factor(Pc)
mobile=as.factor(mobile)
daramad.class=as.factor(categuri)
x1=class.ind(Gender)
x2=class.ind(Ostan)
x3=class.ind(mobile)
x4=class.ind(Faaliat)
x5=class.ind(Madrak)
x6=class.ind(oto)
x7=class.ind(Pc)
x8=class.ind(daramad.class)
dums=data.frame(x1,x2,x3,x4,x5,x6,x7,x8)
colnames(dums)=c(paste("gender",c(1,2),sep=""),
              paste("Ostan",c(1,2,3,4,5),sep=""),
              paste("mobile",c(0,1),sep=""),
              paste("faaliat",c(1,2,3,4,5,6),sep=""),
              paste("madrak",c(1,4,5,6,7,8,9,10),sep=""),
              paste("car",c(0,1),sep=""),
              paste("pc",c(0,1),sep=""),
              paste("daramad.class",c(1,2,3,4),sep=""))

norm.values <- preProcess(dato[,c(2,9,40:50)], method=c("center", "scale"))
dato[,c(2,9,40:50)] <- predict(norm.values, dato[,c(2,9,40:50)])
data.nn=cbind(dums,dato[,c(2,9,40:50,57)])
train.index<-sample(c(1:dim(data.nn)[1]),dim(data.nn)[1]*0.6)
train.nn<-data.nn[train.index,]
valid.nn<-data.nn[-train.index,]

library(MASS)
library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(nnet)
library(neuralnet)
nn=neuralnet(daramad.class1+daramad.class2+daramad.class3+daramad.class4~gender1	+gender2+	Ostan1+	Ostan2+	Ostan3+	Ostan4+	Ostan5	+mobile0+	mobile1+	faaliat1+	faaliat2+	faaliat3+	faaliat4
               +faaliat5	+faaliat6	+madrak1	+madrak4	+madrak5	+madrak6+	madrak7+	madrak8+	madrak9+	madrak10+	car0	+car1+	pc0+	pc1+	Tedad+	S.Z+	H_Khorakivadokhani	+H_Noshidani	
               +H_Pushak+	H_Maskan+	H_mobleman+	H_behdasht+	H_Hamloghl	+H_Ertebatat+	H_Tafrihat+	H_Ghazayeamade	+H_kalavakhadamat
                ,data = train.nn, linear.output = F, hidden =c(3,2))
plot(nn,rep="best")
#train
training.prediction=compute(nn,train.nn[,-c(28:31,45)])
training.class1=apply(training.prediction$net.result,1,which.max)
a=as.factor(training.class1)
b=as.factor(train.nn$categuri)
confusionMatrix(a,b)
#valid
validation.prediction=compute(nn,valid.nn[,-c(28:31,45)])
validation.class1=apply(validation.prediction$net.result,1,which.max)
confusionMatrix(as.factor(validation.class1),as.factor(valid.nn$categuri))
