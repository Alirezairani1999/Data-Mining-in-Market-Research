#reg
attach(Data)
dato$Ostan=factor(Ostan,levels=c(1,2,3,4,5),labels=c("Tehran","Esfehan","Mashhad","Shiraz","Tabriz"))
dato$Gender=factor(Gender,levels=c(1,2),labels=c("Male","Female"))
dato$Savad=factor(Savad,levels=c(1,2),labels=c("Basavad","Bisavad"))
dato$InEdu=factor(InEdu,levels=c(1,2),labels=c("yes","No"))
dato$Madrak=factor(Madrak,levels=c(1,4,5,6,7,8,9),labels=c("zirdiplom","Diplom","Kardani","Karshenasi","Arshad","Doctora","sayer"))
dato$Faaliat=factor(Faaliat,levels=c(1,2,3,4,5,6),labels=c("Shaghel","Bikar","Bikarbadaramad","mohasel","khanedar","sayer"))
dato$N.S=factor(N.S,levels=c(1,2,3),labels=c("Felezi","Betoni","Sayer"))
dato$Masleh=factor(Masleh,levels=c(1,2,3,4,5,6,7),labels=c("Ajor&Ahan&sang","Ajor&Chob&sang","blocksimani","Ajor","Chob","khesht","sayer"))
dato$sookht=factor(sookht,levels=c(3,4),labels=c("gazmaye","gaztabii"))
dato$oto=factor(oto,levels=c(0,1),labels=c("No","Yes"))
dato$motor=factor(motor,levels=c(0,1),labels=c("No","Yes"))
dato$do=factor(do,levels=c(0,1),labels=c("No","Yes"))
dato$radio=factor(radio,levels=c(0,1),labels=c("No","Yes"))
dato$zabt=factor(zabt,levels=c(0,1),labels=c("No","Yes"))
dato$TV=factor(TV,levels=c(0,1),labels=c("No","Yes"))
dato$DVD=factor(DVD,levels=c(0,1),labels=c("No","Yes"))
dato$Pc=factor(Pc,levels=c(0,1),labels=c("No","Yes"))
dato$mobile=factor(mobile,levels=c(0,1),labels=c("No","Yes"))
dato$yakhchal.f=factor(yakhchal.f,levels=c(0,1),labels=c("No","Yes"))
dato$gaz=factor(gaz,levels=c(0,1),labels=c("No","Yes"))
dato$jaro.b=factor(jaro.b,levels=c(0,1),labels=c("No","Yes"))
dato$m.lebas=factor(m.lebas,levels=c(0,1),labels=c("No","Yes"))
dato$charkh.kh=factor(charkh.kh,levels=c(0,1),labels=c("No","Yes"))
dato$panke=factor(panke,levels=c(0,1),labels=c("No","Yes"))
Data$cooler=factor(cooler,levels=c(0,1),labels=c("No","Yes"))
Data$m.zarf=factor(m.zarf,levels=c(0,1),labels=c("No","Yes"))
Data$microfer=factor(microfer,levels=c(0,1),labels=c("No","Yes"))
Data$bargh=factor(bargh,levels=c(0,1),labels=c("No","Yes"))
Data$tel=factor(tel,levels=c(0,1),labels=c("No","Yes"))
Data$internet=factor(internet,levels=c(0,1),labels=c("No","Yes"))
Data$hamam=factor(hamam,levels=c(0,1),labels=c("No","Yes"))
Data$hararat.m=factor(hararat.m,levels=c(0,1),labels=c("No","Yes"))
Data$package=factor(package,levels=c(0,1),labels=c("No","Yes"))
Data$fazelab=factor(fazelab,levels=c(0,1),labels=c("No","Yes"))
Data$T.M.S=factor(T.M.S,levels=c(1,3,4,5,6,7),labels=c("Malek","ejari","Rahn","Khedmat","Free","sayer"))
Data$categuri=as.factor(Data$categuri)
dato$internet=as.factor(dato$internet)
dato$m.lebas=as.factor(dato$m.lebas)

data.df=Data
library(ordinal)
library(MASS)
library("MASS")

library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(rpart)
lib
daramad.class=factor(dato$categuri,ordered=T)
data.df=knn[,37:57]
View(data.df)
attach(dato)
Gender=as.factor(Gender)
Ostan=as.factor(Ostan)
Faaliat=as.factor(Faaliat)
Madrak=as.factor(Madrak)
oto=as.factor(oto)
internet=as.factor(internet)
m.lebas=as.factor(m.lebas)
Pc=as.factor(Pc)
motor=as.factor(motor)

model=lm(log(daramad)~Gender+Ostan+Faaliat+Madrak+oto+Tedad+T.O+H_Maskan+H_Hamloghl+H_kalavakhadamat+
H_Tafrihat+H_Hamloghl+H_Pushak+internet+m.lebas+motor+Pc+H_behdasht,data=Data)
summary(model)
library(ordinal)
categurii=as.factor(categuri)
train.index<-sample(c(1:dim(Data)[1]),dim(Data)[1]*0.6)
train.reg<-Data[train.index,]
valid.reg<-Data[-train.index,]
attach(train.reg)
cat=as.factor(train.reg$categuri)
model=polr(cat~Gender+Ostan+Faaliat+Madrak+oto+Tedad+T.O+H_Maskan+H_Hamloghl+H_kalavakhadamat+
H_Tafrihat+H_Hamloghl+internet+motor+Pc+H_behdasht,data=train.reg,na.action=na.omit,Hess=TRUE,method="logistic")
pred<-predict(model,train.reg,type="class")
confusionMatrix(pred,cat)


###valid
attach(valid.reg)
cat=as.factor(valid.reg$categuri)
model=polr(cat~Gender+Ostan+Faaliat+Madrak+oto+Tedad+T.O+H_Maskan+H_Hamloghl+H_kalavakhadamat+
H_Tafrihat+H_Hamloghl+internet+motor+Pc+H_behdasht,data=valid.reg,na.action=na.omit,Hess=TRUE,method="logistic")
pred<-predict(model,valid.reg,type="class")
confusionMatrix(pred,cat)


attach(data.df)
View(data.df)
d=data.df[,-c(6,7)]
model=glm(categuri~.,data=d,family=binomial)
summary(model)
per=model$fitted.values

#############################################################################
library(readxl)
data<-read_excel("C:/Users/Administrator/Desktop/data.xlsx")
data[,c(1,3,5,6,7,8,9,12:36,42)] <- lapply(data[,c(1,3,5,6,7,8,9,12:36,42)] , factor)
attach(data)
set.seed(1234)
train.row=sample(rownames(data),dim(data)[1]*0.5)
valid.row=sample(setdiff(rownames(data),train.row),dim(data)[1]*0.35)
test.row=setdiff(rownames(data),union(train.row,valid.row))
train.data=data[train.row,]
valid.data=data[valid.row,]
test.data=data[test.row,]

