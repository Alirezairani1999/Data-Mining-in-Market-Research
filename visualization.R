#data

library(readxl)
Data <- read_excel("C:/Users/Administrator/Desktop/Data mining final project/Data.xlsx")
data.df=orginal
data.df=data.frame(Data)
attach(data.df)
Agegroup=cut(Age,breaks=c(18,25,35,45,55,91), labels=c("18-25","26-35", "36-45","46-55","56-91"))
data.df=cbind(Agegroup,data.df)
View(data.df)
dpermonth=rowSums(data.df[,65:68])
Hazine=rowSums(data.df[,54:64])
data.df=cbind(data.df,dpermonth,Hazine)
q=quantile(dpermonth,.7)
response=ifelse(dpermonth>q,1,0)
data.df=cbind(data.df,response)
View(data.df)
data.df=data.df[,-c(2,3)]
names(data.df)
attach(data.df)
library(ggplot2)
View(data.df)
a<-ggplot(data.df, aes(x =oto))
a + geom_bar()


pcs <- prcomp(data.df,scale. = T)
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores,10)


gender=as.factor(Gender)
a<-ggplot(data.df, aes(x =log(dpermonth)))
a + geom_bar(stat="bin")
a + geom_histogram(aes(color = gender, fill = gender),
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))

#age

hist(Age)
plot(density(Age), col="brown1")
summary(Age)
Agegroup=cut(Age,breaks=c(18,25,35,45,55,91), labels=c("18-25","26-35", "36-45","46-55","56-91"))
table(Agegroup)
summary(Agegroup)
plot(Agegroup)
library(ggplot2)
data.plot=data.frame(table(Agegroup))

library(RColorBrewer)
p <- ggplot(data=data.df, aes(x=Agegroup), fill=brown1)
p + geom_bar(col="black",width=.5, fill="cyan3")

##data.plot=data.frame(table(Agegroup))
#gender
x=table(Gender)
library(expss)
val_lab(gender) = num_lab("
          2 woman
          1 man
")

gender=as.character(Gender)
library(ggplot2)
use_labels(mtcars, {
  p <- ggplot(data=data.df, aes(x=gender), fill=brown1)
  p + geom_bar(col="black",width=.4, fill="coral1")
})

man=Gender==1
p <- ggplot(data=data.df, aes(x=Agegroup, fill=man))
p + geom_bar(col="red") 
#tedad
table(Tedad.a)
number_of_family=as.character(Tedad.a)
as.factor(Tedad.a)
p <- ggplot(data=data.df, aes(x=number_of_family), fill=brown1)
p + geom_bar(col="black",width=.6, fill="cyan3")
#oto
p <- ggplot(data=data.df, aes(x=oto), fill=brown1)

p + geom_bar(col="black",width=.5, fill="coral1")
table(oto)
#savad
table(Savad)
val_lab(Savad) = num_lab("
          2 No
          1 Yes 
")
Savad=as.character(Savad)
class(Savad)
table(Savad)
use_labels(mtcars, {
  p <- ggplot(data=data.df, aes(x=Savad), fill=brown1)
  
  p + geom_bar(col="black",width=.5, fill="cyan3")
})

#inedu
table(InEdu)
education=as.character(InEdu)
p <- ggplot(data=data.df, aes(x=InEdu), fill=brown1)
p + geom_bar(col="black",width=.5, fill="cyan3")

#madrak

table(n.t.m)
table(Madrak)
library(expss)
is.na.data.frame(data.df)
Madrak.factor=as.factor(Madrak)
na.omit(Madrak.factor)
table (Faaliat)
table(Madrak)
p <- ggplot(data=data.df, aes(x=Madrak), fill=brown1)
p + geom_bar(col="black",width=.6, fill="cyan3") + scale_x_discrete(limits = c("ebtedai","rahnmai","motevasete",
                                                                               "diplom","foghdiplom","lisans","arshad", "doktora","bisavad"))




#faaliat
table(Faaliat)
Faaliat=as.factor(Faaliat)
p <- ggplot(data=data.df, aes(x=Faaliat), fill=brown1)

p + geom_bar(col="black",width=.5, fill="cyan3")+scale_x_discrete(limits = c("shaghel","bikar(joya)","badpermonth bikar","mohasel","khanedar","sayer" ))

library(ggplot2)
ss <- ggplot(data=data.df, aes(x=Faaliat, fill=brown1))
ss + geom_bar(col="black",width=.6, fill="cyan3") + scale_x_discrete(limits = c("shaghel","bikar(joya)","badpermonth bikar","sayer" ))

table(Faaliat)
View(data.df)
library(forcats)
library(dplyr)
replace(Faaliat, list=c(2,5,6), values=c(4,4,4))
Faaliat
table(Faaliat)
library(plotrix)

lbls=c(x1,x2,x3,x4,x5)
df=data.frame(slice,lbls)
library(ggplot2)
bp<- ggplot(df, aes(x="", y=slice, fill=lbls))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)
pie
#t.shaghel
table(T.shaghel)
p <- ggplot(data=data.df, aes(x=T.shaghel), fill=brown1)

p + geom_bar(col="black",width=.5, fill="cyan3")+scale_x_discrete(limits = c("1","2","3","4" ))
#T.O
table(T.O)
p <- ggplot(data=data.df, aes(x=T.O), fill=brown1)

p + geom_bar(col="black",width=.5, fill="cyan3")+scale_x_discrete(limits = c("2","3","4","5","6","8"))

#N.S
table(N.S)
p <- ggplot(data=data.df, aes(x=N.S), fill=brown1)

p + geom_bar(col="black",width=.5, fill="coral1")+scale_x_discrete(limits = c("felezi","botoni","sayer"))
#masaleh
table(Masleh)
p <- ggplot(data=data.df, aes(x=Masleh), fill=brown1)

p + geom_bar(col="black",width=.5, fill="cyan3")+scale_x_discrete(limits = c("Ahan","Ajor","siman","kheshti"))


#ntm
table(n.t.m)

p + geom_bar(col="black",width=.6, fill="coral1") + scale_x_discrete(limits = c("malek kol","malek","ejari","rahn","khedmat","free"))

boxplot( Age~ cat , ylab = "age", xlab = "dpermonth", )
group=as.factor(cat)
p1 <- ggplot(data.df, aes(x=group,y=Age,color=group))
p1 + geom_boxplot(outlier.size=0)
savad=as.factor(savadd)
p2 <- ggplot(data.df, aes(x=savad,y=dpermonth))
p2 + geom_boxplot(outlier.size=0)

p3 <- ggplot(data.df, aes(x=group,y=H_Khorakivadokhani,color=group))
p3 + geom_boxplot(outlier.size=0)

p3 <- ggplot(data.df, aes(x=group,y=H_Noshidani,color=group))
p3 + geom_boxplot(outlier.size=0)+ylim(0,1000000)

p3 <- ggplot(data.df, aes(x=group,y=H_behdasht,color=group))
p3 + geom_boxplot(outlier.size=0)

p4 <- ggplot(data.df, aes(x=group,y=H_Maskan,color=group))
p4 + geom_boxplot(outlier.size=0)

p5 <- ggplot(data.df, aes(x=group,y=H_Hamloghl,color=group))
p5 + geom_boxplot(outlier.size=0)

p6 <- ggplot(data.df, aes(x=group,y=S.Z,color=group))
p6 + geom_boxplot(outlier.size=0)



p8 <- ggplot(data.df, aes(x=group,y=H_kalavakhadamat,color=group))
p8 + geom_boxplot(outlier.size=0)

p9 <- ggplot(data.df, aes(x=group,y=H_Pushak,color=group))
p9 + geom_boxplot(outlier.size=0)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9)

+geom_jitter(position= position_jitter(h=.1))
par(mfrow=c(2,2))
library(ggplot2)
gender=as.factor(genderr)

  p <- ggplot(data.df, aes(x=gender, y=log(dpermonth)), color=gender) + 
    geom_boxplot(color="cyan3", fill="cyan", alpha=0.2)
  p

r<- ggplot(data.df, aes(x=Agegroup, y=log(dpermonth)), fill=Agegroup) + 
  geom_boxplot(color="cyan3", fill="cyan",alpha=0.2) 
r
manzel=as.factor(T.M.S.)


  t<- ggplot(data.df, aes(x=manzel, y=dpermonth), fill=manzel) + 
    geom_boxplot(color="coral", fill="coral1",alpha=0.2)
t  
N.S.plot=as.factor(N.SS)
r<- ggplot(data.df, aes(x=N.S.plot, y=dpermonth), fill=N.S.plot) + 
  geom_boxplot(color="cyan3", fill="cyan",alpha=0.2) 
r
m=na.omit(Masleh)
masaleh=as.factor(Maslehh)
t<- ggplot(data.df, aes(x=masaleh, y=dpermonth), fill=masaleh) + 
  geom_boxplot(color="coral", fill="coral1",alpha=0.2)



t
faaliat.factor=as.factor(Faaliatt)
tabl
w<- ggplot(data.df, aes(x=faaliat.factor, y=dpermonth), fill=faaliat.fact) + 
  geom_boxplot(color="cyan3", fill="cyan",alpha=0.2)
w
na.omit(Madrak.factor)
Madrak.factor=as.factor(madrakk)
z<- ggplot(data.df, aes(x=Madrak.factor, y=log(dpermonth)), fill=Madrak.fact) + 
  geom_boxplot(color="cyan3", fill="cyan",alpha=0.2)
z
table(Madrak)
ggarrange(p,r)
library(ggpubr)
library(expss)
val_lab(manzel) = num_lab("
                 6 free
                 5 khedmat
                 4 rahn
                 3 ejari
                 2 malek
                 1 malekkol
                            
                              
                              
                            
          
")


manzel=as.character(manzel)
hazine.log=log(Hazine);dpermonth.log=log(dpermonth)
##S.Z
p <- ggplot(data=data.df, aes(x=dpermonth.log, y=hazine.log))
p + geom_point(size=4)

p <- ggplot(data=data.df, aes(x=S.Z))
p + geom_histogram()
##hazine
H_Taghzie=rowSums(data.df[,c(54,55,63)])
data.df=cbind(data.df,H_Taghzie)
View(data.df)
summary(H_Taghzie)
na.omit(H_Taghzie)
na.omit(H_Pushak)
na.omit(H_Maskan)
na.omit(H_behdasht)
na.omit(H_Hamloghl)
na.omit(H_Ertebatat)
na.omit(H_Tafrihat)
na.omit(H_kalavakhadamat)
a <- ggplot(data=data.df, aes(x=H_Khorakivadokhani))
a + geom_histogram()

a <- ggplot(data=data.df, aes(x=H_Noshidani))
a + geom_histogram()

a <- ggplot(data=data.df, aes(x=H_Ghazayeamade))
a + geom_histogram()

a <- ggplot(data=data.df, aes(x=H_mobleman))
a + geom_histogram()
b <- ggplot(data=data.df, aes(x=H_Pushak))
b + geom_histogram(bins=55)

c <- ggplot(data=data.df, aes(x=H_Maskan))
c + geom_histogram(bins=55)

d <- ggplot(data=data.df, aes(x=H_behdasht))
d + geom_histogram(bins=55)

e <- ggplot(data=data.df, aes(x=H_Hamloghl))
e + geom_histogram(bins=55)

f <- ggplot(data=data.df, aes(x=H_Ertebatat))
f + geom_histogram(bins=55)

g <- ggplot(data=data.df, aes(x=H_Tafrihat))
g + geom_histogram(bins=55)

h <- ggplot(data=data.df, aes(x=H_kalavakhadamat))
h + geom_histogram(bins=55)
ggarrange(c,e,f,h)
?stat_bin

g <- ggplot(data=data.df, aes(x=dpermonth))
g + geom_histogram()
na.omit(H_Pushak)
meanhazineha=colMeans(data.df[,c(56,57,58,59,60,61,62,64)])

H.means=colMeans(data.df[,c(56,57,58,59,60,61,62,64,69)])
barplot(H.means, col="coral1", main="Hazine",density =100,width = .9)

D.means=colMeans(data.df[,c(65,66,67,68)])
barplot(D.means, col="cyan3", main="dpermonth",density =100,width = .9)

#hararati
library(gplots)
heatmap.2(cor(data.df[,c(4,11,43:53,59)]),Rowv = F,Colv = F,dendrogram = "none",cellnote = round(cor(data.df[,c(4,11,43:53,59)]),2),notecol = "black",key = F,trace = "none",margins = c(10,10), main = "نمودار حرارتی ")
cate=factor(cat,levels = c("A","B","C","D"),labels = c("1","2","3","4"))

#matrici
plot(data.df[,c(4,11,43:53,59)])
library(GGally)
ggpairs(data.df[,c(43:53,59)])
par(xpd=TRUE) # allow legend to be displayed outside of plot area

plot(housing.df$NOX ~ housing.df$LSTAT, ylab = "NOX", xlab = "LSTAT",
     col = ifelse(housing.df$CAT..MEDV == 1, "black", "gray"))
library(ggplot2)
##3D
ggplot(data.df, aes(y = H_Maskan, x =S.Z , colour= response)) + geom_point(alpha = 0.6)

ggplot(data.df, aes(y = H_Khorakivadokhani, x = H_kalavakhadamat, colour= response)) + geom_point(alpha = 0.6)

ggplot(data.df, aes(y = H_behdasht, x =H_Tafrihat , colour= response)) + geom_point(alpha = 0.6)

ggplot(data.df, aes(y = H_behdasht, x =H_Ertebatat , colour= response)) + geom_point(alpha = 0.6)

ggplot(data.df, aes(y = H_Maskan, x =H_kalavakhadamat , colour= response)) + geom_point(alpha = 0.6)

ggplot(data.df, aes(y = H_Maskan, x =H_Khorakivadokhani, colour= response)) + geom_point(alpha = 0.6)

#scatter plot
p <- ggplot(data=data.df, aes(x=Age, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()

p <- ggplot(data=data.df, aes(x=H_Maskan, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()

p <- ggplot(data=data.df, aes(x=H_behdasht, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()

p <- ggplot(data=data.df, aes(x=H_Ertebatat, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()
p <- ggplot(data=data.df, aes(x=H_Hamloghl, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()
boxplot(H_Hamloghl)
boxplot(y)
p <- ggplot(data=data.df, aes(x=H_Tafrihat, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()

p <- ggplot(data=data.df, aes(x=H_mobleman, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()

p <- ggplot(data=data.df, aes(x=H_Khorakivadokhani, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()

p <- ggplot(data=data.df, aes(x=H_behdasht, y=dpermonth))
p + geom_point(shape=1) + geom_smooth()


#hararati
library(gplots)
colMeans(x)
x=na.omit(data.df[,c(1,2,3,6:11,13,14,61)])
heatmap.2(cor(x),Rowv = F,Colv = F,dendrogram = "none",cellnote = round(cor(x),2),notecol = "black",key = F,trace = "none",margins = c(10,10), main = "نمودار حرارتی ")
data.df=Data

attach(data.df)
boxplot(dpermonth~oto)
library(ggplot2)
p <- ggplot(data=data.df, aes(x=otoo, y=dpermonth))
p + geom_boxplot(shape=1) + geom_smooth()
car=as.factor(oto)
