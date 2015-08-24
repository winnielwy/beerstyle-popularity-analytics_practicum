#Data cleaning
setwd("/Users/wenyingliu/Desktop")
untap=read.csv("practicum/untappd512/untappd_0512data.csv")
untapp=data.frame(untap$beerstyle,untap$GEOID_Data)
untapp["Count"]=c(rep(0,nrow(untapp)))
colnames(untapp)=c("beerstyle","GEOID","Count")
checkin <- aggregate(Count ~ GEOID + beerstyle, untapp, length)

#Top10 Beers
library(plyr)
beer_records=ddply(checkin,.(beerstyle),summarise,num_records=sum(Count))
beer_range=beer_records[with(beer_records,order(-num_records)),]
top10=beer_range[1:10,]$beerstyle
TOP10=checkin[(checkin$beerstyle %in% top10),]
TOP10=TOP10[-1,]
TOP10["Count"]=NULL
TOP10["Checkin"]=c(rep(1,nrow(TOP10)))
TOP10$Checkin=gsub(1,'Y',TOP10$Checkin)

#separate the data by top10 beerstyle;
x=list()
for (i in 1:length(top10)) {x[[i]]=TOP10[which(TOP10$beerstyle==top10[i]),]}

#geographic data
setwd("/Users/wenyingliu/Desktop/practicum/untappd512") 
filenames <- list.files(getwd()) 
dat<-lapply(filenames,read.csv)
load("acsdata.Rdata")
#delete column"m" in geographic data
for (i in 1:length(filenames)){dat[[i]]=dat[[i]][,c(-grep('m',colnames(dat[[i]])))]}
#delete unrelated columns
data=data.frame(dat)
acs=data[,c(-grep(paste(c('GEOID.','OBJECTID'),collapse='|'), colnames(data)))]
acs=acs[, colSums(is.na(acs)) == 0]

geo=list()
geo[["GEOID"]]=acs$GEOID
geo[["population"]]=acs$B01001e1                             
geo[["pct_male"]]=acs$B01001e2/acs$B01001e1                                  
geo[["pct_22.39"]]=rowSums(acs[,c("B01001e10","B01001e11","B01001e12","B01001e13","B01001e34",
                                  "B01001e35","B01001e36","B01001e37")])/acs$B01001e1
geo[["pct_40.59"]]=rowSums(acs[,c("B01001e14","B01001e15","B01001e16","B01001e17","B01001e38",
                                  "B01001e39","B01001e40","B01001e41")])/acs$B01001e1                               
geo[["pct_white.percentage"]]=acs$B02001e2/acs$B02001e1                     
geo[["pct_black.or.African.American.percentage"]]=acs$B02001e3/acs$B02001e1 
geo[["pct_American.Indian.percentage"]]=acs$B02001e4/acs$B02001e1           
geo[["pct_Asian.percentage"]]=acs$B02001e5/acs$B02001e1                      
geo[["pct_work.in.state"]]=acs$B08007e2/acs$B08007e1
geo[["pct_work.in.county"]]=acs$B08007e3/acs$B08007e1
geo[["total travel time to work"]]=acs$B080013e1
geo[["number of vehicles"]]=acs$B080015e1
geo[["pct_never.married"]]=rowSums(acs[,c("B12001e3","B12001e12")])/acs$B12001e1
geo[["pct_married"]]=rowSums(acs[,c("B12001e4","B12001e13")])/acs$B12001e1
geo[["pct_married_spouse_present"]]=rowSums(acs[,c("B12001e5","B12001e14")])/acs$B12001e1
geo[["pct_married_spouse_absent"]]=rowSums(acs[,c("B12001e6","B12001e15")])/acs$B12001e1
geo[["pct_married_separated"]]=rowSums(acs[,c("B12001e7","B12001e16")])/acs$B12001e1
geo[["pct_widowed"]]=rowSums(acs[,c("B12001e9","B12001e18")])/acs$B12001e1                              
geo[["pct_divorced"]]=rowSums(acs[,c("B12001e10","B12001e19")])/acs$B12001e1                             
geo[["pct_not.in.school"]]=rowSums(acs[,c("B14002e25","B14002e49")])/acs$B14002e1                         
geo[["pct_graduate"]]=rowSums(acs[,c("B14002e22","B14002e46")])/acs$B14002e1                              
geo[["pct_undergraduate"]]=rowSums(acs[,c("B14002e19","B14002e43")])/acs$B14002e1
geo[["pct_lower than undergraduate"]]=rowSums(acs[,c("B14002e4","B14002e7","B14002e10","B14002e13","B14002e16",
                                                     "B14002e28","B14002e31","B14002e34","B14002e37","B14002e40")])/acs$B14002e1
geo[["pct_income.larger.than.100.000"]]=rowSums(acs[,c("B19001e14","B19001e15","B19001e16","B19001e17")])/acs$B19001e1         
geo[["pct_income.less.than.30.000"]]=rowSums(acs[,c("B19001e2","B19001e3","B19001e4","B19001e5","B19001e6")])/acs$B19001e1    
geo[["pct_income.30.000-99.000"]]=rowSums(acs[,c("B19001e7","B19001e8","B19001e9","B19001e10","B19001e11","B19001e12")])/acs$B19001e1 
geo[["median_income"]]=acs$B19013e1
geo[["per capital income"]]=acs$B191301e1 
geo[["pct_employment"]]=rowSums(acs[,c("B23022e3","B23022e27")])/acs$B23022e1
geo[["pct_more.than.two.type.insurance"]]=rowSums(acs[,c("B27010e26","B27010e42")])/rowSums(acs[,c("B27010e18","B27010e34")])
geo[["pct_no.insurance"]]=rowSums(acs[,c("B27010e33","B27010e50")])/rowSums(acs[,c("B27010e18","B27010e34")])
geodata=as.data.frame(geo)

#delete NA and 0;
no_na=geodata[rowSums(is.na(geodata)) == 0,]
binary=no_na[,colSums(no_na==1|no_na==0)==nrow(no_na)]
zero=no_na[,colSums(no_na==0)==nrow(no_na)]
#`%ni%` <- Negate(`%in%`)
#practicum=subset(no_na, select =names(no_na) %ni% colnames(binary) )
#clean=practicum[,colSums(practicum==0)<= 0.2*nrow(practicum)]
GEOID=no_na$GEOID
#save(GEOID,file="GEO29.Rdata")


###############top1 beer example
untapp=data.frame(untap$beerstyle,untap$GEOID_Data)
colnames(untapp)=c("beerstyle","GEOID")
TOP10=untapp[(untapp$beerstyle %in% top10),]
x=list()
for (i in 1:length(top10)) {x[[i]]=TOP10[which(TOP10$beerstyle==top10[i]),]}
one=as.data.frame(x[[2]])
one["Checkin"]=rep(1,dim(one)[1])

TTOP1=rbind(one,add)
TTOP_1=merge(TTOP1,no_na,by="GEOID")

TTOP_1[TTOP_1==-Inf]=0
TTOP_1[TTOP_1==Inf]=0
#save(TTOP_1,file="TTOP1.Rdata")

#####correlation
library(corrplot)
M <- cor(TTOP_1[,-c(1:3)])
corrplot(M, method = "circle",tl.cex=0.005)
#install.packages("caret")
library('caret')
hc = findCorrelation(M, cutoff=0.8,exact=TRUE)
Hc = findCorrelation(M, cutoff=0.8,names=TRUE,exact=TRUE)# putt any value as a "cutoff" 
hc = sort(hc)
t_1=TTOP_1[,-c(15,16,28)]
save(t_1,file="top2_no_coor.Rdata")

####Logistics regression
summary(t_1)
top1=t_1[,3:length(t_1)]
#standardize data
top1_scale=scale(top1[,-1],scale=FALSE)
# check that we get mean of 0 and sd of 1
colMeans(top1_scale)
apply(top1_scale, 2, sd)
top1_scale=data.frame(top1_scale)
top1_scale['Checkin']=top1$Checkin
#pairs(top1_scale,col=top1_scale$Checkin)
table(top1_scale$Checkin)
library(plotrix)
pie3D(c(198016,73737), labels =c("No","Yes"), main="Pie Chart of Checkin record_all data")
pie3D(c(132011,49158), labels =c("No","Yes"), main="Pie Chart of Checkin record_train set")
pie3D(c(66005,24579), labels =c("No","Yes"), main="Pie Chart of Checkin record_test set")
######training and test set1
set.seed(4)
beerR<-subset(top1_scale, Checkin!='1')
sampR<-sample(1:nrow(beerR), trunc(nrow(beerR)/3), replace=FALSE)
beerS<-subset(top1_scale, Checkin=='1')
sampS<-sample(1:nrow(beerS), trunc(nrow(beerS)/3), replace=FALSE)
Vsamp<-beerR[sampR,]
test_set2<-rbind(Vsamp, beerS[sampS,])
table(test_set2$Checkin)
#  0     1 
#66005 24579

Vsamp<-Vsamp[,-c(1,2)]
V_test <- Vsamp$Checkin
beerR<-beerR[setdiff(1:nrow(beerR), sampR),]
beerS<-beerS[setdiff(1:nrow(beerS), sampS),]
train_set2<-rbind(beerR, beerS)
table(train_set2$Checkin)
#   0      1 
# 132011  49158 

#######train test set2
yes=subset(top1_scale,Checkin==1)
no=subset(top1_scale,Checkin==0)
set.seed(4)
no_rate=sample(1:nrow(no), floor(nrow(yes)))
sample_no=no[no_rate,]

train=sample(1:nrow(yes), floor(0.7*nrow(yes)))
train_yes= yes[train,]
train_no=sample_no[train,]
train_set=rbind(train_yes,train_no)

test_yes = yes[-train,]
test_no=sample_no[-train,]
test_set=rbind(test_yes,test_no)

Obs.test = test_set$Checkin

####Logsitics regression

trainfit<-glm(Checkin ~., data=train_set, family=binomial)

#pre=cv.glm(data=top1_scale,glmfit=trainfit,K=10)
summary(trainfit)
trainfit<-glm(Checkin ~.-pct_40.59-pct_lower.than.undergraduate-pct_income.30.000.99.000, data=train_set, family=binomial)


#Model coefficients
coef(trainfit)
#Exponentiated coefficients
exp(coef(trainfit)) 
#stepwise logistic regression
pp=step(trainfit)
#Prediciting the test_set dataset
pred<-predict(trainfit,test_set,type="response")
#Check some of the predicted values
pred[1:5]
#plot(test_set$Checkin,pred)
#Confusion Matrix and some accuracy measures
library(SDMTools)
matrix = confusion.matrix(test_set$Checkin,pred,threshold=0.46)
AccuMeasures = accuracy(test_set$Checkin,pred,threshold=0.46)

#Extracting specific values from accuracy table
AccuMeasures$sensitivity

#creating a range of values to test for accuracy
thresh=seq(0,1,by=0.01)
#Initializing a 1*20 matrix of zeros to save values of accuracy
acc = matrix(0,1,100)
specificity=matrix(0,1,100)
sensitivity=matrix(0,1,100)

par(mar=c(5.1,4.1,4.1,2.1))
for (i in 1:101){
  matrix = confusion.matrix(test_set$Checkin,pred,threshold=thresh[i])
  acc[i]=(matrix[1,1]+matrix[2,2])/nrow(test_set)
  specificity[i]=matrix[2,2]/(matrix[1,2]+matrix[2,2])
  sensitivity[i]=matrix[1,1]/(matrix[1,1]+matrix[2,1])
}

plot(thresh,acc,type="l",ylim=c(0,1),col="black",xlab="Threshold",ylab="Accuracy/specificity/sensitivity",cex=0.6)
points(thresh,specificity,type="l",col="red",xlab="Threshold")
points(thresh,sensitivity,type="l",col="blue",xlab="Threshold")
grid()
legend("bottom",legend=c("acc","sensitivity","specificity"),col=c("black","red","blue"),bg="white",lwd=2)


#Lift chart
bb=cbind(test_set$Checkin,pred)
bb1=bb[order(pred,decreasing=TRUE),]
xbar=mean(pred)
axis=dim(test_set)
ax=dim(test_set)
ay=dim(test_set)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:dim(test_set)){
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}
aaa=cbind(bb1[,1],bb1[,2],ay,ax)
plot(axis,ay,xlab="number of cases",ylab="number of successes",col="red",main="lift:Cum
     success sorted by pred val/success prob")
points(axis,ax,col="blue")
grid()
#legend("bottomright",legend=c("Cumulativ 1's sorted by predicted values","Cumulative 1's using average"),col=c("red","blue"),bg="white",text.width=1.5)

##Cross-Validation
newdata=ROSE(Checkin~.,data=top1_scale,seed=4)$data
table(newdata$Checkin)

folds=sample(rep(1:10,length=nrow(top1_scale)))
folds
table(folds)
predict0=list()
matrix0=list()
acc_table=list()
specificity_table=list()
sensitivity_table=list()

for (k in 1:10){
  trainfit0<-glm(Checkin ~.-pct_40.59-pct_lower.than.undergraduate-pct_income.30.000.99.000, data=top1_scale[folds!=k,], family=binomial)
  predict0[[k]]<-predict(trainfit0,top1_scale[folds==k,],type="response")
  matrix0[[k]] = confusion.matrix(top1_scale[folds==k,]$Checkin,predict0[[k]],threshold=0.46)
  acc_table[[k]]=(matrix0[[k]][1,1]+matrix0[[k]][2,2])/nrow(top1_scale[folds==k,])
  specificity_table[[k]]=matrix0[[k]][2,2]/(matrix0[[k]][1,2]+matrix0[[k]][2,2])
  sensitivity_table[[k]]=matrix0[[k]][1,1]/(matrix0[[k]][1,1]+matrix0[[k]][2,1])
}
a=unlist(acc_table)
b=unlist(specificity_table)
c=unlist(sensitivity_table)
plot(a,type="l",ylim=c(0,1),col="black",xlab="Fold",ylab="Accuracy/specificity/sensitivity",cex=0.6,main="Cross validation")
points(b,type="l",col="red",xlab="Fold")
points(c,type="l",col="blue",xlab="Fold")
grid()
legend("bottom",legend=c("acc","sensitivity","specificity"),col=c("black","red","blue"),bg="white",lwd=2)


#imbalance with Rose package
m=glm(Checkin~.-pct_income.30.000.99.000 -pct_lower.than.undergraduate ,family=binomial(), data=train_set2)
p=predict(m,test_set2,type="response")

#install.packages("ROSE")
library(ROSE)
newset=ROSE(Checkin~.,data=train_set2,seed=4)$data
table(newset$Checkin)
m.new=glm(Checkin~.-pct_married_separated-pct_income.less.than.30.000 -pct_lower.than.undergraduate ,family=binomial(), data=newset)
p.new=predict(m.new,test_set2,type="response")
#check accuracy by measuring auc
matrix_new = confusion.matrix(test_set2$Checkin,p.new,threshold=0.47)
AccuMeasures_new = accuracy(test_set2$Checkin,p.new,threshold=0.47)
matrix = confusion.matrix(test_set2$Checkin,p,threshold=0.47)
AccuMeasures = accuracy(test_set2$Checkin,p,threshold=0.47)
roc.curve(test_set2$Checkin,p)
roc.curve(test_set2$Checkin,p.new,add.roc=TRUE,col=2)
accuracy.meas(test_set2$Checkin,p,threshold=0.46)
#balance data set with over and under sampling
data.balanced.ou=ovun.sample(Checkin~.,data=train_set2,N=nrow(train_set2),
                             p=0.5,seed=4,method="both")$data
table(data.balanced.ou$Checkin)
m.new_ou=glm(Checkin~.-pct_married_separated-pct_income.less.than.30.000 -pct_lower.than.undergraduate ,family=binomial(), data=data.balanced.ou)
p.new_ou=predict(m.new,test_set2,type="response")

######Cutoff setting
set.seed(4)
beerR<-subset(top1_scale, Checkin!='1')
sampR<-sample(1:nrow(beerR), trunc(nrow(beerR)/3), replace=FALSE)
beerS<-subset(top1_scale, Checkin=='1')
sampS<-sample(1:nrow(beerS), trunc(nrow(beerS)/3), replace=FALSE)
Vsamp<-beerR[sampR,]
test_set2<-rbind(Vsamp, beerS[sampS,])

Vsamp<-Vsamp[,-c(1,2)]
V_test <- Vsamp$Checkin
beerR<-beerR[setdiff(1:nrow(beerR), sampR),]
beerS<-beerS[setdiff(1:nrow(beerS), sampS),]
train_set2<-rbind(beerR, beerS)

m=glm(Checkin~.-pct_lower.than.undergraduate ,family=binomial(), data=train_set2)
p=predict(m,test_set2,type="response")


getMisclass <- function(cutoff, p, labels){
  pred <- factor(1*(p > cutoff), labels=c("No Checkin", "Checkin")) 
  t <- table(pred, labels)
  cat("cutoff ", cutoff, ":\n")
  print(t)
  cat("correct    :", round(sum(t[c(1,4)])/sum(t), 2),"\n")
  cat("correct No :", round(t[1]/sum(t[,1]), 2),"\n")
  cat("correct Yes:", round(t[4]/sum(t[,2]), 2),"\n\n")
  invisible(t)
}
cutoffs <- seq(.1,.9,by=.1)
sapply(cutoffs, getMisclass, p=p, labels=test_set2$Checkin)

##Random Forest
#load('AmericanIPA')
#TOP1 = read.csv('top1.csv')
load('top1_no_coor.Rdata')
#table(TOP1$Checkin)
#TOP1 <- TOP1[sample(1:nrow(TOP1),5000),]
TOP1<-t_1

set.seed(17)
TOP1$Checkin <- as.factor(TOP1$Checkin)
beerR<-subset(TOP1, Checkin!='1')
sampR<-sample(1:nrow(beerR), trunc(nrow(beerR)/3), replace=FALSE)
beerS<-subset(TOP1, Checkin=='1')
sampS<-sample(1:nrow(beerS), trunc(nrow(beerS)/3), replace=FALSE)
Vsamp<-beerR[sampR,]
Vsamp<-rbind(Vsamp, beerS[sampS,])
Vsamp<-Vsamp[,-c(1,2)]
V_test <- Vsamp$Checkin
beerR<-beerR[setdiff(1:nrow(beerR), sampR),]
beerS<-beerS[setdiff(1:nrow(beerS), sampS),]
TOP11<-rbind(beerR, beerS)
TOP11 <- TOP11[,-c(1,2)]

eN1 <- c()
eY1 <- c()
eA1 <- c()
library(randomForest)
for (x in c(seq(1,10,length.out=45))) {
beer.rf<-randomForest(Checkin~ ., data=TOP11, importance=TRUE, do.trace=TRUE, ntree=10, 
                      mtry=2*(trunc(length(TOP11)^(1/2))), replace=FALSE, scale=FALSE,
                      classwt=c(x*(length(TOP11[,1])/table(TOP11$Checkin)[["0"]]),(length(TOP11[,1])/table(TOP11$Checkin)[["1"]])))
print(x) 
eN1<-c(eN1,beer.rf$err.rate[2,2])
eY1<-c(eY1,beer.rf$err.rate[2,3])
eA1 <- c(eA1,beer.rf$err.rate[2,1])
}
plot(eN1~c(seq(1,10,length.out=45)), type='l', ylim=c(0,0.2))
points(eY1~c(seq(1,10,length.out=45)),type='l',col='Red')
points(eA1~c(seq(1,10,length.out=45)),type='l',col='Blue')

X <- c(seq(1,10,length.out=45))
error.table <- data.frame(X,eN1,eY1,eA1)

error.table['sum'] <- rowSums(error.table[,c(2,3)])

which(error.table$eA1==min(error.table$eA1)) #16
which(error.table$sum==min(error.table$sum)) #27
which(error.table$eY1==min(error.table$eY1)) #42


beer.rf<-randomForest(Checkin~ ., data=TOP11, importance=TRUE, do.trace=TRUE, ntree=500, 
                      mtry=2*(trunc(length(TOP11)^(1/2))), replace=FALSE, scale=FALSE,
                      classwt=c(9.386364*(length(TOP11[,1])/table(TOP11$Checkin)[["0"]]),(length(TOP11[,1])/table(TOP11$Checkin)[["1"]])))
varImpPlot(beer.rf)
validation = predict(beer.rf, newdata=Vsamp)
confu<-table(predicted=validation,observed=V_test)
spec=confu[1]/sum(confu[1],confu[2])
sensi=confu[4]/sum(confu[3],confu[4])
print(spec)
print(sensi)
save(beer.rf, file='beer.rf')

#####ROC Curve#####
beer.rf.pr = predict(beer.rf,type="prob",newdata=Vsamp)[,2]
library('ROCR')
beer.pred <- prediction(beer.rf.pr, V_test)
beer.perf = performance(beer.pred,"fpr","tpr")

plot(beer.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

auc <- performance(beer.pred,"auc")

auc <- unlist(slot(auc, "y.values"))
#auc = 0.9418386
minauc<-min(round(auc, digits = 2))
maxauc<-max(round(auc, digits = 2))
minauct <- paste(c("min(AUC) = "),minauc,sep="")
maxauct <- paste(c("max(AUC) = "),maxauc,sep="")



###########################################
eN1 <- c()
eY1 <- c()
eA1 <- c()
set.seed(14)
for (x in c(seq(1,2,length.out=50))) {
  
  beer.rf<-randomForest(Checkin~ ., data=tran_w, importance=TRUE, do.trace=TRUE, ntree=100, 
                      mtry=2*(trunc(length(TOP11)^(1/2))), replace=FALSE, scale=FALSE,
                      strata = TOP11$Checkin, sampsize=c(1,x))
  print(x) 
  eN1<-c(eN1,beer.rf$err.rate[100,2])
  eY1<-c(eY1,beer.rf$err.rate[100,3])
  eA1 <- c(eA1,beer.rf$err.rate[100,1])
}

plot(eN1~c(seq(1,2,length.out=50)), type='l')
points(eY1~c(seq(1,2,length.out=50)),type='l',col='Red')
points(eA1~c(seq(1,2,length.out=50)),type='l',col='Blue')

X <- c(seq(1,2,length.out=50))
error.table <- data.frame(X,eN1,eY1,eA1)

error.table['sum'] <- rowSums(error.table[,c(2,3)])

which(error.table$eA1==min(error.table$eA1))
which(error.table$sum==min(error.table$sum))

#the over sampling rate smallest sum of specificty and sensitivity is 1.122449
set.seed(7)
beer.rf<-randomForest(Checkin~., data=TOP11, importance=TRUE, do.trace=TRUE, ntree=100, 
                      mtry=2*(trunc(length(TOP11)^(1/2))), replace=FALSE, scale=FALSE,
                      strata = TOP11$Checkin, sampsize=c(1,1.6))
beer.rf$err.rate
validation = predict(beer.rf, newdata=V_test)
confu<-table(observed=testset,predicted=validation)
spec=confu[1]/sum(confu[1],confu[2])
sensi=confu[4]/sum(confu[3],confu[4])
print(spec)
print(sensi)
varImpPlot(beer.rf)
plot(witvalidation, rating_w.test)


plot(beer.rf$err.rate[,1])
points(beer.rf$err.rate[,2], col='Red')
points(beer.rf$err.rate[,3], col='Blue')


beer.rf<-randomForest(Checkin~ ., data=TOP11, importance=TRUE, do.trace=TRUE, ntree=50, 
                      mtry=2*(trunc(length(TOP11)^(1/2))), replace=FALSE, scale=FALSE,
                      cutoff=c(0.6368421,1-0.6368421))

variable <-names(sort(beer.rf$importance[,3], dec=T)[1:(length(TOP11)-1)])
c <- c()
eY1 <- c()
eA1 <- c()
for (x in 1:length(variable)) {
  topx<-TOP11[c("Checkin",names(sort(beer.rf$importance[,3], dec=TRUE)[1:x]))]
  forward.rf<-randomForest(Checkin~ ., data=topx, importance=TRUE, do.trace=TRUE, ntree=50, 
                        mtry=2*(trunc(length(TOP11)^(1/2))), replace=FALSE, scale=FALSE,
                        cutoff=c(0.6368421,1-0.6368421))
  eN1<-c(eN1,forward.rf$err.rate[10,2])
  eY1<-c(eY1,forward.rf$err.rate[10,3])
  eA1 <- c(eA1,forward.rf$err.rate[10,1])
}

X <- c(1:25)
error.table <- data.frame(X,eN1,eY1,eA1)
write.csv(error.table,file='Error.csv')

plot(eN1~c(1:25), type='l', ylim = c(0,0.5), 
     main = 'Error Rate plot with different Predictors',ylab='Error Rate',xlab='Number of predictor')
points(eY1~c(1:25),type='l',col='Red')
points(eA1~c(1:25),type='l',col='Blue')


