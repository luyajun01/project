#import data
options(encoding="utf-8")
setwd("/Users/luyajun/Documents/坚果云/我的坚果云/工作/论文/信用风险模型论文/data")
#setwd("D:\\坚果云\\我的坚果云\\工作\\论文\\信用风险模型论文\\data")
library(readxl)
library(Hmisc)
library(mice)
library(DMwR)
library(car)
library(grpreg)
library(boot)
library(glmnet)
normaldata=read_xls("data_normal.xls")
stdata=read_xls("data_st.xls")
dim(normaldata)
dim(stdata)
#缺失值补全
#normaldata[!complete.cases(normaldata),]
#nrow(normaldata[!complete.cases(normaldata),])
#stdata[!complete.cases(stdata),]
#nrow(stdata[!complete.cases(stdata),])
#describe(normaldata)
typeof(normaldata)##识别数据类型
nostdata<-data.matrix(normaldata)#将list转成numeric
ystdata<-data.matrix(stdata)#将list转成numeric
for(i in 1:ncol(normaldata)){
  nostdata[is.na(nostdata[,i]),i]<-mean(nostdata[,i],na.rm=TRUE)
}
for(i in 1:ncol(stdata)){
  ystdata[is.na(ystdata[,i]),i]<-mean(ystdata[,i],na.rm=TRUE)
}
#knnOutput<-knnImputation(normaldata[,!names(normaldata) %in% "medv"])  # 使用KNN插值.
#anyNA(knnOutput)
###导出数据
write.csv(nostdata,"nostdata.csv",append=FALSE,quote=TRUE,sep="",eol="\n",dec=".",fileEncoding="UTF-8")
write.csv(ystdata,"ystdata.csv",append=FALSE,quote=TRUE,sep="",eol="\n",dec=".",fileEncoding="UTF-8")

###检验

#方差齐次性检验
##数据准备
data1=c(nostdata[,2],ystdata[,2])
dim(nostdata)[1]
a<-factor(c(rep(1,dim(nostdata)[1]),rep(2,dim(ystdata)[1])))
#bartlett.test方差齐性检验
bartlett.test(data1~a)
#var.test方差齐性检验
var.test(data1~a)
#levene.test方差齐性检验（也是SPSS的默认方差齐性检验方法）
library(car)
levene.test(data1~a)
#前两者是对原始数据的方差进行检验的，leveneTest是对方差模型的残差进行组间齐性检验.一般认为是要求残差的方差齐，所以一般的统计软件都做的是leveneTest
#t检验
dim(nostdata)[2]
#for (i in 1:dim(nostdata)[2]) {
# t.test(nostdata[,i],ystdata[,i],paired=FALSE) }
testresults=rep(NA,(dim(nostdata)[2]-1))
for (i in 2:dim(nostdata)[2]) {
  testresults[i]=print(t.test(nostdata[,i],ystdata[,i],paired=FALSE)$p.value) }  #把所有p值列举出来
```

由于样本存在不平衡的情况所以要对原始样本进行重抽样。

```{r, results="hide"}
###smote抽样

#增加一个标签Species

normaldata=read.csv("nostdata.csv")
stdata=read.csv("ystdata.csv")
Species=c(rep("common",length=dim(normaldata)[1]),rep("rare",length=dim(stdata)[1]))
data=data.frame(Species,rbind(normaldata,stdata))
newData<-SMOTE(Species~.,data,perc.over=300,perc.under=100)
write.csv(newData,"newdata.csv",append=FALSE,quote=TRUE,sep="",eol="\n",dec=".",fileEncoding="UTF-8")

###检验重抽样之后两组样本间是否有显著差异
newdata=read.csv("newdata.csv")
nostnewdata=subset(newdata,newdata$Species=="common")
nostnewdata=nostnewdata[,-c(1:4)]
#ystnewdata=newData[which(Species=="rare"),]
ystnewdata=subset(newdata,newdata$Species=="rare")
ystnewdata=ystnewdata[,-c(1:4)]
testresults=rep(NA,(dim(newdata)[2]-4))
for (i in 1:length(testresults)) {
  testresults[i]=print(t.test(nostnewdata[,i],ystnewdata[,i],paired=FALSE)$p.value) } 

Y=c(rep(0,length(which(newdata$Species=="common"))),rep(1,length(which(newdata$Species=="rare"))))
X=newdata[,-c(1:4)]
scale.X=scale(X)
#group<-as.vector(unlist(c(rep("x1",11),rep("x2",8),rep("x3",7),rep("x4",5),rep("x5",11),rep("x6",6))))
group<-c(rep(1,11),rep(2,8),rep(3,7),rep(4,5),rep(5,11),rep(6,6))
#####方法选择######
###################
###################
#fit <- grpreg(scale.X, Y, group, penalty="grLasso", family="binomial")
#plot(fit)
#fit <- grpreg(scale.X, Y, group, penalty="grMCP", family="binomial")
#plot(fit)
#fit <- grpreg(scale.X, Y, group, penalty="grSCAD", family="binomial")
#plot(fit)
#fit <- grpreg(scale.X, Y, group, penalty="gel", family="binomial")
#plot(fit)
#fit <- grpreg(scale.X, Y, group, penalty="cMCP", family="binomial")
#plot(fit)
#select(fit, "BIC")

#####
#####
cvfit<-cv.grpreg(scale.X,Y,group,family="binomial",penalty="gel")
coef(cvfit)
predict(cvfit, scale.X)
predict(cvfit, scale.X, type="class")
#predict(cvfit, type="groups")
library(boot)
dt=data.frame(Y,scale.X)
dt<-dt[sample(nrow(dt)),]
folds<-cut(seq(1,nrow(dt)),breaks=4,labels=FALSE)
testIndexes<-which(folds==1,arr.ind=TRUE)
testData<-dt[testIndexes, ]
trainData<-dt[-testIndexes, ]
result1<-rep(NA,dim(testData)[1])
result2<-rep(NA,dim(testData)[1])
result3<-rep(NA,dim(testData)[1])
result4<-rep(NA,dim(testData)[1])
result5<-rep(NA,dim(testData)[1])
result6<-rep(NA,dim(testData)[1])
###选择lambda
cvfit1<-cv.grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grMCP", family="binomial")
cvfit2<-cv.grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="gel", family="binomial")
cvfit3<-cv.grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grLasso", family="binomial")
cvfit4<-cv.grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grSCAD", family="binomial")
cvfit5<-cv.grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="cMCP", family="binomial")
cvfit6<-cv.glmnet(as.matrix(trainData[,-1]), trainData[,1], family="binomial",alpha=1)
##组变量方法
fit1<-grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grMCP", family="binomial",lambda=cvfit1$lambda[which.min(cvfit1$cve)])
fit2<-grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="gel", family="binomial",lambda=cvfit2$lambda[which.min(cvfit2$cve)])
fit3<-grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grLasso", family="binomial",lambda=cvfit3$lambda[which.min(cvfit3$cve)])
fit4<-grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grSCAD", family="binomial",lambda=cvfit4$lambda[which.min(cvfit4$cve)])
fit5<-grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="cMCP", family="binomial",lambda=cvfit5$lambda[which.min(cvfit5$cve)])
fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1)
##一般方法
##logistic
ldata=rbind(trainData,)

fit6<-glm(Y~.,family=binomial,data=trainData)
summary(fit6)
##

result1=predict(fit1,as.matrix(testData[,-1]),type="class")
result2=predict(fit2,as.matrix(testData[,-1]),type="class")
result3=predict(fit3,as.matrix(testData[,-1]),type="class")
result4=predict(fit4,as.matrix(testData[,-1]),type="class")
result5=predict(fit5,as.matrix(testData[,-1]),type="class")
result6<-predict(fit6,testData,type="response")



result <- list()
accuracy1 <- list()
dt=data.frame(Y,scale.X)
folds<-cut(seq(1,nrow(dt)),breaks=4,labels=FALSE)
testIndexes<-which(folds==1,arr.ind=TRUE)
ht<-list()
for(i in 1:4){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData<-ht[[i]][testIndexes, ]
  trainData<-ht[[i]][-testIndexes, ]
  cvfit1<-cv.grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grMCP", family="binomial")
  ##组变量方法
  fit1<-grpreg(as.matrix(trainData[,-1]), trainData[,1], group, penalty="grMCP", family="binomial",lambda=cvfit1$lambda[which.min(cvfit1$cve)])
  result[[i]] <- predict(fit1,as.matrix(testData[,-1]),type="class")
  accuracy1[[i]]=mean(testData[,1]==result[[i]])
}
mean(accuracy1)













dt=data.frame(Y,scale.X)
folds<-cut(seq(1,nrow(dt)),breaks=4,labels=FALSE)
testIndexes<-which(folds==1,arr.ind=TRUE)
result1<-list()
result2<-list()
result3<-list()
result4<-list()
result5<-list()
result6<-list()
result7<-list()
accuracy1 <- list()
accuracy2 <- list()
accuracy3 <- list()
accuracy4 <- list()
accuracy5 <- list()
accuracy6 <- list()
accuracy7 <- list()
cvfit1 <- list()
cvfit2 <- list()
cvfit3 <- list()
cvfit4 <- list()
cvfit5 <- list()
cvfit6 <- list()
fit1 <- list()
fit2 <- list()
fit3 <- list()
fit4 <- list()
fit5 <- list()
fit6<-list()
fit7<-list()
ht<-list()
testData<-list()
trainData<-list()
##################
####grMCP罚#######
##################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit1[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grMCP", family="binomial")
  #cvfit2[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="gel", family="binomial")
  #cvfit3[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial")
  #cvfit4[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial")
  #cvfit5[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial")
  #cvfit6<-cv.glmnet(as.matrix(trainData[,-1]), trainData[,1], family="binomial",alpha=1)
  ##组变量方法
  fit1[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grMCP", family="binomial",lambda=cvfit1[[i]]$lambda[which.min(cvfit1[[i]]$cve)])
  #fit2[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="gel", family="binomial",lambda=cvfit2[[i]]$lambda[which.min(cvfit2[[i]]$cve)])
  #fit3[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial",lambda=cvfit3[[i]]$lambda[which.min(cvfit3[[i]]$cve)])
  #fit4[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial",lambda=cvfit4[[i]]$lambda[which.min(cvfit4[[i]]$cve)])
  #fit5[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial",lambda=cvfit5[[i]]$lambda[which.min(cvfit5[[i]]$cve)])
  ##一般方法
  ##logistic
  #fit6<-glm(Y~.,family=binomial,data=trainData)
  #fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1,lambda = cvfit6$lambda.min) 
  result1[[i]]=predict(fit1[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result2[[i]]=predict(fit2[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result3[[i]]=predict(fit3[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result4[[i]]=predict(fit4[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result5[[i]]=predict(fit5[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result6[[i]]<-ifelse(predict(fit6,testData,type="response") > 0.51,1,0)
  #result7[[i]]<-predict(fit7,as.matrix(testData[,-1]),type="class")
  ###计算准确率
  accuracy1[[i]]=mean(testData[[i]][,1]==result1[[i]])
  #accuracy2[[i]]=mean(testData[[i]][,1]==result2[[i]])
  #accuracy3[[i]]=mean(testData[[i]][,1]==result3[[i]])
  #accuracy4[[i]]=mean(testData[[i]][,1]==result4[[i]])
  #accuracy5[[i]]=mean(testData[[i]][,1]==result5[[i]])
  #accuracy6[[i]]=mean(testData[,1]==result6[[i]])
  #accuracy7[[i]]=mean(testData[,1]==result7[[i]])
}
#######################
##########gel##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit2[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="gel", family="binomial")
  #cvfit3[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial")
  #cvfit4[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial")
  #cvfit5[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial")
  #cvfit6<-cv.glmnet(as.matrix(trainData[,-1]), trainData[,1], family="binomial",alpha=1)
  ##组变量方法
  fit2[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="gel", family="binomial",lambda=cvfit2[[i]]$lambda[which.min(cvfit2[[i]]$cve)])
  #fit3[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial",lambda=cvfit3[[i]]$lambda[which.min(cvfit3[[i]]$cve)])
  #fit4[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial",lambda=cvfit4[[i]]$lambda[which.min(cvfit4[[i]]$cve)])
  #fit5[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial",lambda=cvfit5[[i]]$lambda[which.min(cvfit5[[i]]$cve)])
  ##一般方法
  ##logistic
  #fit6<-glm(Y~.,family=binomial,data=trainData)
  #fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1,lambda = cvfit6$lambda.min) 
  #result1[[i]]=predict(fit1[[i]],as.matrix(testData[[i]][,-1]),type="class")
  result2[[i]]=predict(fit2[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result3[[i]]=predict(fit3[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result4[[i]]=predict(fit4[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result5[[i]]=predict(fit5[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result6[[i]]<-ifelse(predict(fit6,testData,type="response") > 0.51,1,0)
  #result7[[i]]<-predict(fit7,as.matrix(testData[,-1]),type="class")
  ###计算准确率
  accuracy2[[i]]=mean(testData[[i]][,1]==result2[[i]])
  #accuracy3[[i]]=mean(testData[[i]][,1]==result3[[i]])
  #accuracy4[[i]]=mean(testData[[i]][,1]==result4[[i]])
  #accuracy5[[i]]=mean(testData[[i]][,1]==result5[[i]])
  #accuracy6[[i]]=mean(testData[,1]==result6[[i]])
  #accuracy7[[i]]=mean(testData[,1]==result7[[i]])
}
#######################
######grLasso##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit3[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial")
  #cvfit4[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial")
  #cvfit5[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial")
  #cvfit6<-cv.glmnet(as.matrix(trainData[,-1]), trainData[,1], family="binomial",alpha=1)
  ##组变量方法
  fit3[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial",lambda=cvfit3[[i]]$lambda[which.min(cvfit3[[i]]$cve)])
  #fit4[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial",lambda=cvfit4[[i]]$lambda[which.min(cvfit4[[i]]$cve)])
  #fit5[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial",lambda=cvfit5[[i]]$lambda[which.min(cvfit5[[i]]$cve)])
  ##一般方法
  ##logistic
  #fit6<-glm(Y~.,family=binomial,data=trainData)
  #fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1,lambda = cvfit6$lambda.min) 
  result3[[i]]=predict(fit3[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result4[[i]]=predict(fit4[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result5[[i]]=predict(fit5[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result6[[i]]<-ifelse(predict(fit6,testData,type="response") > 0.51,1,0)
  #result7[[i]]<-predict(fit7,as.matrix(testData[,-1]),type="class")
  ###计算准确率
  accuracy3[[i]]=mean(testData[[i]][,1]==result3[[i]])
  #accuracy4[[i]]=mean(testData[[i]][,1]==result4[[i]])
  #accuracy5[[i]]=mean(testData[[i]][,1]==result5[[i]])
  #accuracy6[[i]]=mean(testData[,1]==result6[[i]])
  #accuracy7[[i]]=mean(testData[,1]==result7[[i]])
}
#######################
######grSCAD##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit4[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial")
  #cvfit5[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial")
  #cvfit6<-cv.glmnet(as.matrix(trainData[,-1]), trainData[,1], family="binomial",alpha=1)
  ##组变量方法
  fit4[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial",lambda=cvfit4[[i]]$lambda[which.min(cvfit4[[i]]$cve)])
  #fit5[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial",lambda=cvfit5[[i]]$lambda[which.min(cvfit5[[i]]$cve)])
  ##一般方法
  ##logistic
  #fit6<-glm(Y~.,family=binomial,data=trainData)
  #fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1,lambda = cvfit6$lambda.min) 
  result4[[i]]=predict(fit4[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result5[[i]]=predict(fit5[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result6[[i]]<-ifelse(predict(fit6,testData,type="response") > 0.51,1,0)
  #result7[[i]]<-predict(fit7,as.matrix(testData[,-1]),type="class")
  ###计算准确率
  accuracy4[[i]]=mean(testData[[i]][,1]==result4[[i]])
  #accuracy5[[i]]=mean(testData[[i]][,1]==result5[[i]])
  #accuracy6[[i]]=mean(testData[,1]==result6[[i]])
  #accuracy7[[i]]=mean(testData[,1]==result7[[i]])
}
#######################
######cMCP##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit5[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial")
  #cvfit6<-cv.glmnet(as.matrix(trainData[,-1]), trainData[,1], family="binomial",alpha=1)
  ##组变量方法
  fit5[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial",lambda=cvfit5[[i]]$lambda[which.min(cvfit5[[i]]$cve)])
  ##一般方法
  ##logistic
  #fit6<-glm(Y~.,family=binomial,data=trainData)
  #fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1,lambda = cvfit6$lambda.min) 
  result5[[i]]=predict(fit5[[i]],as.matrix(testData[[i]][,-1]),type="class")
  #result6[[i]]<-ifelse(predict(fit6,testData,type="response") > 0.51,1,0)
  #result7[[i]]<-predict(fit7,as.matrix(testData[,-1]),type="class")
  ###计算准确率
  accuracy5[[i]]=mean(testData[[i]][,1]==result5[[i]])
  #accuracy6[[i]]=mean(testData[,1]==result6[[i]])
  #accuracy7[[i]]=mean(testData[,1]==result7[[i]])
}
#######################
######logistics##########
###############################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  # cvfit6[[i]]<-cv.glmnet(as.matrix(trainData[[i]][,-1]), trainData[,1], family="binomial",alpha=1)
  ##组变量方法
  ##一般方法
  ##logistic
  fit6[[i]]<-glm(Y~.,family=binomial,data=trainData[[i]])
  #fit7<-glmnet(as.matrix(trainData[,-1]), trainData[,1],family="binomial",alpha = 1,lambda = cvfit6$lambda.min) 
  result6[[i]]<-ifelse(predict(fit6[[i]],testData[[i]],type="response") > 0.51,1,0)
  #result7[[i]]<-predict(fit7,as.matrix(testData[,-1]),type="class")
  ###计算准确率
  accuracy6[[i]]=mean(testData[[i]][,1]==result6[[i]])
  #accuracy7[[i]]=mean(testData[,1]==result7[[i]])
}

#######################
######logistics-lasso##########
###############################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit6[[i]]<-cv.glmnet(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], family="binomial",alpha=1)
  ##组变量方法
  ##一般方法
  ##logistic
  fit7[[i]]<-glmnet(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1],family="binomial",alpha = 1,lambda = cvfit6[[i]]$lambda.min) 
  result7[[i]]<-predict(fit7[[i]],as.matrix(testData[[i]][,-1]),type="class")
  ###计算准确率
  accuracy7[[i]]=mean(testData[[i]][,1]==result7[[i]])
}



##计算各类方法得出的预测准确率均值
maccuracy1=mean(as.numeric(as.character(unlist(accuracy1))))
maccuracy2=mean(as.numeric(as.character(unlist(accuracy2))))
maccuracy3=mean(as.numeric(as.character(unlist(accuracy3))))
maccuracy4=mean(as.numeric(as.character(unlist(accuracy4))))
maccuracy5=mean(as.numeric(as.character(unlist(accuracy5))))
maccuracy6=mean(as.numeric(as.character(unlist(accuracy6))))
maccuracy7=mean(as.numeric(as.character(unlist(accuracy7))))

data.frame(methods=c("grMCP","gel","grLasso","grSCAD","cMCP","logistics","logistics-lasso"),meanofaccuracy=c(maccuracy1,maccuracy2,maccuracy3,maccuracy4,maccuracy5,maccuracy6,maccuracy7))

