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
  ##组变量方法
  fit1[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grMCP", family="binomial",lambda=cvfit1[[i]]$lambda[which.min(cvfit1[[i]]$cve)])
  result1[[i]]=predict(fit1[[i]],as.matrix(testData[[i]][,-1]),type="class")
  ###计算准确率
  accuracy1[[i]]=mean(testData[[i]][,1]==result1[[i]])
}
#######################
##########gel##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit2[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="gel", family="binomial")
  fit2[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="gel", family="binomial",lambda=cvfit2[[i]]$lambda[which.min(cvfit2[[i]]$cve)])
  result2[[i]]=predict(fit2[[i]],as.matrix(testData[[i]][,-1]),type="class")
  accuracy2[[i]]=mean(testData[[i]][,1]==result2[[i]])
}
#######################
######grLasso##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit3[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial")
  fit3[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grLasso", family="binomial",lambda=cvfit3[[i]]$lambda[which.min(cvfit3[[i]]$cve)])
  result3[[i]]=predict(fit3[[i]],as.matrix(testData[[i]][,-1]),type="class")
  accuracy3[[i]]=mean(testData[[i]][,1]==result3[[i]])
}
#######################
######grSCAD##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit4[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial")
  fit4[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="grSCAD", family="binomial",lambda=cvfit4[[i]]$lambda[which.min(cvfit4[[i]]$cve)])
  result4[[i]]=predict(fit4[[i]],as.matrix(testData[[i]][,-1]),type="class")
  accuracy4[[i]]=mean(testData[[i]][,1]==result4[[i]])
}
#######################
######cMCP##########
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit5[[i]]<-cv.grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial")
  fit5[[i]]<-grpreg(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], group, penalty="cMCP", family="binomial",lambda=cvfit5[[i]]$lambda[which.min(cvfit5[[i]]$cve)])
  result5[[i]]=predict(fit5[[i]],as.matrix(testData[[i]][,-1]),type="class")
  accuracy5[[i]]=mean(testData[[i]][,1]==result5[[i]])
}
#######################
######logistics##########
###############################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  fit6[[i]]<-glm(Y~.,family=binomial,data=trainData[[i]])
  result6[[i]]<-ifelse(predict(fit6[[i]],testData[[i]],type="response") > 0.51,1,0)
  accuracy6[[i]]=mean(testData[[i]][,1]==result6[[i]])
}
#######################
######logistics-lasso##
#######################
for(i in 1:100){
  ht[[i]]<-dt[sample(nrow(dt)),]
  testData[[i]]<-ht[[i]][testIndexes, ]
  trainData[[i]]<-ht[[i]][-testIndexes, ]
  cvfit6[[i]]<-cv.glmnet(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1], family="binomial",alpha=1)
  fit7[[i]]<-glmnet(as.matrix(trainData[[i]][,-1]), trainData[[i]][,1],family="binomial",alpha = 1,lambda = cvfit6[[i]]$lambda.min) 
  result7[[i]]<-predict(fit7[[i]],as.matrix(testData[[i]][,-1]),type="class")
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
###list result 
data.frame(methods=c("grMCP","gel","grLasso","grSCAD","cMCP","logistics","logistics-lasso"),meanofaccuracy=c(maccuracy1,maccuracy2,maccuracy3,maccuracy4,maccuracy5,maccuracy6,maccuracy7))

############
#参数估计###
############
cvfit.gel<-cv.grpreg(scale.X,Y,group,penalty="gel", family="binomial",tau)
fit.gel<-grpreg(scale.X,Y, group, penalty="gel", family="binomial",lambda=cvfit.gel$lambda[which.min(cvfit.gel$cve)])
#fit.gel<-grpreg(scale.X,Y, group, penalty="gel", family="binomial")

plot(cvfit.gel)
par(mfrow=c(2,2)) 
plot(cvfit.cmcp, type="all")
plot(fit.gel)
select(fit.gel, "BIC")

cvfit.cmcp<-cv.grpreg(scale.X,Y,group,penalty="cMCP", family="binomial")
fit.cmcp<-grpreg(scale.X,Y, group, penalty="cMCP", family="binomial",lambda=cvfit.cmcp$lambda[which.min(cvfit.cmcp$cve)])
plot(cvfit.cmcp)
par(mfrow=c(2,2)) 
plot(cvfit.cmcp, type="all")
plot(fit.cmcp)
select(fit.cmcp,"BIC")


cvfit.grLasso<-cv.grpreg(scale.X,Y,group,penalty="grLasso", family="binomial")
fit.grLasso<-grpreg(scale.X,Y, group, penalty="grLasso", family="binomial",lambda=cvfit.cmcp$lambda[which.min(cvfit.cmcp$cve)])
plot(cvfit.grLasso)
par(mfrow=c(2,2)) 
plot(cvfit.grLasso, type="all")
plot(fit.grLasso)
select(fit.grLasso,"BIC")

# Plot group norms, with labels in right margin 
plot(fit.grLasso, norm=TRUE, label=TRUE)
select(fit.grLasso) 
select(fit.grLasso,crit="AIC",df="active") 
plot(fit.grLasso) 
abline(v=select(fit.grLasso)$lambda) 
par(mfrow=c(1,3)) 
l <- fit.grLasso$lambda 
xlim <- rev(range(l)) 
plot(l, select(fit.grLasso)$IC, xlim=xlim, pch=19, type="o", ylab="BIC") 
plot(l, select(fit.grLasso,"AIC")$IC, xlim=xlim, pch=19, type="o",ylab="AIC") 
plot(l, select(fit.grLasso,"GCV")$IC, xlim=xlim, pch=19, type="o",ylab="GCV")
