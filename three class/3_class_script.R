setwd("G:/R/project/project6")
library(nnet)
library(data.table)
require(questionr)
require(DALEX)
require(iBreakDown)
require(caret)
require(glmnet)
library(pROC)
train<-read.csv('3_class_input_train.csv')

test<-read.csv('3_class_input_test.csv')
colnames(train)
#event=1
rt<-train
rt$label<-as.numeric(rt$label)
rt$label[rt$label==2]=0
rt$label<-factor(rt$label)
fit.full<-glm(label~.,data=rt,family = binomial)
fit.result<-summary(fit.full)
train_logstic<-round(odds.ratio(fit.full),2)
fwrite(train_logstic,file = "Logstic_OR_1.csv",
          quote = F,row.names = T) #多因素logstic分析结果保存

#----2.计算ROC值------
library(pROC)
#---label=1---
rt<-train
rt$label<-as.numeric(rt$label)
rt$label[rt$label==2]=0
rt$label<-factor(rt$label)
fit.full<-glm(label~.,data=rt,family = binomial)

train_pre<-predict(fit.full,newdata = rt)
train_roc<-roc(rt$label,train_pre,ci=T)
best=coords(train_roc,'best',ret='all',transpose=F)

rt2<-test
rt2$label[rt2$label==2]=0
test_pre<-predict(fit.full,newdata = test,terms = 'response') 
test_roc<-roc(rt2$label,test_pre,ci=T)
best_2=coords(test_roc,'best',ret='all',transpose=F)
roc1_train<- roc(rt$label,train_pre,ci=T)
roc1_test<-roc(rt2$label,test_pre,ci=T)
outTab<-data.frame(
  label=c('train_1'),
  AUC=round(train_roc$auc,3),
  '95% CI'=paste(round(train_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  check.names = F)
outTab2<-data.frame(
  label=c('test_1'),
  AUC=round(test_roc$auc,3),
  '95% CI'=paste(round(test_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  check.names = F)
#--label=2---
rt<-train
rt$label<-as.numeric(rt$label)
rt$label[rt$label==1]=0
rt$label<-factor(rt$label)
fit.full<-glm(label~.,data=rt,family = binomial)

train_pre<-predict(fit.full,newdata = rt)
train_roc<-roc(rt$label,train_pre,ci=T)
best=coords(train_roc,'best',ret='all',transpose=F)

rt2<-test
rt2$label[rt2$label==1]=0
test_pre<-predict(fit.full,newdata = test) 
test_roc<-roc(rt2$label,test_pre,ci=T)
best_2=coords(test_roc,'best',ret='all',transpose=F)
roc2_train<- roc(rt$label,train_pre,ci=T)
roc2_test<-roc(rt2$label,test_pre,ci=T)
outTab3<-data.frame(
  label=c('train_2'),
  AUC=round(train_roc$auc,3),
  '95% CI'=paste(round(train_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  check.names = F)
outTab4<-data.frame(
  label=c('test_2'),
  AUC=round(test_roc$auc,3),
  '95% CI'=paste(round(test_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  check.names = F)
#----label=0---
rt<-train
rt$label<-as.numeric(rt$label)
rt$label[rt$label==2]=1
rt$label<-factor(rt$label,levels = c(1,0))

fit.full<-glm(label~.,data=rt,family = binomial)

train_pre<-predict(fit.full,newdata = rt)
train_roc<-roc(rt$label,train_pre,ci=T)
best=coords(train_roc,'best',ret='all',transpose=F)

rt2<-test
rt2$label[rt2$label==2]=1
rt2$label<-factor(rt2$label,levels = c(1,0))

test_pre<-predict(fit.full,newdata = test,terms = 'response') 
test_roc<-roc(rt2$label,test_pre,ci=T)
best_2=coords(test_roc,'best',ret='all',transpose=F)
roc3_train<- roc(rt$label,train_pre,ci=T)
roc3_test<-roc(rt2$label,test_pre,ci=T)
outTab5<-data.frame(
  label=c('train_0'),
  AUC=round(train_roc$auc,3),
  '95% CI'=paste(round(train_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  check.names = F)
outTab6<-data.frame(
  label=c('test_0'),
  AUC=round(test_roc$auc,3),
  '95% CI'=paste(round(test_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  check.names = F)
whole_table<-rbind(outTab,outTab2,outTab3,outTab4,outTab5,outTab6)


fwrite(whole_table,'Logstic_ROC.csv')


save(roc1_test,roc1_train,roc2_test,roc2_train,roc3_test,roc3_train,file = 'ROCplot.Rdata')
#----3.绘图----
rm(list = ls())
load('ROCplot.Rdata')
pdf('ROC_train.pdf',width = 5,height = 5)
plot.roc(roc3_train,col='red',identity.col = 'grey',legacy.axes = T)
plot.roc(roc1_train,col='green',legacy.axes = T,add = T)
plot.roc(roc2_train,col='blue',add = T,legacy.axes = T)
legend('bottomright',c('0 vs Rest (AUC=0.802)','1 vs Rest (AUC=0.706)','2 vs Rest (AUC=0.894)'),col = c('red','green','blue'),lwd=2,bty='n')
dev.off()

pdf('ROC_test.pdf',width = 5,height = 5)
plot.roc(roc3_test,col='red',identity.col = 'grey',legacy.axes = T)
plot.roc(roc1_test,col='green',legacy.axes = T,add = T)
plot.roc(roc2_test,col='blue',add = T,legacy.axes = T)
legend('bottomright',c('0 vs Rest (AUC=0.762)','1 vs Rest (AUC=0.635)','2 vs Rest (AUC=0.784)'),col = c('red','green','blue'),lwd=2,bty='n')
dev.off()

#---4.delong test----
test_12<-roc.test(roc1_test,roc2_test,method='delong')
test_02<-roc.test(roc3_test,roc2_test,method='delong')
test_01<-roc.test(roc1_test,roc3_test,method='delong')

train_12<-roc.test(roc1_train,roc2_train,method='delong')
train_02<-roc.test(roc3_train,roc2_train,method='delong')
train_01<-roc.test(roc1_train,roc3_trai,method='delong')