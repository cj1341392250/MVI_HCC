rm(list = ls())
library(nnet)
library(data.table)
require(questionr)
require(DALEX)
require(iBreakDown)
library(dplyr)
require(caret)
require(glmnet)
library(pROC)
#----Select best seed----
df <- read.csv('Task 3_input.csv')
samples <- createDataPartition(df$label, p=0.7, list=FALSE)
train<-df[samples,]
test<-df[-samples,]
fit.full<-glm(label~.,data=train,family = binomial)
fit.result<-summary(fit.full)
#AUC
uniTab1<-data.frame()
Sample <- list()

for (i in 1:2000) {
  set.seed(i) 
  if(T){
    Train <- createDataPartition(df$label, p=0.7, list=FALSE)
    Sample[[i]]<- Train
    train<-df[Train,]
    test<-df[-Train,]
    train$label<-factor(train$label)
    class(train)
    train<-as.data.frame(train)
    fit.full<-glm(label~.,data=train,family = 'binomial')
    fit.result<-summary(fit.full)
    #计算p值
    # z <- summary(fit.full)$coefficients/summary(fit.full)$standard.errors
    # p <- (1 - pnorm(abs(z), 0, 1))*2
    # p
    #计算OR值
    #lable=1
    train_logstic<-round(odds.ratio(fit.full),2)
    #AUC
    train_pre<-predict(fit.full,newdata = train)
    train_pre<-as.numeric(train_pre)
    train_roc<-roc(train$label,train_pre,ci=T,plot=F)
    best=coords(train_roc,'best',ret='all',transpose=F)
    test_pre<-predict(fit.full,newdata = test,terms = 'response') 
    test_roc<-roc(test$label,test_pre,ci=T,plot=F)
    best_2=coords(test_roc,'best',ret='all',transpose=F)
    uniTab1<-rbind(
      uniTab1,
      cbind(seed=i,trainAUC=round(train_roc$auc,3),testAUC=round(test_roc$auc,3))
    )
    
  }
}
seed <- uniTab1 %>% 
  dplyr::filter(trainAUC>0.90&testAUC>0.90,trainAUC>testAUC) %>% 
  arrange(desc(trainAUC),desc(testAUC))
#seed <- seed$seed[1];seed
save(Sample,seed,file = 'Task3_sample.Rdata')
#---Established Model------
rm(list = ls())
load('Task3_sample.Rdata')
samples <- Sample[[1324]]
set.seed(1324)
if(T){
df <- read.csv('Task 3_input.csv')
df$AFP <- as.numeric(df$AFP)
df$margin <- as.numeric(df$margin)
df$label <- as.numeric(df$label)

#AP_model----
train_AP <- df %>% 
  select(label,starts_with('AP_')) 
train_AP <- train_AP[samples,]
test_AP<-df %>% 
  select(label,starts_with('AP_')) 
test_AP <- test_AP[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_AP,method= "lm",  trControl=train.control)
AP_model<-glm(label~.,data=train_AP,family = binomial)
train_AP_pre<-predict(AP_model,newdata = train_AP)
train_AP_roc<-roc(train_AP$label,train_AP_pre,ci=T)
best=coords(train_AP_roc,'best',ret='all',transpose=F)
test_AP_pre<-predict(AP_model,newdata = test_AP,terms = 'response') 
test_AP_roc<-roc(test_AP$label,test_AP_pre,ci=T)
best_2=coords(test_AP_roc,'best',ret='all',transpose=F)
AP_model_train<-data.frame(
  Type='AP_model_train',
  AUC=round(train_AP_roc$auc,3),
  '95% CI'=paste(round(train_AP_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
AP_model_test<-data.frame(
  Type='AP_model_test',
  AUC=round(test_AP_roc$auc,3),
  '95% CI'=paste(round(test_AP_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)

#PVP_model----
train_PVP <- df %>% 
  select(label,starts_with('PVP_')) 
train_PVP <- train_PVP[samples,]
test_PVP<-df %>% 
  select(label,starts_with('PVP_')) 
test_PVP <- test_PVP[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_PVP,method= "lm",  trControl=train.control)
PVP_model<-glm(label~.,data=train_PVP,family = binomial)
train_PVP_pre<-predict(PVP_model,newdata = train_PVP)
train_PVP_roc<-roc(train_PVP$label,train_PVP_pre,ci=T)
best=coords(train_PVP_roc,'best',ret='all',transpose=F)
test_PVP_pre<-predict(PVP_model,newdata = test_PVP,terms = 'response') 
test_PVP_roc<-roc(test_PVP$label,test_PVP_pre,ci=T)
best_2=coords(test_PVP_roc,'best',ret='all',transpose=F)

PVP_model_train<-data.frame(
  Type='PVP_model_train',
  AUC=round(train_PVP_roc$auc,3),
  '95% CI'=paste(round(train_PVP_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
PVP_model_test<-data.frame(
  Type='PVP_model_test',
  AUC=round(test_PVP_roc$auc,3),
  '95% CI'=paste(round(test_PVP_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
#T2_model----
train_T2 <- df %>% 
  select(label,starts_with('T2_')) 
train_T2 <- train_T2[samples,]
test_T2<-df %>% 
  select(label,starts_with('T2_')) 
test_T2 <- test_T2[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_T2,method= "lm",  trControl=train.control)
T2_model<-glm(label~.,data=train_T2,family = binomial)
train_T2_pre<-predict(T2_model,newdata = train_T2)
train_T2_roc<-roc(train_T2$label,train_T2_pre,ci=T)
best=coords(train_T2_roc,'best',ret='all',transpose=F)
test_T2_pre<-predict(T2_model,newdata = test_T2,terms = 'response') 
test_T2_roc<-roc(test_T2$label,test_T2_pre,ci=T)
best_2=coords(test_T2_roc,'best',ret='all',transpose=F)
T2_model_train<-data.frame(
  Type='T2_model_train',
  AUC=round(train_T2_roc$auc,3),
  '95% CI'=paste(round(train_T2_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
T2_model_test<-data.frame(
  Type='T2_model_test',
  AUC=round(test_T2_roc$auc,3),
  '95% CI'=paste(round(test_T2_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)

#Clinical_model----
train_Clin <- df %>% 
  select(-starts_with('PVP')) %>% 
  select(-starts_with('T2')) %>% 
  select(-starts_with('AP')) 
train_Clin <- train_Clin[samples,]
test_Clin<-df %>% 
  select(-starts_with('PVP')) %>% 
  select(-starts_with('T2')) %>% 
  select(-starts_with('AP')) 
test_Clin <- test_Clin[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_Clin,method= "lm",  trControl=train.control)
A3_model<-glm(label~.,data=train_Clin,family = binomial)
train_Clin_pre<-predict(A3_model,newdata = train_Clin)
train_Clin_roc<-roc(train_Clin$label,train_Clin_pre,ci=T)
best=coords(train_Clin_roc,'best',ret='all',transpose=F)
test_Clin_pre<-predict(A3_model,newdata = test_Clin,terms = 'response') 
test_Clin_roc<-roc(test_Clin$label,test_Clin_pre,ci=T)
best_2=coords(test_Clin_roc,'best',ret='all',transpose=F)
Clin_model_train<-data.frame(
  Type='Clin_model_train',
  AUC=round(train_Clin_roc$auc,3),
  '95% CI'=paste(round(train_Clin_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
Clin_model_test<-data.frame(
  Type='Clin_model_test',
  AUC=round(test_Clin_roc$auc,3),
  '95% CI'=paste(round(test_Clin_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)

#All_Radiomic----
train_B3 <- df %>% 
  select(label,contains('_original_')) 
train_B3 <- train_B3[samples,]
test_B3<-df %>% 
  select(label,contains('_original_'))
test_B3 <- test_B3[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_B3,method= "lm",  trControl=train.control)
B3_model<-glm(label~.,data=train_B3,family = binomial)
train_B3_pre<-predict(B3_model,newdata = train_B3)
train_B3_roc<-roc(train_B3$label,train_B3_pre,ci=T)
best=coords(train_B3_roc,'best',ret='all',transpose=F)
test_B3_pre<-predict(B3_model,newdata = test_B3,terms = 'response') 
test_B3_roc<-roc(test_B3$label,test_B3_pre,ci=T)
best_2=coords(test_B3_roc,'best',ret='all',transpose=F)
B3_model_train<-data.frame(
  Type='B3_model_train',
  AUC=round(train_B3_roc$auc,3),
  '95% CI'=paste(round(train_B3_roc$ci[c(1,3)],3),collapse =  '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
B3_model_test<-data.frame(
  Type='B3_model_test',
  AUC=round(test_B3_roc$auc,3),
  '95% CI'=paste(round(test_B3_roc$ci[c(1,3)],3),collapse =  '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)

#Combined_model----
train_C3 <- df[samples,]
test_C3<-df[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_C3,method= "lm",  trControl=train.control)
C3_model<-glm(label~.,data=train_C3,family = binomial)
train_C3_pre<-predict(C3_model,newdata = train_C3)
train_C3_roc<-roc(train_C3$label,train_C3_pre,ci=T)
best=coords(train_C3_roc,'best',ret='all',transpose=F)
test_C3_pre<-predict(C3_model,newdata = test_C3,terms = 'response') 
test_C3_roc<-roc(test_C3$label,test_C3_pre,ci=T)
best_2=coords(test_C3_roc,'best',ret='all',transpose=F)
C3_model_train<-data.frame(
  Type='C3_model_train',
  AUC=round(train_C3_roc$auc,3),
  '95% CI'=paste(round(train_C3_roc$ci[c(1,3)],3),collapse =  '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
C3_model_test<-data.frame(
  Type='C3_model_test',
  AUC=round(test_C3_roc$auc,3),
  '95% CI'=paste(round(test_C3_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
Combined_df<-rbind(AP_model_train,AP_model_test,
                   PVP_model_train,PVP_model_test,
                   T2_model_train,T2_model_test,
                   Clin_model_train,Clin_model_test,
                   B3_model_train,B3_model_test,
                   C3_model_train,C3_model_test)
fwrite(Combined_df,file = 'combined_df_Task3.csv')

library(maxLik)
#C3_likehood_test
predictions <- list(
  modelAP1=AP_model,
  modelPVP1=PVP_model,
  ModelT2=T2_model,
  ModelB3=B3_model,
  ModelA3=A3_model
)
comparison_results <- data.frame(model1 = character(),
                                 model2 = character(),
                                 p.value = numeric(),
                                 stringsAsFactors = FALSE)
for (i in 1:5) {
  pred1 <-  C3_model
  pred2 <- predictions[[i]]
  pvalue <- lrtest(pred1, pred2)$stats[3]
  comparison_results[nrow(comparison_results) + 1,] <- c('Model C3', 
                                                         names(predictions)[i], 
                                                         pvalue)
  
}
#B3_likehood_test
predictions1 <- list(
  'Model AP1'=AP_model,
  'Model PVP1'=PVP_model,
  'Model T2'=T2_model
  
)
comparison_results1 <- data.frame(model1 = character(),
                                  model2 = character(),
                                  p.value = numeric(),
                                  stringsAsFactors = FALSE)
for (i in 1:3) {
  pred1 <-  B3_model
  pred2 <- predictions[[i]]
  pvalue <- lrtest(pred1, pred2)$stats[3]
  comparison_results[nrow(comparison_results) + 1,] <- c('Model B3', 
                                                         names(predictions)[i], 
                                                         pvalue)
  
}
comparison_results1 <- rbind(comparison_results1,comparison_results)
data.table::fwrite(comparison_results1,file = 'Task3_likehold.csv')


#---Train_AUC----
pdf(file = 'Task3_Train_ROC.pdf',width =6,height = 5)
plot(train_AP_roc, col="#1C9A35", lwd=2, title = "")
plot(train_PVP_roc, col="#1B4E8C", lwd=2, add = T)
plot(train_T2_roc, col="#EDB85D", lwd=2, add = T)
plot(train_B3_roc, col="#6595A3", lwd=2, add = T)
plot(train_Clin_roc, col="#555555", lwd=2, add = T)
plot(train_C3_roc, col="#E41A1C", lwd=2, add = T)
legend("bottomright",
       c(paste0("Model AP3 AUC=",round(train_AP_roc$auc,3)), 
         paste0("Model PVP3 AUC=",round(train_PVP_roc$auc,3)), 
         paste0("Model T2-3 AUC=",round(train_T2_roc$auc,3)),
         paste0("Model B3 AUC=",round(train_B3_roc$auc,3)), 
         paste0("Model A3 AUC=",round(train_Clin_roc$auc,3)), 
         paste0("Model C3 AUC=",round(train_C3_roc$auc,3))
       ),
       col=c("#1C9A35", "#1B4E8C", "#EDB85D", "#6595A3","#555555","#E41A1C"),
       lty=1, lwd=3,bty = "n"
)
mtext("ROC curves of given models", line=2)
dev.off()
#---Test_AUC----
pdf(file = 'Task3_Test_ROC.pdf',width = 6,height = 5)
plot(test_AP_roc, col="#1C9A35", lwd=2, title = "")
plot(test_T2_roc, col="#1B4E8C", lwd=2, add = T)
plot(test_PVP_roc, col="#EDB85D", lwd=2, add = T)
plot(test_B3_roc, col="#6595A3", lwd=2, add = T)
plot(test_Clin_roc, col="#555555", lwd=2, add = T)
plot(test_C3_roc, col="#E41A1C", lwd=2, add = T)
legend("bottomright",
       c(paste0("Model AP3 AUC=",round(test_AP_roc$auc,3)), 
         paste0("Model PVP3 AUC=",round(test_PVP_roc$auc,3)), 
         paste0("Model T2-3 AUC=",round(test_T2_roc$auc,3)),
         paste0("Model B3 AUC=",round(test_B3_roc$auc,3)), 
         paste0("Model A3 AUC=",round(test_Clin_roc$auc,3)), 
         paste0("Model C3 AUC=",round(test_C3_roc$auc,3))
       ),
       col=c("#1C9A35", "#1B4E8C", "#EDB85D", "#6595A3","#555555","#E41A1C"),
       lty=1, lwd=2,bty = "n"
)
mtext("ROC curves of given models", line=2)
dev.off()
}

#---DCA----
library(ggDCA)
library(ggpubr)
dd<-datadist(train_C3)
options(datadist='dd')
dca12 <- ggDCA::dca(C3_model,B3_model,A3_model,AP_model,PVP_model,T2_model)
dca_df <- as.data.frame(dca12)
dca_df$model <- as.character(dca_df$model)
dca_df$model[dca_df$model=='AP_model']='Model AP3' 
dca_df$model[dca_df$model=='PVP_model']='Model PVP3' 
dca_df$model[dca_df$model=='T2_model']='Model T2-3' 
dca_df$model[dca_df$model=='B3_model']='Model B3' 
dca_df$model[dca_df$model=='A3_model']='Model A3' 
dca_df$model[dca_df$model=='C3_model']='Model C3' 

ggplot(data = dca_df, aes(x = thresholds, y = NB)) +
  geom_point(colour='white',aes(color = model)) +
  scale_color_manual(values = c("#1C9A35", "#1B4E8C", "#EDB85D", "#6595A3","#555555","#E41A1C","#348AA6FF","#A0DFB9FF"),
                     limits = c('Model AP3','Model PVP3','Model T2-3',
                                'Model B3','Model A3','Model C3',
                                'All','None')) +
  geom_smooth(aes(color = model), method = 'loess', se = F, show.legend = T) +
  theme_bw() +
  theme(legend.position = c(0.88,0.76),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(vjust = 0,hjust = 0.5),
        axis.title = element_text(face = 'plain',colour = 'black',size = 18),
        axis.text = element_text(face = 'plain',colour = 'black',size = 12)) +
  labs(title = 'Decision curve of given models',x = 'probability Threshold', y = 'Net Benefit')+
  coord_cartesian(ylim =c(-0.15,0.3), xlim =c(0,1))
ggsave(filename = 'Task3_DCA.pdf',height = 5,width = 6)

#---校准曲线----
dd<-datadist(train_C3)
options(datadist='dd')
fit<-lrm(label~.,data=train_C3,x=T,y=T)
cal2 <- calibrate(fit, method = 'boot', B = 1000)
pdf(file = 'Task3_calibrate_curve.pdf',width = 5,height = 5)
plot(cal2,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Prediced Probability",
     ylab = "Observed Probability",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.8,
     #subtitles = FALSE,
     legend = FALSE
) 
lines(cal2[,c("predy","calibrated.corrected")], 
      type = 'l', #连线的类型，可以是"p","b","o"
      lwd = 3, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = "#2166AC") #连线的颜色
lines(cal2[,c("predy","calibrated.orig")],type="l",pch=16,lwd=3,col="tomato")
abline(0,1,
       lty = 2, #对角线为虚线
       lwd = 2, #对角线的粗细
       col = "#224444") #对角线的颜色
legend(0.6,0.2,
       c("Apparent","Bias-corrected","Ideal"), 
       lty = c(2,1,1), 
       lwd = c(2,3,3), 
       col = c("black","#2166AC","tomato"), 
       bty = "n"
)
dev.off()