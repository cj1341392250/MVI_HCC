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
df <- read.csv('Task 2.csv')
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
  dplyr::filter(trainAUC>0.70&testAUC>0.70,trainAUC>testAUC) %>% 
  arrange(desc(trainAUC),desc(testAUC))

#seed <- seed$seed[1];seed
save(Sample,seed,file = 'model2_sample.Rdata')
#---Established Model------
rm(list = ls())
library(dplyr)
library(pROC)
library(caret)
load('model2_sample.Rdata')
samples <- Sample[[1228]] #585 #483 #1763
set.seed(1228)
if(T){
df <- read.csv('Task 2.csv')
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
AP_model<-glm(label~.,data=train_AP,family = binomial,)
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
A1_model<-glm(label~.,data=train_Clin,family = binomial)
train_Clin_pre<-predict(A1_model,newdata = train_Clin)
train_Clin_roc<-roc(train_Clin$label,train_Clin_pre,ci=T)
best=coords(train_Clin_roc,'best',ret='all',transpose=F)
test_Clin_pre<-predict(A1_model,newdata = test_Clin,terms = 'response') 
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
train_B1 <- df %>% 
  select(label,contains('_original_')) 
train_B1 <- train_B1[samples,]
test_B1<-df %>% 
  select(label,contains('_original_'))
test_B1 <- test_B1[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_B1,method= "lm",  trControl=train.control)
B1_model<-glm(label~.,data=train_B1,family = binomial)
train_B1_pre<-predict(B1_model,newdata = train_B1)
train_B1_roc<-roc(train_B1$label,train_B1_pre,ci=T)
best=coords(train_B1_roc,'best',ret='all',transpose=F)
test_B1_pre<-predict(B1_model,newdata = test_B1,terms = 'response') 
test_B1_roc<-roc(test_B1$label,test_B1_pre,ci=T)
best_2=coords(test_B1_roc,'best',ret='all',transpose=F)
B1_model_train<-data.frame(
  Type='B1_model_train',
  AUC=round(train_B1_roc$auc,3),
  '95% CI'=paste(round(train_B1_roc$ci[c(1,3)],3),collapse =  '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
B1_model_test<-data.frame(
  Type='B1_model_test',
  AUC=round(test_B1_roc$auc,3),
  '95% CI'=paste(round(test_B1_roc$ci[c(1,3)],3),collapse =  '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)

#Combined_model----
train_C1 <- df[samples,]
test_C1<-df[-samples,]
train.control<-trainControl(method= "repeatedcv",  number= 10,repeats= 3)
model<-train(label~.,data=train_C1,method= "lm",  trControl=train.control)
C1_model<-glm(label~.,data=train_C1,family = binomial)
train_C1_pre<-predict(C1_model,newdata = train_C1)
train_C1_roc<-roc(train_C1$label,train_C1_pre,ci=T)
best=coords(train_C1_roc,'best',ret='all',transpose=F)
test_C1_pre<-predict(C1_model,newdata = test_C1,terms = 'response') 
test_C1_roc<-roc(test_C1$label,test_C1_pre,ci=T)
best_2=coords(test_C1_roc,'best',ret='all',transpose=F)
C1_model_train<-data.frame(
  Type='C1_model_train',
  AUC=round(train_C1_roc$auc,3),
  '95% CI'=paste(round(train_C1_roc$ci[c(1,3)],3),collapse =  '-'),
  'Cutoff value'=round(best$threshold,3),
  spensitivity=round(best$specificity,3),
  sensitivity=round(best$sensitivity,3),
  accuracy=round(best$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
C1_model_test<-data.frame(
  Type='C1_model_test',
  AUC=round(test_C1_roc$auc,3),
  '95% CI'=paste(round(test_C1_roc$ci[c(1,3)],3),collapse = '-'),
  'Cutoff value'=round(best_2$threshold,3),
  spensitivity=round(best_2$specificity,3),
  sensitivity=round(best_2$sensitivity,3),
  accuracy=round(best_2$accuracy,3),
  RMSE=model$results[,2],
  Rsquared=model$results[,3],
  MAE=model$results[,4],
  check.names = F)
#Merged----
Combined_df<-rbind(AP_model_train,AP_model_test,
                   PVP_model_train,PVP_model_test,
                   T2_model_train,T2_model_test,
                   Clin_model_train,Clin_model_test,
                   B1_model_train,B1_model_test,
                   C1_model_train,C1_model_test)
fwrite(Combined_df,file = 'combined_df_model2.csv')
library(maxLik)
#C1_likehood_test
predictions <- list(
  modelAP1=AP_model,
  modelPVP1=PVP_model,
  ModelT2=T2_model,
  ModelB1=B1_model,
  ModelA1=A1_model
)
comparison_results <- data.frame(model1 = character(),
                                 model2 = character(),
                                 p.value = numeric(),
                                 stringsAsFactors = FALSE)
for (i in 1:5) {
  pred1 <-  C1_model
  pred2 <- predictions[[i]]
  pvalue <- lrtest(pred1, pred2)$stats[3]
  comparison_results[nrow(comparison_results) + 1,] <- c('Model C1', 
                                                         names(predictions)[i], 
                                                         pvalue)
  
}
#B1_likehood_test
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
  pred1 <-  B1_model
  pred2 <- predictions[[i]]
  pvalue <- lrtest(pred1, pred2)$stats[3]
  comparison_results[nrow(comparison_results) + 1,] <- c('Model B1', 
                                                         names(predictions)[i], 
                                                         pvalue)
  
}
comparison_results1 <- rbind(comparison_results1,comparison_results)
data.table::fwrite(comparison_results1,file = 'model2_likehold.csv')


#---Train_AUC----
pdf(file = 'Model2_Train_ROC.pdf',width =6,height = 5)
plot(train_AP_roc, col="#1C9A35", lwd=2, title = "")
plot(train_PVP_roc, col="#1B4E8C", lwd=2, add = T)
plot(train_T2_roc, col="#EDB85D", lwd=2, add = T)
plot(train_B1_roc, col="#6595A3", lwd=2, add = T)
plot(train_Clin_roc, col="#555555", lwd=2, add = T)
plot(train_C1_roc, col="#E41A1C", lwd=2, add = T)
legend("bottomright",
       c(paste0("Model AP2 AUC=",round(train_AP_roc$auc,3)), 
         paste0("Model PVP2 AUC=",round(train_PVP_roc$auc,3)), 
         paste0("Model T2-2 AUC=",round(train_T2_roc$auc,3)),
         paste0("Model B2 AUC=",round(train_B1_roc$auc,3)), 
         paste0("Model A2 AUC=",round(train_Clin_roc$auc,3)), 
         paste0("Model C2 AUC=",round(train_C1_roc$auc,3))
       ),
       col=c("#1C9A35", "#1B4E8C", "#EDB85D", "#6595A3","#555555","#E41A1C"),
       lty=1, lwd=3,bty = "n"
)
mtext("ROC curves of given models", line=2)
dev.off()
#---Test_AUC----
pdf(file = 'Model2_Test_ROC.pdf',width = 6,height = 5)
plot(test_AP_roc, col="#1C9A35", lwd=2, title = "")
plot(test_T2_roc, col="#1B4E8C", lwd=2, add = T)
plot(test_PVP_roc, col="#EDB85D", lwd=2, add = T)
plot(test_B1_roc, col="#6595A3", lwd=2, add = T)
plot(test_Clin_roc, col="#555555", lwd=2, add = T)
plot(test_C1_roc, col="#E41A1C", lwd=2, add = T)
legend("bottomright",
       c(paste0("Model AP2 AUC=",round(test_AP_roc$auc,3)), 
         paste0("Model PVP2 AUC=",round(test_PVP_roc$auc,3)), 
         paste0("Model T2-2 AUC=",round(test_T2_roc$auc,3)),
         paste0("Model B2 AUC=",round(test_B1_roc$auc,3)), 
         paste0("Model A2 AUC=",round(test_Clin_roc$auc,3)), 
         paste0("Model C2 AUC=",round(test_C1_roc$auc,3))
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
library(rms)
dd<-datadist(train_C1)
options(datadist='dd')
dca12 <- ggDCA::dca(C1_model,B1_model,A1_model,AP_model,PVP_model,T2_model)
dca12$model <- case_when(dca12$model=='AP_model'~'Model AP2',
                         dca12$model=='PVP_model'~'Model PVP2',
                         dca12$model=='T2_model'~'Model T2-2',
                         dca12$model=='B1_model'~'Model B2',
                         dca12$model=='A1_model'~'Model A2',
                         dca12$model=='C1_model'~'Model C2')
dca_df <- as.data.frame(dca12)
dca_df$model <- as.character(dca_df$model)
dca_df$model[dca_df$model=='AP_model']='Model AP2' 
dca_df$model[dca_df$model=='PVP_model']='Model PVP2' 
dca_df$model[dca_df$model=='T2_model']='Model T2-2' 
dca_df$model[dca_df$model=='B1_model']='Model B2' 
dca_df$model[dca_df$model=='A1_model']='Model A2' 
dca_df$model[dca_df$model=='C1_model']='Model C2' 
#dca_df$NB[dca_df$NB>0&dca_df$model=='All']=-1
ggplot(data = dca12, aes(x = thresholds, y = NB)) +
  geom_point(colour='white',aes(color = model)) +
  scale_color_manual(values = c("#1C9A35", "#1B4E8C", "#EDB85D",
                                         "#6595A3","#555555","#E41A1C",
                                         "#348AA6FF","#A0DFB9FF"),
                     limits = c('Model AP2','Model PVP2','Model T2-2',
                                'Model B2','Model A2','Model C2',
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
  coord_cartesian(ylim =c(-0.15,0.5), xlim =c(0,1))

ggplot(dca12,linetype = F,
       theme(legend.background=element_rect()))+
  scale_color_manual(values = c("#1C9A35", "#1B4E8C", "#EDB85D",
                                "#6595A3","#555555","#E41A1C",
                                "#348AA6FF","#A0DFB9FF"),
                     limits = c('Model AP2','Model PVP2','Model T2-2',
                                'Model B2','Model A2','Model C2',
                                'All','None')) +
  theme_bw() +
  theme(legend.position = c(0.8,0.8),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(vjust = 0,hjust = 0.5),
        axis.title = element_text(face = 'plain',
                                  colour = 'black',size = 18),
        axis.text = element_text(face = 'plain',
                                 colour = 'black',size = 12)) +
  labs(title = '',x = 'Probability Threshold', y = 'Net Benefit')+
  coord_cartesian(ylim =c(-0.15,0.5), xlim =c(0,1))

ggsave(filename = 'Model2_DCA.pdf',height = 5,width = 6)
dev.off()

#---ca
library(rms)
dd<-datadist(train_C1)
options(datadist='dd')
fit<-lrm(label~.,data=train_C1,x=T,y=T)
cal2 <- calibrate(fit, method = 'boot', B = 1000)
pdf(file = 'Model2_calibrate_curve.pdf',width = 5,height = 5)
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
