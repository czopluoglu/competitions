
eval <- function(x,type){
  
  # x, 4 column matrix (gender, race, probability, model prediction, outcome)
  # type, brier.m, brier.f, brier.a, fair.m, fair.f
  # brier.m --> brier score for males
  # brier.f --> brier score for females
  # brier.a --> brier score for all
  # fair.m --> fairness score for males
  # fair.f --> fairness score for females
  
  if(type == 'brier.m'){
    x1  <- x[which(x$gender==0),]
    ret <- mean((x1$pred - x1$out)^2)
  }
  
  if(type == 'brier.f'){
    x2  <- x[which(x$gender==1),]
    ret <- mean((x2$pred - x2$out)^2)
  }
  
  if(type == 'brier.a'){
    ret <- mean((x$pred - x$out)^2)
  }
  
  if(type == 'fair.m'){
    
    x1  <- x[which(x$gender==0),]
    
    ret1 <- mean((x1$pred - x1$out)^2)
    
    x10 <- x1[which(x1$race==0),]
    
    conf.w1 <- matrix(nrow=2,ncol=2)
    conf.w1[1,1] = length(which(x10$out==0 & x10$pred2==0))
    conf.w1[1,2] = length(which(x10$out==0 & x10$pred2==1))
    conf.w1[2,1] = length(which(x10$out==1 & x10$pred2==0))
    conf.w1[2,2] = length(which(x10$out==1 & x10$pred2==1))
    fpw1    <- conf.w1[1,2]/sum(conf.w1[1,]) # false-positive rate for white
    
    x11 <- x1[which(x1$race==1),]
    
    conf.b1 <- matrix(nrow=2,ncol=2)
    conf.b1[1,1] = length(which(x11$out==0 & x11$pred2==0))
    conf.b1[1,2] = length(which(x11$out==0 & x11$pred2==1))
    conf.b1[2,1] = length(which(x11$out==1 & x11$pred2==0))
    conf.b1[2,2] = length(which(x11$out==1 & x11$pred2==1))
    fpb1    <- conf.b1[1,2]/sum(conf.b1[1,]) # false-positive rate for black
    
    ret <- (1-ret1)*(1- abs(fpb1 - fpw1))
    
  }
  
  if(type == 'fair.f'){
    
    x2  <- x[which(x$gender==1),]
    
    ret1 <- mean((x2$pred - x2$out)^2)
    
    x20 <- x2[which(x2$race==0),]
    
    conf.w1 <- matrix(nrow=2,ncol=2)
    conf.w1[1,1] = length(which(x20$out==0 & x20$pred2==0))
    conf.w1[1,2] = length(which(x20$out==0 & x20$pred2==1))
    conf.w1[2,1] = length(which(x20$out==1 & x20$pred2==0))
    conf.w1[2,2] = length(which(x20$out==1 & x20$pred2==1))
    fpw1    <- conf.w1[1,2]/sum(conf.w1[1,]) # false-positive rate for white
    
    x21 <- x2[which(x2$race==1),]
    
    conf.b1 <- matrix(nrow=2,ncol=2)
    conf.b1[1,1] = length(which(x21$out==0 & x21$pred2==0))
    conf.b1[1,2] = length(which(x21$out==0 & x21$pred2==1))
    conf.b1[2,1] = length(which(x21$out==1 & x21$pred2==0))
    conf.b1[2,2] = length(which(x21$out==1 & x21$pred2==1))
    fpb1    <- conf.b1[1,2]/sum(conf.b1[1,]) # false-positive rate for black
    
    ret <- (1-ret1)*(1- abs(fpb1 - fpw1))
    
  }
  
  ret
  
}

################################################################################

# Import full dataset

full <- read.csv(here('data/NIJ_s_Recidivism_Challenge_Full_Dataset.csv'))

out <- full[full$Training_Sample==0,c('ID','Gender','Race','Recidivism_Arrest_Year1','Recidivism_Arrest_Year2','Recidivism_Arrest_Year3')]

out$gender <- ifelse(out$Gender=='M',0,1)
out$race   <- ifelse(out$Race=='WHITE',0,1)
out        <- out[,-c(2,3)]


################################################################################
################################################################################
################################################################################
#
# Year 1 Predictions
#
################################################################################
################################################################################
################################################################################

#########   LOCAL TEST DATA ####################################################

m1 <- read.csv(here('data','test_model1.csv')) # xgboost1 predictions
m2 <- read.csv(here('data','test_model2.csv')) # xgboost2 predictions
m3 <- read.csv(here('data','test_model3.csv')) # xgboost3 predictions
m4 <- read.csv(here('data','test_model4.csv')) # xgboost4 predictions
m5 <- read.csv(here('data','test_model5.csv')) # xgboost5 predictions
m6 <- read.csv(here('data','glmnet_test_model1.csv')) # LR1 predictions
m7 <- read.csv(here('data','glmnet_test_model2.csv')) # LR2 predictions



m1$pred2 <- ifelse(m1$pred>.5,1,0)
m2$pred2 <- ifelse(m2$pred>.5,1,0)
m3$pred2 <- ifelse(m3$pred>.5,1,0)
m4$pred2 <- ifelse(m4$pred>.5,1,0)
m5$pred2 <- ifelse(m5$pred>.5,1,0)
m6$pred2 <- ifelse(m6$pred>.5,1,0)
m7$pred2 <- ifelse(m7$pred>.5,1,0)

# Brier score for males

round(eval(x=m1,type='brier.m'),4)
round(eval(x=m2,type='brier.m'),4)
round(eval(x=m3,type='brier.m'),4)
round(eval(x=m4,type='brier.m'),4)
round(eval(x=m5,type='brier.m'),4)
round(eval(x=m6,type='brier.m'),4)
round(eval(x=m7,type='brier.m'),4)

# Brier Score for females

round(eval(x=m1,type='brier.f'),4)
round(eval(x=m2,type='brier.f'),4)
round(eval(x=m3,type='brier.f'),4)
round(eval(x=m4,type='brier.f'),4)
round(eval(x=m5,type='brier.f'),4)
round(eval(x=m6,type='brier.f'),4)
round(eval(x=m7,type='brier.f'),4)

# Brier score for all 

round(eval(x=m1,type='brier.a'),4)
round(eval(x=m2,type='brier.a'),4)
round(eval(x=m3,type='brier.a'),4)
round(eval(x=m4,type='brier.a'),4)
round(eval(x=m5,type='brier.a'),4)
round(eval(x=m6,type='brier.a'),4)
round(eval(x=m7,type='brier.a'),4)

# Fairness adjusted score for males

round(eval(x=m1,type='fair.m'),4)
round(eval(x=m2,type='fair.m'),4)
round(eval(x=m3,type='fair.m'),4)
round(eval(x=m4,type='fair.m'),4)
round(eval(x=m5,type='fair.m'),4)
round(eval(x=m6,type='fair.m'),4)
round(eval(x=m7,type='fair.m'),4)

# Fairness adjusted score for females

round(eval(x=m1,type='fair.f'),4)
round(eval(x=m2,type='fair.f'),4)
round(eval(x=m3,type='fair.f'),4)
round(eval(x=m4,type='fair.f'),4)
round(eval(x=m5,type='fair.f'),4)
round(eval(x=m6,type='fair.f'),4)
round(eval(x=m7,type='fair.f'),4)


round(auc_roc(preds =m1$pred,actuals = m1$out),3)
round(auc_roc(preds =m2$pred,actuals = m2$out),3)
round(auc_roc(preds =m3$pred,actuals = m3$out),3)
round(auc_roc(preds =m4$pred,actuals = m4$out),3)
round(auc_roc(preds =m5$pred,actuals = m5$out),3)
round(auc_roc(preds =m6$pred,actuals = m6$out),3)
round(auc_roc(preds =m7$pred,actuals = m7$out),3)



m <- (m1+m3+m4+m7)/4

m$pred2 <- ifelse(m$pred>.5,1,0)


round(eval(x=m,type='brier.m'),4)
round(eval(x=m,type='brier.f'),4)
round(eval(x=m,type='brier.a'),4)
round(eval(x=m,type='fair.m'),4)
round(eval(x=m,type='fair.f'),4)
round(auc_roc(preds =m$pred,actuals = m$out),3)

####################### CHALLENGE TEST DATA ####################################

m1 <- read.csv(here('data','comptetition_test_model1.csv'))
m2 <- read.csv(here('data','comptetition_test_model2.csv'))
m3 <- read.csv(here('data','comptetition_test_model3.csv'))
m4 <- read.csv(here('data','comptetition_test_model4.csv'))
m5 <- read.csv(here('data','comptetition_test_model5.csv'))
m6 <- read.csv(here('data','glmnet_comptetition_test_model1.csv'))
m7 <- read.csv(here('data','glmnet_comptetition_test_model2.csv'))

m1           <- merge(out[,c(1,2)],m1,by.x='ID',by.y='id')
colnames(m1) <- c('id','out','gender','race','pred')
m1$out       <- ifelse(m1$out=='true',1,0)
m1           <- m1[,c('id','gender','race','pred','out')]


m2           <- merge(out[,c(1,2)],m2,by.x='ID',by.y='id')
colnames(m2) <- c('id','out','gender','race','pred')
m2$out       <- ifelse(m2$out=='true',1,0)
m2           <- m2[,c('id','gender','race','pred','out')]

m3           <- merge(out[,c(1,2)],m3,by.x='ID',by.y='id')
colnames(m3) <- c('id','out','gender','race','pred')
m3$out       <- ifelse(m3$out=='true',1,0)
m3           <- m3[,c('id','gender','race','pred','out')]

m4           <- merge(out[,c(1,2)],m4,by.x='ID',by.y='id')
colnames(m4) <- c('id','out','gender','race','pred')
m4$out       <- ifelse(m4$out=='true',1,0)
m4           <- m4[,c('id','gender','race','pred','out')]

m5           <- merge(out[,c(1,2)],m5,by.x='ID',by.y='id')
colnames(m5) <- c('id','out','gender','race','pred')
m5$out       <- ifelse(m5$out=='true',1,0)
m5           <- m5[,c('id','gender','race','pred','out')]

m6           <- merge(out[,c(1,2)],m6,by.x='ID',by.y='id')
colnames(m6) <- c('id','out','gender','race','pred')
m6$out       <- ifelse(m6$out=='true',1,0)
m6           <- m6[,c('id','gender','race','pred','out')]

m7           <- merge(out[,c(1,2)],m7,by.x='ID',by.y='id')
colnames(m7) <- c('id','out','gender','race','pred')
m7$out       <- ifelse(m7$out=='true',1,0)
m7           <- m7[,c('id','gender','race','pred','out')]


m1$pred2 <- ifelse(m1$pred>.5,1,0)
m2$pred2 <- ifelse(m2$pred>.5,1,0)
m3$pred2 <- ifelse(m3$pred>.5,1,0)
m4$pred2 <- ifelse(m4$pred>.5,1,0)
m5$pred2 <- ifelse(m5$pred>.5,1,0)
m6$pred2 <- ifelse(m6$pred>.5,1,0)
m7$pred2 <- ifelse(m7$pred>.5,1,0)

# Brier score for males

round(eval(x=m1,type='brier.m'),4)
round(eval(x=m2,type='brier.m'),4)
round(eval(x=m3,type='brier.m'),4)
round(eval(x=m4,type='brier.m'),4)
round(eval(x=m5,type='brier.m'),4)
round(eval(x=m6,type='brier.m'),4)
round(eval(x=m7,type='brier.m'),4)

# Brier Score for females

round(eval(x=m1,type='brier.f'),4)
round(eval(x=m2,type='brier.f'),4)
round(eval(x=m3,type='brier.f'),4)
round(eval(x=m4,type='brier.f'),4)
round(eval(x=m5,type='brier.f'),4)
round(eval(x=m6,type='brier.f'),4)
round(eval(x=m7,type='brier.f'),4)

# Brier score for all 

round(eval(x=m1,type='brier.a'),4)
round(eval(x=m2,type='brier.a'),4)
round(eval(x=m3,type='brier.a'),4)
round(eval(x=m4,type='brier.a'),4)
round(eval(x=m5,type='brier.a'),4)
round(eval(x=m6,type='brier.a'),4)
round(eval(x=m7,type='brier.a'),4)

# Fairness adjusted score for males

round(eval(x=m1,type='fair.m'),4)
round(eval(x=m2,type='fair.m'),4)
round(eval(x=m3,type='fair.m'),4)
round(eval(x=m4,type='fair.m'),4)
round(eval(x=m5,type='fair.m'),4)
round(eval(x=m6,type='fair.m'),4)
round(eval(x=m7,type='fair.m'),4)

# Fairness adjusted score for females

round(eval(x=m1,type='fair.f'),4)
round(eval(x=m2,type='fair.f'),4)
round(eval(x=m3,type='fair.f'),4)
round(eval(x=m4,type='fair.f'),4)
round(eval(x=m5,type='fair.f'),4)
round(eval(x=m6,type='fair.f'),4)
round(eval(x=m7,type='fair.f'),4)

# AUC

round(auc_roc(preds =m1$pred,actuals = m1$out),3)
round(auc_roc(preds =m2$pred,actuals = m2$out),3)
round(auc_roc(preds =m3$pred,actuals = m3$out),3)
round(auc_roc(preds =m4$pred,actuals = m4$out),3)
round(auc_roc(preds =m5$pred,actuals = m5$out),3)
round(auc_roc(preds =m6$pred,actuals = m6$out),3)
round(auc_roc(preds =m7$pred,actuals = m7$out),3)

# STACKED predictions

pred1 <- read.csv(here('data','CrescentStar_1YearForecast.csv'))
y1 <- merge(out,pred1)
y1 <- y1[,c(1,5,6,7,2)]
colnames(y1) <- c('id','gender','race','pred','out')
y1$pred2 <- ifelse(y1$pred>.5,1,0)
y1$out   <- ifelse(y1$out=='true',1,0)


round(eval(x=y1,type='brier.m'),4)
round(eval(x=y1,type='brier.f'),4)
round(eval(x=y1,type='brier.a'),4)
round(eval(x=y1,type='fair.f'),4)
round(eval(x=y1,type='fair.m'),4)
round(auc_roc(preds =y1$pred,actuals = y1$out),3)


###############################################################################

# Output based on different thresholds


fp <- c()
tp <- c()
pr <- c()

th <- seq(.5,.999,.001)

for(i in 1:length(th)){

  m1$pred2 <- ifelse(m1$pred>th[i],1,0)

  tab      <- matrix(nrow=2,ncol=2)
  tab[1,1] <- sum(m1$out==0 & m1$pred2==0)
  tab[1,2] <- sum(m1$out==0 & m1$pred2==1)
  tab[2,1] <- sum(m1$out==1 & m1$pred2==0)
  tab[2,2] <- sum(m1$out==1 & m1$pred2==1)
  
  fp[i] = tab[1,2]/sum(tab[1,])
  tp[i] = tab[2,2]/sum(tab[2,])
  pr[i] = tab[2,2]/sum(tab[,2])

}



plot(th,fp,type='l',ylim=c(0,1),ylab='',xlab='Threshold')
points(th,tp,type='l',lty=2)
points(th,pr,type='l',lty=3)

text(0.5,0.58,'Precision')
text(0.52,0.25,'True Positive Rate')
text(0.52,0.09,'False Positive Rate')



################################################################################
################################################################################
################################################################################
#
# Year 2 Predictions
#
################################################################################
################################################################################
################################################################################

test1 <- read.csv(here('data','test_model1_Y2.csv'))
test1$pred2 <- ifelse(test1$pred>.5,1,0)

round(eval(x=test1,type='brier.m'),4)
round(eval(x=test1,type='brier.f'),4)
round(eval(x=test1,type='brier.a'),4)
round(eval(x=test1,type='fair.f'),4)
round(eval(x=test1,type='fair.m'),4)
round(auc_roc(preds =test1$pred,actuals = test1$out),3)




test2 <- read.csv(here('data','comptetition_test_model1_Y2.csv'))

y1 <- merge(out[,c(1,3)],test2,by.x='ID',by.y='id')

y1 <- y1[,c(1,3,4,5,2)]
colnames(y1) <- c('id','gender','race','pred','out')
y1$out   <- ifelse(y1$out=='true',1,0)
y1$pred2 <- ifelse(y1$pred>.5,1,0)

round(eval(x=y1,type='brier.m'),4)
round(eval(x=y1,type='brier.f'),4)
round(eval(x=y1,type='brier.a'),4)
round(eval(x=y1,type='fair.f'),4)
round(eval(x=y1,type='fair.m'),4)
round(auc_roc(preds =y1$pred,actuals = y1$out),3)



################################################################################
################################################################################
################################################################################
#
# Year 2 Predictions
#
################################################################################
################################################################################
################################################################################

test1 <- read.csv(here('data','test_model1_Y3.csv'))
test1$pred2 <- ifelse(test1$pred>.5,1,0)

round(eval(x=test1,type='brier.m'),4)
round(eval(x=test1,type='brier.f'),4)
round(eval(x=test1,type='brier.a'),4)
round(eval(x=test1,type='fair.f'),4)
round(eval(x=test1,type='fair.m'),4)
round(auc_roc(preds =test1$pred,actuals = test1$out),3)




test2 <- read.csv(here('data','comptetition_test_model1_Y3.csv'))

y1 <- merge(out[,c(1,4)],test2,by.x='ID',by.y='id')

y1 <- y1[,c(1,3,4,5,2)]
colnames(y1) <- c('id','gender','race','pred','out')
y1$out   <- ifelse(y1$out=='true',1,0)
y1$pred2 <- ifelse(y1$pred>.5,1,0)

round(eval(x=y1,type='brier.m'),4)
round(eval(x=y1,type='brier.f'),4)
round(eval(x=y1,type='brier.a'),4)
round(eval(x=y1,type='fair.f'),4)
round(eval(x=y1,type='fair.m'),4)
round(auc_roc(preds =y1$pred,actuals = y1$out),3)



################################################################################
pred1 <- read.csv('CrescentStar_1YearForecast.csv')
pred2 <- read.csv('CrescentStar_2YearForecast.csv')
pred3 <- read.csv('CrescentStar_3YearForecast.csv')
################################################################################



















