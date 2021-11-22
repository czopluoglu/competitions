
require(here)
require(mltools)
###############################################################################

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

###############################################################################


m1 <- read.csv(here('data','test_model1.csv'))
m2 <- read.csv(here('data','test_model2.csv'))
m3 <- read.csv(here('data','test_model3.csv'))
m4 <- read.csv(here('data','test_model4.csv'))
m5 <- read.csv(here('data','test_model5.csv'))
m6 <- read.csv(here('data','glmnet_test_model1.csv'))
m7 <- read.csv(here('data','glmnet_test_model2.csv'))



m1$pred2 <- ifelse(m1$pred>.5,1,0)
m2$pred2 <- ifelse(m2$pred>.5,1,0)
m3$pred2 <- ifelse(m3$pred>.5,1,0)
m4$pred2 <- ifelse(m4$pred>.5,1,0)
m5$pred2 <- ifelse(m5$pred>.5,1,0)
m6$pred2 <- ifelse(m6$pred>.5,1,0)


eval(x=m1,type='brier.m') 
eval(x=m2,type='brier.m') 
eval(x=m3,type='brier.m') 
eval(x=m4,type='brier.m') 
eval(x=m5,type='brier.m') 
eval(x=m6,type='brier.m') 
eval(x=m7,type='brier.m') 

eval(x=m1,type='brier.f') 
eval(x=m2,type='brier.f') 
eval(x=m3,type='brier.f') 
eval(x=m4,type='brier.f') 
eval(x=m5,type='brier.f') 
eval(x=m6,type='brier.f') 
eval(x=m7,type='brier.f') 


eval(x=m1,type='brier.a') 
eval(x=m2,type='brier.a') 
eval(x=m3,type='brier.a') 
eval(x=m4,type='brier.a') 
eval(x=m5,type='brier.a') 
eval(x=m6,type='brier.a') 
eval(x=m7,type='brier.a') 


eval(x=m1,type='fair.m') 
eval(x=m2,type='fair.m') 
eval(x=m3,type='fair.m') 
eval(x=m4,type='fair.m') 
eval(x=m5,type='fair.m') 
eval(x=m6,type='fair.m') 
eval(x=m7,type='fair.m') 


eval(x=m1,type='fair.f') 
eval(x=m2,type='fair.f') 
eval(x=m3,type='fair.f') 
eval(x=m4,type='fair.f') 
eval(x=m5,type='fair.f') 
eval(x=m6,type='fair.f') 
eval(x=m7,type='fair.f') 




auc_roc(preds =m1$pred,actuals = m1$out)
auc_roc(preds =m2$pred,actuals = m2$out)
auc_roc(preds =m3$pred,actuals = m3$out)
auc_roc(preds =m4$pred,actuals = m4$out)
auc_roc(preds =m5$pred,actuals = m5$out)
auc_roc(preds =m6$pred,actuals = m6$out)
auc_roc(preds =m7$pred,actuals = m7$out)



cor(cbind(m1$pred,
          m2$pred,
          m3$pred,
          m4$pred,
          m5$pred,
          m6$pred,
          m7$pred))





m <- (m1+m3+m4+m7)/4

m$pred2 <- ifelse(m$pred>.5,1,0)


eval(x=m,type='brier.m') 
eval(x=m,type='brier.f') 
eval(x=m,type='brier.a') 
eval(x=m,type='fair.m') 
eval(x=m,type='fair.f') 
auc_roc(preds =m$pred,actuals = m$out)


###############################################################################



m1 <- read.csv(here('data','comptetition_test_model1.csv'))
m2 <- read.csv(here('data','comptetition_test_model2.csv'))
m3 <- read.csv(here('data','comptetition_test_model3.csv'))
m4 <- read.csv(here('data','comptetition_test_model4.csv'))
m5 <- read.csv(here('data','comptetition_test_model5.csv'))
m6 <- read.csv(here('data','glmnet_comptetition_test_model1.csv'))
m7 <- read.csv(here('data','glmnet_comptetition_test_model2.csv'))



cor(cbind(m1$pred,
          m2$pred,
          m3$pred,
          m4$pred,
          m5$pred,
          m6$pred,
          m7$pred))


m <- (m1+m3+m4+m7)/4

m$pred2 <- ifelse(m$pred>.5,1,0)


m <- m[,c('id','pred')]

m[,2] <- round(m[,2],4)

colnames(m) <- c('ID','Probability')


write.csv(m, 
          here('data','CrescentStar_1YearForecast.csv'),
          row.names = FALSE)




