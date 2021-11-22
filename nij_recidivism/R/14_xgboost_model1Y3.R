require(xgboost)
require(caret)
require(pROC)
require(xgboostExplainer)
require(mltools)
require(here)

load(here('data','final_dataY3.RData'))

dim(full_data)
dim(train_)


train_ <- train_[which(full_data$Recidivism_Arrest_Year2=='false'),]
out    <- out[which(full_data$Recidivism_Arrest_Year2=='false')]


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
    
    ret <- (1-ret1)*(1- (fpb1 - fpw1))
    
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
    
    ret <- (1-ret1)*(1- (fpb1 - fpw1))
    
  }
  
  ret
  
}

# Custom objective function

brier <- function(preds, dtrain){
  
  labels <- getinfo(dtrain, "label")
  preds  <- 1 / (1 + exp(-preds))
  
  grad = 2*(preds-labels)*preds*(1-preds)
  hess = 2*(2*(labels+1)*preds-labels-3*preds*preds)*preds*(1-preds)
  
  return(list(grad = grad, hess = hess))
}

# Custom Eval Metrix

brier.err <- function(preds, dtrain){
  labels <- getinfo(dtrain, "label")
  err <- as.numeric(sum(labels != (preds > 0))) / length(labels)
  return(list(metric = "briererr", value = err))
}



###############################################################################

set.seed(06142021)

vec <- 1:nrow(train_)
loc1 <- sample(vec,8000)
loc2 <- vec[!vec%in%loc1]


test  <- train_[loc2,]
train <- train_[loc1,]

out2 <- out[loc2]
out1 <- out[loc1]

################################################################################

dtrain <- xgb.DMatrix(data = data.matrix(train[,-1]), label=out1)
dtest  <- xgb.DMatrix(data = data.matrix(test[,-1]),  label=out2)

################################################################################

myfolds <- createFolds(1:nrow(dtrain),10)

################################################################################

params <- list(booster           = "gbtree", 
               gamma             = 0, 
               max_depth         = 6, 
               min_child_weight  = 1, 
               subsample         = 1, 
               colsample_bytree  = 1,
               max_delta_step    = 0,
               lambda            = 1,
               alpha             = 0,
               scale_pos_weight  = 1,
               num_parallel_tree = 1)

  
mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'rmse',
                 showsd                 = TRUE,
                 nthread                = 10, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 1000,
                 eta                    = 0.01,
                 early_stopping_rounds  = 100)

model   <- xgboost(params           = params,
                   objective        = 'binary:logistic',
                   eval_metric      = 'rmse',
                   data             = dtrain,
                   nrounds          = 364,
                   eta              = 0.01)


pr <- predict(model,dtest)*1
hist(pr)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f') 
eval(x=outcome,type='brier.a')  # 0.1658196
eval(x=outcome,type='fair.m')   
eval(x=outcome,type='fair.f')   
auc_roc(preds = outcome$pred,actuals = outcome$out) #0.7210745

table(pr>.5)

###############################################################################

imp = xgb.importance(model=model)

xgb.plot.importance(importance_matrix = imp, 
                    top_n = 15,
                    xlim=c(0,.3),
                    xlab='Importance',
                    ylab='Features')

################################################################################

# Save the results for the test sample  


pr <- predict(model,dtest)

outcome <- data.frame(id     = test$ID,
                      gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


write.csv(outcome, 
          here('data','test_model1_Y3.csv'),
          row.names = FALSE)


################################################################################

# Save the results for the competition test sample

head(final_test)


dtest2  <- xgb.DMatrix(data = data.matrix(final_test[,2:659]))

pr2 <- predict(model,dtest2)

outcome2 <- data.frame(id     = final_test$ID,
                       gender = final_test$gender, 
                       race   = final_test$race, 
                       pred   = pr2)

write.csv(outcome2, 
          here('data','comptetition_test_model1_Y3.csv'),
          row.names = FALSE)






















