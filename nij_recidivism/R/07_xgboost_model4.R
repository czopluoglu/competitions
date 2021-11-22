require(xgboost)
require(caret)
require(pROC)
require(xgboostExplainer)
require(here)
require(mltools)

load(here('data','final_data.RData'))

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

set.seed(05012021)

vec <- 1:nrow(train_)
loc1 <- sample(vec,15000)
loc2 <- vec[!vec%in%loc1]


test  <- train_[loc2,]
train <- train_[loc1,]

out2 <- out[loc2]
out1 <- out[loc1]


#corr <- as.data.frame(t(cor(out1,train[,-1],use='pairwise')))

#corr$ind <- NA
#for(i in 1:nrow(corr)){
#  corr[i,]$ind <- which(colnames(train_)==rownames(corr)[i])
#}

#corr <- corr[order(corr$V1,decreasing=TRUE),]

#ind <- sort(corr[which(abs(corr$V1)>.01),]$ind)


#test  <- train_[loc2,c(1,ind)]
#train <- train_[loc1,c(1,ind)]

################################################################################

dtrain <- xgb.DMatrix(data = data.matrix(train[,-1]), label=out1)
dtest  <- xgb.DMatrix(data = data.matrix(test[,-1]),  label=out2)

################################################################################

myfolds <- createFolds(1:nrow(dtrain),10)

################################################################################


################################################################################
################################################################################
#Stage 1:  Tune eta
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               max_depth         = 5, 
               min_child_weight  = 1, 
               gamma             = 0, 
               subsample         = 1, 
               colsample_bytree  = 1,
               max_delta_step    = 0,
               lambda            = 1,
               alpha             = 1,
               scale_pos_weight  = 1,
               num_parallel_tree = 1)


mod <-  xgb.cv(data                   = dtrain, 
               params                 = params,
               objective              = 'binary:logistic',
               eval_metric            = 'logloss',
               showsd                 = TRUE,
               nthread                = 10, 
               predict                = TRUE,
               folds                  = myfolds,
               nrounds                = 1000,
               eta                    = .1,
               early_stopping_rounds  = 30)


################################################################################
################################################################################
#Stage 2:  Max depth and min_child_Weight
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               gamma             = 0, 
               subsample         = 1, 
               colsample_bytree  = 1,
               max_delta_step    = 0,
               lambda            = 1,
               alpha             = 1,
               scale_pos_weight  = 1,
               num_parallel_tree = 1)

max_dep <- seq(3,7,1)
min_child_weight <- seq(0,7,0.25)

grid <- expand.grid(depth = max_dep, weight = min_child_weight)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 10, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 44,
                 eta                    = 0.1,
                 max_depth              = grid[i,]$depth, 
                 min_child_weight       = grid[i,]$weight,)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

grid[order(grid$loss),]

plot(grid$weight,grid$loss)
plot(grid$depth,grid$loss)


# Depth = 4, weight = 4, optimized


model   <- xgboost(params           = params,
                   objective        = 'binary:logistic',
                   eval_metric      = 'logloss',
                   data             = dtrain,
                   nrounds          = 44,
                   eta              = 0.1,
                   max_depth        = 4,
                   min_child_weight = .5)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f')  
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')   
eval(x=outcome,type='fair.f')   
auc_roc(preds = outcome$pred,actuals = outcome$out)
################################################################################
################################################################################
#Stage 3:  Gamma
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               subsample         = 1, 
               colsample_bytree  = 1,
               max_delta_step    = 0,
               lambda            = 1,
               alpha             = 1,
               scale_pos_weight  = 1,
               num_parallel_tree = 1)

gamma <- seq(0,1,.01)

grid <- expand.grid(gamma = gamma)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 10, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 44,
                 eta                    = 0.1,
                 max_depth              = 4,
                 min_child_weight       = .5,
                 gamma                  = grid[i,]$gamma)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

grid[order(grid$loss),]

plot(grid$gamma,grid$loss)

# Gamma = 0.51, optimized


model   <- xgboost(params           = params,
                   objective        = 'binary:logistic',
                   eval_metric      = 'logloss',
                   data             = dtrain,
                   nrounds          = 44,
                   eta              = 0.1,
                   max_depth        = 4,
                   min_child_weight = .5,
                   gamma            = 0.51)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f')  
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')  
eval(x=outcome,type='fair.f')   
auc_roc(preds = outcome$pred,actuals = outcome$out)

################################################################################
################################################################################
#Stage 4:  max_delta_step
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               subsample         = 1, 
               colsample_bytree  = 1,
               lambda            = 1,
               alpha             = 0,
               scale_pos_weight  = 1,
               num_parallel_tree = 1)


delta <- seq(0,10,.1)

grid <- expand.grid(delta = delta)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 10, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds          = 44,
                 eta              = 0.1,
                 max_depth        = 4,
                 min_child_weight = .5,
                 gamma            = 0.51,
                 max_delta_step         = grid[i,]$delta)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

plot(grid$delta,grid$loss)

# max_delta_step = 2.7, optimized


model   <- xgboost(params                 = params,
                   objective              = 'binary:logistic',
                   eval_metric            = 'logloss',
                   data                   = dtrain,
                   nrounds                = 44,
                   eta                    = 0.1,
                   max_depth              = 4,
                   min_child_weight       = .5,
                   gamma                  = 0.51,
                   max_delta_step         = 2.7)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f')  
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')  
eval(x=outcome,type='fair.f')   

auc_roc(preds = outcome$pred,actuals = outcome$out) 

################################################################################
################################################################################
#Stage 5: scale_pos_weight
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               subsample         = 1, 
               colsample_bytree  = 1,
               lambda            = 1,
               alpha             = 0,
               num_parallel_tree = 1)

pos <- seq(0,3,.25)

grid <- expand.grid(pos=pos)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 20, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 44,
                 eta                    = 0.1,
                 max_depth              = 4,
                 min_child_weight       = .5,
                 gamma                  = 0.51,
                 max_delta_step         = 2.7,
                 scale_pos_weight       = grid[i,]$pos)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

# scale_pos_Weight = 1, optimized

model   <- xgboost(params                 = params,
                   objective              = 'binary:logistic',
                   eval_metric            = 'logloss',
                   data                   = dtrain,
                   nrounds                = 44,
                   eta                    = 0.1,
                   max_depth              = 4,
                   min_child_weight       = .5,
                   gamma                  = 0.51,
                   max_delta_step         = 2.7,
                   scale_pos_weight       = 1)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f')  
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')   
eval(x=outcome,type='fair.f')   

auc_roc(preds = outcome$pred,actuals = outcome$out) 

################################################################################
################################################################################
#Stage 6: lambda and alpha
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               subsample         = 1, 
               colsample_bytree  = 1,
               num_parallel_tree = 1)

l <- seq(0.1,30,1)
a <- seq(0,6,.5)

grid <- expand.grid(l = l, a = a)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 20, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 44,
                 eta                    = 0.1,
                 max_depth              = 4,
                 min_child_weight       = .5,
                 gamma                  = 0.51,
                 max_delta_step         = 2.7,
                 scale_pos_weight       = 1,
                 lambda                 = grid[i,]$l,
                 alpha                  = grid[i,]$a)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

plot(grid$a,grid$loss)
plot(grid$l,grid$loss)

# lambda = 7.1 alpha = 1.5, optimized

model   <- xgboost(params                 = params,
                   objective              = 'binary:logistic',
                   eval_metric            = 'logloss',
                   data                   = dtrain,
                   nrounds                = 44,
                   eta                    = 0.1,
                   max_depth              = 4,
                   min_child_weight       = .5,
                   gamma                  = 0.51,
                   max_delta_step         = 2.7,
                   scale_pos_weight       = 1,
                   lambda                 = 7.1,
                   alpha                  = 1.5)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  # 0.1914684
eval(x=outcome,type='brier.f')  # 0.1596497 
eval(x=outcome,type='brier.a')  # 0.1875699
eval(x=outcome,type='fair.m')   # 0.8026085
eval(x=outcome,type='fair.f')   # 0.83219616
auc_roc(preds = outcome$pred,actuals = outcome$out) # 0.6976663 


################################################################################
################################################################################
#Stage 7: subsample
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               colsample_bytree  = 1,
               num_parallel_tree = 1)

sample <- seq(0.05,1,.05)

grid <- expand.grid(sample= sample)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 10, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 44,
                 eta                    = 0.1,
                 max_depth              = 4,
                 min_child_weight       = .5,
                 gamma                  = 0.51,
                 max_delta_step         = 2.7,
                 scale_pos_weight       = 1,
                 lambda                 = 7.1,
                 alpha                  = 1.5,
                 subsample              = grid[i,]$sample)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

# subsample = 0.75, optimized


model   <- xgboost(params                 = params,
                   objective              = 'binary:logistic',
                   eval_metric            = 'logloss',
                   data                   = dtrain,
                   nrounds                = 44,
                   eta                    = 0.1,
                   max_depth              = 4,
                   min_child_weight       = .5,
                   gamma                  = 0.51,
                   max_delta_step         = 2.7,
                   scale_pos_weight       = 1,
                   lambda                 = 7.1,
                   alpha                  = 1.5,
                   subsample              = 0.75)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f')  
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')   
eval(x=outcome,type='fair.f')   
auc_roc(preds = outcome$pred,actuals = outcome$out) 


################################################################################
################################################################################
#Stage 6: colsample_bytree
################################################################################
################################################################################

params <- list(booster           = "gbtree", 
               num_parallel_tree = 1)


sample <- seq(0.05,1,.05)

grid <- expand.grid(sample= sample)

grid$loss    <- NA
grid$iter    <- NA

for(i in 1:nrow(grid)){
  
  mod <-  xgb.cv(data                   = dtrain, 
                 params                 = params,
                 objective              = 'binary:logistic',
                 eval_metric            = 'logloss',
                 showsd                 = TRUE,
                 nthread                = 20, 
                 predict                = TRUE,
                 folds                  = myfolds,
                 nrounds                = 44,
                 eta                    = 0.1,
                 max_depth              = 4,
                 min_child_weight       = .5,
                 gamma                  = 0.51,
                 max_delta_step         = 2.7,
                 scale_pos_weight       = 1,
                 lambda                 = 7.1,
                 alpha                  = 1.5,
                 subsample              = 1,
                 colsample_bytree       = grid[i,]$sample)
  
  logs <- mod$evaluation_log
  
  grid[i,]$iter <- which.min(logs$test_logloss_mean)
  grid[i,]$loss <- min(logs$test_logloss_mean)
  
  print(i)
  
}

grid[which.min(grid$loss),]

# colsample_bytree = 1, optimized

model   <- xgboost(params                 = params,
                   objective              = 'binary:logistic',
                   eval_metric            = 'logloss',
                   data                   = dtrain,
                   nrounds                = 44,
                   eta                    = 0.1,
                   max_depth              = 4,
                   min_child_weight       = .5,
                   gamma                  = 0.51,
                   max_delta_step         = 2.7,
                   scale_pos_weight       = 1,
                   lambda                 = 7.1,
                   alpha                  = 1.5,
                   subsample              = 0.75,
                   colsample_bytree       = 1)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f') 
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')   
eval(x=outcome,type='fair.f')   
auc_roc(preds = outcome$pred,actuals = outcome$out) 

################################################################################
################################################################################
#Stage 8: Reduce learning rate , re-iterate
################################################################################
################################################################################
0.556108
params <- list(booster           = "gbtree")

mod <-  xgb.cv(data                   = dtrain, 
               params                 = params,
               objective              = 'binary:logistic',
               eval_metric            = 'logloss',
               showsd                 = TRUE,
               nthread                = 20, 
               predict                = TRUE,
               folds                  = myfolds,
               nrounds                = 10000,
               eta                    = 0.1,
               max_depth              = 4,
               min_child_weight       = .5,
               gamma                  = 0.51,
               max_delta_step         = 2.7,
               scale_pos_weight       = 1,
               lambda                 = 7.1,
               alpha                  = 1.5,
               subsample              = 1,
               colsample_bytree       = 1,
               num_parallel_tree      = 1,
               early_stopping_rounds  = 30)



watchlist <- list(train=dtrain, test=dtest)
model   <- xgb.train(booster                = 'gbtree',
                     objective              = 'binary:logistic',
                     eval_metric            = 'rmse',
                     data                   = dtrain,
                     nrounds                = 69,
                     eta                    = 0.1,
                     max_depth              = 4,
                     min_child_weight       = .5,
                     gamma                  = 0.51,
                     max_delta_step         = 2.7,
                     scale_pos_weight       = 1,
                     lambda                 = 7.1,
                     alpha                  = 1.5,
                     subsample              = .75,
                     colsample_bytree       = 1,
                     num_parallel_tree      = 1,
                     watchlist              = watchlist)


pr <- predict(model,dtest)


outcome <- data.frame(gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


outcome$pred2 <- ifelse(outcome$pred>.5,1,0)

eval(x=outcome,type='brier.m')  
eval(x=outcome,type='brier.f')  
eval(x=outcome,type='brier.a')  
eval(x=outcome,type='fair.m')   
eval(x=outcome,type='fair.f')   
auc_roc(preds = outcome$pred,actuals = outcome$out)




###################################################

imp = xgb.importance(model=model)

xgb.plot.importance(importance_matrix = imp, 
                    top_n = 50,
                    xlim=c(0,.3),
                    xlab='Importance',
                    ylab='Features')

as.matrix(imp[1:50,])

[1,] "comp1"        "0.182783378" "0.110752237" "0.070381232" "0.182783378"
[2,] "age1c"        "0.131975905" "0.094873599" "0.057673509" "0.131975905"
[3,] "gang"         "0.100280529" "0.051303344" "0.030303030" "0.100280529"
[4,] "prop"         "0.038474486" "0.031357735" "0.025415445" "0.038474486"
[5,] "risk1"        "0.035589844" "0.050177505" "0.054740958" "0.035589844"
[6,] "felony"       "0.034959597" "0.043807853" "0.032258065" "0.034959597"
[7,] "Xv1"          "0.030256077" "0.025303853" "0.023460411" "0.030256077"
[8,] "gender"       "0.019948384" "0.043828183" "0.020527859" "0.019948384"
[9,] "age1"         "0.019853902" "0.023626829" "0.018572825" "0.019853902"
[10,] "age7"         "0.017628980" "0.032607008" "0.013685239" "0.017628980"
[11,] "year3"        "0.016617623" "0.028438797" "0.018572825" "0.016617623"
[12,] "year1c"       "0.015645814" "0.022028162" "0.013685239" "0.015645814"
[13,] "mhsa"         "0.012901706" "0.030550802" "0.016617791" "0.012901706"
[14,] "off3"         "0.012455865" "0.016653328" "0.011730205" "0.012455865"
[15,] "comp3"        "0.008885730" "0.012945419" "0.015640274" "0.008885730"
[16,] "race"         "0.008589306" "0.008698408" "0.015640274" "0.008589306"
[17,] "coged"        "0.008209914" "0.010734126" "0.014662757" "0.008209914"
[18,] "prior_parole" "0.008190567" "0.021909506" "0.011730205" "0.008190567"
[19,] "cmisd"        "0.007989374" "0.004667679" "0.016617791" "0.007989374"
[20,] "misd"         "0.007373880" "0.009319673" "0.015640274" "0.007373880"
[21,] "viol"         "0.007253917" "0.013134622" "0.012707722" "0.007253917"
[22,] "comp2"        "0.007009805" "0.015883467" "0.015640274" "0.007009805"
[23,] "off5"         "0.006839517" "0.028740159" "0.008797654" "0.006839517"
[24,] "off1"         "0.005849775" "0.011302521" "0.010752688" "0.005849775"
[25,] "drug"         "0.005685555" "0.004863027" "0.012707722" "0.005685555"
[26,] "educ3"        "0.005308896" "0.005042972" "0.008797654" "0.005308896"
[27,] "agri14"       "0.005207611" "0.008127638" "0.005865103" "0.005207611"
[28,] "educ2"        "0.004573221" "0.008262908" "0.009775171" "0.004573221"
[29,] "cprop"        "0.004557903" "0.004133374" "0.010752688" "0.004557903"
[30,] "hins7"        "0.004446149" "0.002885999" "0.005865103" "0.004446149"
[31,] "cviol"        "0.004292165" "0.003540228" "0.006842620" "0.004292165"
[32,] "slevel3"      "0.003955695" "0.002519321" "0.005865103" "0.003955695"
[33,] "slevel2"      "0.003574074" "0.002000420" "0.006842620" "0.003574074"
[34,] "age3"         "0.003524902" "0.002639718" "0.002932551" "0.003524902"
[35,] "cother"       "0.003521011" "0.006076923" "0.006842620" "0.003521011"
[36,] "prior_prob"   "0.003226697" "0.004733565" "0.005865103" "0.003226697"
[37,] "gcm1"         "0.003167255" "0.002778262" "0.004887586" "0.003167255"
[38,] "hospital17"   "0.003160748" "0.002250661" "0.003910068" "0.003160748"
[39,] "off2"         "0.003120880" "0.003267160" "0.010752688" "0.003120880"
[40,] "age5c"        "0.002988190" "0.003368077" "0.005865103" "0.002988190"
[41,] "off4"         "0.002946460" "0.002480772" "0.005865103" "0.002946460"
[42,] "educ1"        "0.002862018" "0.001668807" "0.005865103" "0.002862018"
[43,] "age2c"        "0.002805249" "0.001772887" "0.004887586" "0.002805249"
[44,] "transfer2"    "0.002710886" "0.002417055" "0.002932551" "0.002710886"
[45,] "vital11"      "0.002643456" "0.001451535" "0.002932551" "0.002643456"
[46,] "slevel1"      "0.002600268" "0.002252245" "0.006842620" "0.002600268"
[47,] "vital3"       "0.002599987" "0.002872685" "0.002932551" "0.002599987"
[48,] "year2"        "0.002429471" "0.002948972" "0.005865103" "0.002429471"
[49,] "assault"      "0.002399911" "0.002899992" "0.003910068" "0.002399911"
[50,] "dep"          "0.002356585" "0.001138626" "0.004887586" "0.002356585"
################################################################################

# Save the results for the test sample  


pr <- predict(model,dtest)

outcome <- data.frame(id     = test$ID,
                      gender = test$gender, 
                      race   = test$race, 
                      pred   = pr, 
                      out    = out2)


write.csv(outcome, 
          here('data','test_model4.csv'),
          row.names = FALSE)


################################################################################

head(final_test)

# Save the results for the competition test sample


dtest2  <- xgb.DMatrix(data = data.matrix(final_test[,-1]))

pr2 <- predict(model,dtest2)

outcome2 <- data.frame(id     = final_test$ID,
                       gender = final_test$gender, 
                       race   = final_test$race, 
                       pred   = pr2)

write.csv(outcome2, 
          here('data','comptetition_test_model4.csv'),
          row.names = FALSE)






















