
#Stage 1:  Tune nround and eta

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
               num_parallel_tree = 1,
               nthread           = 10)


grid <- expand.grid(iter = seq(1,500,10),
                    etas = seq(0.01,1,.05))

grid$loss    <- NA
grid$brier_m <- NA
grid$brier_f <- NA
grid$brier_a <- NA
grid$fair_m  <- NA
grid$fair_f  <- NA

for(i in 1:nrow(grid)){
  
  brier.m <- c()
  brier.f <- c()
  brier.a <- c()
  fair.m <- c()
  fair.f <- c()
  loss   <- c()
  
  for(j in 1:length(myfolds)){
    
    temp.x <- train[-myfolds[[j]],-1]
    temp.y <- out1[-myfolds[[j]]]
    
    temp.x2 <- train[myfolds[[j]],-1]
    temp.y2 <- out1[myfolds[[j]]]
    
    mod   <- xgboost(params           = params,
                     objective        = 'binary:logistic',
                     eval_metric      = 'logloss',
                     data             = as.matrix(temp.x), 
                     label            = temp.y,
                     nrounds          = grid[i,]$iter,
                     eta              = grid[i,]$iter)
    
    
    pr <- predict(mod,as.matrix(temp.x2))
    #pr <- 1 / (1 + exp(-pr))
    
    outcome <- data.frame(gender = temp.x2$gender, 
                          race   = temp.x2$race, 
                          pred   = pr, 
                          out    = temp.y2)
    
    
    outcome$pred2 <- ifelse(outcome$pred>.5,1,0)
    
    brier.m[j] = eval(x=outcome,type='brier.m')
    brier.f[j] = eval(x=outcome,type='brier.f')
    brier.a[j] = eval(x=outcome,type='brier.a')
    fair.m[j]  = eval(x=outcome,type='fair.m')
    fair.f[j]  = eval(x=outcome,type='fair.f')
    loss[j]    = as.numeric(mod$evaluation_log[grid[i,]$iter][1,2])
  }
  
  grid[i,]$brier_m <-  mean(brier.m)
  grid[i,]$brier_f <-  mean(brier.f)
  grid[i,]$brier_a <-  mean(brier.a)
  grid[i,]$fair_m  <-  mean(fair.m)
  grid[i,]$fair_f  <-  mean(fair.f)
  
  print(i)
  
}


