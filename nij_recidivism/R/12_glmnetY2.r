
require(caret)
require(here)
require(mltools)
require(glmnet)
require(psych)

load(here('data','final_dataY2.RData'))

################################################################################
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

################################################################################

set.seed(06142021)

vec <- 1:nrow(train_)
loc1 <- sample(vec,11000)
loc2 <- vec[!vec%in%loc1]


test  <- train_[loc2,]
train <- train_[loc1,]

out2 <- out[loc2]
out1 <- out[loc1]


# Fill missing values with median

for(i in 1:ncol(train)){
  train[is.na(train[,i]),i] = median(train[,i],na.rm=TRUE)
}

for(i in 1:ncol(test)){
  test[is.na(test[,i]),i] = median(test[,i],na.rm=TRUE)
}


desc <- describe(train[,2:659])


which(desc$skew > 3)


final_test2 <- final_test

for(i in 1:ncol(final_test2)){
  final_test2[is.na(final_test2[,i]),i] = median(final_test2[,i],na.rm=TRUE)
}

################################################################################
################################################################################
################################################################################
#
#
#         Main Effects only
#
################################################################################
################################################################################
################################################################################


ridge <- cv.glmnet(x = as.matrix(train[,2:659]),
                   y = out1,
                   alpha = 0)

plot(ridge, main = "Ridge penalty\n\n")


ridge$lambda.min
coef(ridge,ridge$lambda.min)

ridge.fit <- glmnet(x = as.matrix(train[,2:659]), 
                    y = out1, 
                    alpha = 0, 
                    lambda = ridge$lambda.min,
                    family = "binomial")


pred <- predict(ridge.fit,as.matrix(test[,2:659]))
pred <- exp(pred)/(1+exp(pred))


auc_roc(preds = pred[,1],
        actuals=out2)

mean((pred[,1]-out2)^2)

################################################################

lasso <-  cv.glmnet(x = as.matrix(train[,2:659]),
                    y = out1,
                    alpha = 1)

plot(lasso, main = "Lasso penalty\n\n")

lasso$lambda.min
coef(lasso,lasso$lambda.min)

lasso.fit <- glmnet(x = as.matrix(train[,2:659]), 
                    y = out1, 
                    alpha = 1, 
                    lambda = lasso$lambda.min,
                    family = "binomial")


pred <- predict(lasso.fit,as.matrix(test[,2:659]))
pred <- exp(pred)/(1+exp(pred))

auc_roc(preds = pred[,1],
        actuals=out2)

mean((pred[,1]-out2)^2)

################################################################

getModelInfo(model='glmnet')


cv_elastic <- caret::train(x = as.matrix(train[,2:659]),
                           y = as.factor(out1),
                           method = "glmnet",
                           preProc = c("zv", "center", "scale"),
                           trControl = trainControl(method = "cv", number = 10),
                           type.measure = 'ROC',
                           tuneLength = 10)

cv_elastic$bestTune

elastic.fit <- glmnet(x = as.matrix(train[,2:659]), 
                      y = out1, 
                      alpha  = 0.2, 
                      lambda = 0.00944305,
                      family = "binomial")

pred <- predict(elastic.fit,as.matrix(test[,2:659]))
pred <- exp(pred)/(1+exp(pred))

auc_roc(preds = pred[,1],
        actuals=out2)

mean((pred[,1]-out2)^2)





outcome <- data.frame(id     = test$ID,
                      gender = test$gender, 
                      race   = test$race, 
                      pred   = pred[,1], 
                      out    = out2)


write.csv(outcome, 
          here('data','glmnet_test_model1Y2.csv'),
          row.names = FALSE)


pred <- predict(elastic.fit,as.matrix(final_test2[,2:659]))
pred <- exp(pred)/(1+exp(pred))


outcome2 <- data.frame(id     = final_test2$ID,
                       gender = final_test2$gender, 
                       race   = final_test2$race, 
                       pred   = pred[,1])

write.csv(outcome2, 
          here('data','glmnet_comptetition_test_model1Y2.csv'),
          row.names = FALSE)



sig <- coef(elastic.fit)@i

main.effects <- colnames(train[,2:659])[sig[2:length(sig)]]
################################################################################
################################################################################
################################################################################
#
#
#        Main Effect and Interactions
#
################################################################################
################################################################################
################################################################################

train$out1 <- out1
f <- as.formula(out1 ~ .*.)
X <- model.matrix(f, train[,c(main.effects,'out1')])[, -1]


test$out2 <- out2 
f <- as.formula(out2 ~ .*.)

X2 <- model.matrix(f, test[,c(main.effects,'out2')])[, -1]

#################################################################### 


################################################################

cv_elastic <- caret::train(x = X,
                           y = as.factor(out1),
                           method = "glmnet",
                           preProc = c("zv", "center", "scale"),
                           trControl = trainControl(method = "cv", number = 10),
                           type.measure = 'ROC',
                           tuneLength = 10)

cv_elastic$bestTune

elastic.fit <- glmnet(x = X, 
                      y = out1, 
                      alpha  = cv_elastic$bestTune[1] ,
                      lambda = cv_elastic$bestTune[2],
                      family = "binomial")

pred <- predict(elastic.fit,X2)
pred <- exp(pred)/(1+exp(pred))

auc_roc(preds = pred[,1],
        actuals=out2)

mean((pred[,1]-out2)^2)


