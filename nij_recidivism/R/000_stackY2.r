
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

