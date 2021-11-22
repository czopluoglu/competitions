
###############################################################################



m1 <- read.csv(here('data','comptetition_test_model1_Y3.csv'))


m <- m1

m$pred2 <- ifelse(m$pred>.5,1,0)


m <- m[,c('id','pred')]

m[,2] <- round(m[,2],4)

colnames(m) <- c('ID','Probability')


write.csv(m, 
          here('data','CrescentStar_3YearForecast.csv'),
          row.names = FALSE)

