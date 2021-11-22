
require(here)

################### Year 1 #####################################################

# Infer the true values in the test dataset by comparing Year 1 and Year 2 
# test datasets

test1 <- read.csv(here('data/NIJ_s_Recidivism_Challenge_Test_Dataset1.csv')) 
test2 <- read.csv(here('data/NIJ_s_Recidivism_Challenge_Test_Dataset2.csv')) 

true.out <- rbind(cbind(test1$ID[!test1$ID%in%test2$ID],1),
                  cbind(test1$ID[test1$ID%in%test2$ID],0))

true.out <- as.data.frame(true.out[order(true.out[,1]),])
colnames(true.out) <- c('ID','out')

pred <- read.csv(here('data/CrescentStar_1YearForecast.csv')) 

comb <- merge(true.out,pred)

mean((comb$out-comb$Probability)^2)
