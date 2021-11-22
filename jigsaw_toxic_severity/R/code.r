
require(here)

################################################################################

# Load the datasets

comments_score    <- read.csv(here('jigsaw_toxic_severity/data/comments_to_score.csv'),header = TRUE)
sample_submission <- read.csv(here('jigsaw_toxic_severity/data/sample_submission.csv'),header = TRUE)
validation        <- read.csv(here('jigsaw_toxic_severity/data/validation_data.csv'),header = TRUE)

################################################################################
#   TESTING DETOXIFY module from HUGGINGFACE
################################################################################

# load the reticulate package

  library(reticulate)

  # py_config(), check python configuration

# Specify the python path

  use_python('C:/Users/cengiz/Anaconda3/python.exe')

# Import the detoxify module

  detoxify <- import('detoxify',convert=FALSE)

# Test

  a <- detoxify$Detoxify('original')$predict('I like you.')
  
  a
  
  
  b <- detoxify$Detoxify('original')$predict('I hate Muslims!')
  
  b

# Restructure the output

  pd <- import('pandas',convert=FALSE)

  tmp <- pd$DataFrame$from_dict(a,orient='index')
  tmp
  
  a <- py_to_r(tmp)
  str(a)
  t(a)

################################################################################
#   APPLY DETOXIFY MODULE to COMMENTS in the SCORING SET
################################################################################

scores <- data.frame(comment_id = sample_submission$comment_id)
scores$toxicity <- NA
scores$severe_toxicity <- NA
scores$obscene <- NA
scores$threat <- NA
scores$insult <- NA
scores$identity_attack <- NA

for(i in 1:nrow(scores)){
  
  id  <- scores[i,]$comment_id
  loc <- which(comments_score$comment_id==id) 
  inp <- comments_score[loc,]$text
  
  # Remove \n, and \"
  
  inp <- gsub('\n', '', inp)
  
  inp <- gsub('\"', '', inp)
  
  # Predict
  
  tmp <- detoxify$Detoxify('original')$predict(inp)
  tmp <- py_to_r(tmp)
  
  scores[i,]$toxicity        <- tmp$toxicity
  scores[i,]$severe_toxicity <- tmp$severe_toxicity
  scores[i,]$obscene         <- tmp$obscene
  scores[i,]$threat          <- tmp$threat
  scores[i,]$insult          <- tmp$insult
  scores[i,]$identity_attack <- tmp$identity_attack
  
  print(i)
}  
  
  
################################################################################
#   APPLY DETOXIFY MODULE to COMMENTS in the VALIDATION SET
################################################################################

# run in batches
# when you run all of them, it runs into memory problems

batches <- seq(1,nrow(validation),2000)
batches <- c(batches,nrow(validation)+1)
batches

tmp1 <- list('vector',length(batches))

for(i in 1:(length(batches)-1)){
  
  loc1 <- batches[i]
  loc2 <- batches[i+1]-1
  
  tmp1[[i]] <- detoxify$Detoxify('original')$predict(validation[loc1:loc2,]$less_toxic)   
  print(i)
}
  

tmp2 <- list('vector',length(batches))

for(i in 1:(length(batches)-1)){
  
  loc1 <- batches[i]
  loc2 <- batches[i+1]-1
  
  tmp2[[i]] <- detoxify$Detoxify('original')$predict(validation[loc1:loc2,]$more_toxic)   
  print(i)
}


df1 <- data.frame(Reduce(cbind,py_to_r(tmp1[[1]])))
for(i in 2:16){
  df1 <- rbind(df1,data.frame(Reduce(cbind,py_to_r(tmp1[[i]]))))
}

df2 <- data.frame(Reduce(cbind,py_to_r(tmp2[[1]])))
for(i in 2:16){
  df2 <- rbind(df2,data.frame(Reduce(cbind,py_to_r(tmp2[[i]]))))
}

################################################################################

sum(df1[,1] < df2[,1])/30108
sum(df1[,2] < df2[,2])/30108
sum(df1[,3] < df2[,3])/30108
sum(df1[,4] < df2[,4])/30108
sum(df1[,5] < df2[,5])/30108
sum(df1[,6] < df2[,6])/30108

















  
  
  
  
