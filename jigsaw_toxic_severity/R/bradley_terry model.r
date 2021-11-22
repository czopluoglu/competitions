
require(here)

################################################################################

# Load the datasets

validation        <- read.csv(here('jigsaw_toxic_severity/data/validation_data.csv'),header = TRUE)

################################################################################

# Install thhe BradleyTerry2 model
# https://cran.r-project.org/web/packages/BradleyTerry2/vignettes/BradleyTerry.pdf
# install.packages('BradleyTerry2')

require(BradleyTerry2)

################################################################################

# Prepare data

  # Unique number of texts

    length(unique(c(validation$less_toxic,
                    validation$more_toxic)))
           
  # Assign a unique id to each unique text

    uniq_text <- unique(c(validation$less_toxic,
                          validation$more_toxic))
    
    ids <- cbind(uniq_text,paste0('X',1:14251))
    
  # Put the ids in the original data
    
    validation$id1 <- NA
    validation$id2 <- NA
    validation$win1 <- 0
    validation$win2 <- 1
    
    for(i in 1:nrow(validation)){
      
      loc1 <- which(ids[,1] == validation[i,]$less_toxic)
      
      validation[i,]$id1 <- ids[loc1,2]
     
      loc2 <- which(ids[,1] == validation[i,]$more_toxic)
      
      validation[i,]$id2 <- ids[loc2,2]
      
      print(i)
    }
    
    
    
    d <- validation[,4:7]
    head(d)
    
  # Aggregate he same comparisons if any
    
    d2 <- aggregate(cbind(win1,win2) ~ id1 + id2,data = d,sum)
  
    
    sub <- d2
    
    ids <- unique(c(sub$id1,sub$id2))
    
    sub$id1 <- factor(sub$id1,
                      labels = ids,
                      levels = ids)
    
    sub$id2 <- factor(sub$id2,
                      labels = ids,
                      levels = ids)
    
    
# Fit the model
    
    my.model <- BTm(cbind(win1, win2), 
                    id1, 
                    id2, 
                    data = sub)
    
    summary(my.model)
    
    BTabilities(my.model)
    

    
    
    
    
    
    
    
    