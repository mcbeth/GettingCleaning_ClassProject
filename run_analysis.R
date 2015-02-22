library(dplyr)

getHARdata <- function() {                                                                        
  dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(dataURL,'HAR Dataset.zip')                                                   
}    

# This function presupposes that getHARdata has been called previously
# Read in the basic tables and paste them together
# In subject, activity, everything else order
read.UCI <- function(d){

  X.file <- unz('HAR Dataset.zip',paste('UCI HAR Dataset/',d,'/X_',d,'.txt',sep=''))
  X <- read.table(X.file)
  #close(X.file)
  
  Y.file <- unz('HAR Dataset.zip',paste('UCI HAR Dataset/',d,'/y_',d,'.txt',sep=''))
  Y <- read.table(Y.file)
  
  subject.file <- unz('HAR Dataset.zip',paste('UCI HAR Dataset/',d,'/subject_',d,'.txt',sep=''))
  subject <- read.table(subject.file)
 
  # Not reading the raw acceleration and gyro data, as the next task is to look at only the means
  # body.acc.x <- read.table(paste('UCI HAR Dataset/',d,'/Inertial Signals/body_acc_x_',d,'.txt',sep=''))
  # etc.
  
  cbind(subject,Y,X)
}

# paste together the test and train data and tidy it for further analysis
# this function presupposes that getHARdata has been called previously
clean.UCI <- function()
{
  raw.data <- rbind(read.UCI('test'),read.UCI('train'))

  # Correct the column names
  feature.names <- read.table('UCI HAR Dataset/features.txt')
  augmented.names <- c('Subject','Activity',as.character(feature.names$V2))
  colnames(raw.data) <- augmented.names

  # Replace the activities with their names
  activity.names <- read.table('UCI HAR Dataset/activity_labels.txt')
  raw.data$Activity <- factor(raw.data$Activity,labels=activity.names$V2)

  # We could stop here, but the dataset wanted is now pared down

  # We want the first two columns, and anything that says mean or std
  desired.cols <- sort(c(1,2,grep('mean()',augmented.names,fixed=TRUE),grep('std())',augmented.names,fixed=TRUE)))
  
  data.subset <- raw.data[,desired.cols]
}

getHARdata()
cleaned.data <- clean.UCI()
average.data <- group_by(cleaned.data,Subject,Activity) %>%
    summarise_each(funs(mean))
write.table(average.data,'averageData.txt',row.name=FALSE)
