run_analysis <- function(path){

  ## set path for reading data locally
  ## example :  path <- "UCI HAR Dataset/"

  print("reading train data")  
  ## Read and store training data
  x_train = read.table(paste(path, "train/x_train.txt", sep=""))
  Y_train = read.table(paste(path, "train/Y_train.txt", sep=""))
  subject_train<-read.table(paste(path, "train/subject_train.txt", sep=""))
  
  print("reading test data")
  ## Read and store test data
  x_test = read.table(paste(path, "test/x_test.txt", sep=""))
  Y_test = read.table(paste(path, "test/Y_test.txt", sep=""))
  subject_test<-read.table(paste(path, "test/subject_test.txt", sep=""))
  
  print("reading features")
  ## Read features
  features <- read.table(paste(path, "features.txt",sep=""))
  
  print("reading activity labels")
  ## Read activity labels
  labels<-read.table(paste(path, "activity_labels.txt", sep=""))
  
  print("loading plyr and reshape libraries")
  ## load plyr and reshape libraries
  library(plyr)
  library(reshape2)
  
  print("merging data")
  ## 1. Merges the training and the test sets to create one data set.
  ## Merge all kind of data in one data set for x data , Y data and subject data
  x_all<-rbind(x_test, x_train)
  Y_all<-rbind(Y_test, Y_train)
  subject_all<-rbind(subject_test, subject_train)
  
  ## Name x_all from features.txt
  colnames(x_all)<-features[,2]
  
  ## 2. Extract only the measurements on the mean and standard deviation for each measurement
  
  print("extracting mean and standard deviation measurements")
  ## Create a logical vector using grep function in order to match the desired columns (mean() and std())
  mean_std_cols<- grepl("mean()",colnames(x_all)) | grepl("std()",colnames(x_all))
  
  ## extract data_frame with mean and std
  x_msr <- x_all[,mean_std_cols]
  
  ## 3. Uses descriptive activity names to name the activities in the data set
  ## map Y_all from the set number to the related labels using mapvalues function (plyr package)
  
  print("completing data set with subject id and activity name")
  Y_map_labels <- mapvalues(as.factor(Y_all[,1]),from = as.character(labels[,1]), to = as.character(labels[,2]))
  
  ## 4. Appropriately labels the data set with descriptive variable names. 
  
  ## Use cbind to add the name of the activities to the data set 
  x_msr <- cbind(Y_map_labels, x_msr) 
  
  ## Set a significant name to the new column
  colnames(x_msr)[1] <- "activity_name" 
  
  ## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
  ##    for each activity and each subject.
  
  ##  Use cbind to add id subjects to the data set 
  x_msr <- cbind(subject_all, x_msr)
  
  ## Set a significant name to the new column
  colnames(x_msr)[1] <- "subject_id"
  
  print("completing data set with subject id and activity name")
  
  ## Use melt and dcast functions to clean data 
  x_melted<- melt(x_msr,id.vars=c("subject_id","activity_name"))
  
  print("getting the tidy data set with the mean of each variable")
  x_cleaned <- dcast(x_melted, subject_id + activity_name ~ ..., mean)
  
  print("saving result to a txt file called cleaned_data")
  write.table(x_cleaned,"cleaned_data.txt",row.name=FALSE)

}