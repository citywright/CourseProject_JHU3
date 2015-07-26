library(plyr)
library(reshape2)

run_analysis <- function() {

## This function is designed to do the following:

## 0. Import the data.

features <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                             stringsAsFactors=FALSE)

subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/Y_test.txt")

subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")

## 1. Merge the training and the test sets to create one data set.

data <- rbind(xTrain, xTest)
activityCodes <- rbind(yTrain, yTest)
subject <- rbind(subjectTrain, subjectTest)

## 2. Extract only the measurements on the mean and standard deviation 
##    for each measurement. 

filter <- sort(c(grep("mean()", features[,2], fixed=TRUE),
                 grep("std()",  features[,2], fixed=TRUE)))
subset <- data[,filter]

## 3. Use descriptive activity names to name the activities in the data set

activity <- join(activityCodes,activityLabels)
combined <- cbind(subject, activity[,2], subset)

## 4. Appropriately label the data set with descriptive variable names. 

colnames(combined) <- c("subject", "activity", features[filter,2])

## 5. From the data set in step 4, create a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

melted <- melt(combined, id=c("subject","activity"))
casted <- dcast(melted, subject + activity ~ variable, mean)

write.table(casted, "summary_output.txt", row.name=FALSE)

}