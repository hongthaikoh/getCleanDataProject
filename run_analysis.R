# Author: Hong Thai Koh (26 July 2015) 
# This project is to prepare tidy data based on UCI HAR dataset
# The steps to follow are:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

#Download Data from link provided in Course Project
filesPath <- "D:/DataScience/Coursera/UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

#Unzip Data to ./data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")


#Load dplyr, data.table, tidyr packages
library(dplyr)
library(data.table)
library(tidyr)


filesPath <- "D:/DataScience/Coursera/UCI HAR Dataset/data"
#Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

#Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "y_test.txt" )))

#Read data files
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

##1. Merges the training and the test data to become one dataset.

#Rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#Join 2 sets of training and test dataset
dataTable <- rbind(dataTrain, dataTest)

#Match variables to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#Match column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

#Merge columns
alldataSubjAct <- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

##2. Extracts measurements with mean value and standard deviation value for each measurement.

#Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)

#Extracts only measurements with mean value and standard deviation value and add 2 columns "subject","activityNum"
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##3. Rename activities with user descriptive activity names in the dataset

#Merges name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

#Creates dataTable with variable means, order by Subject and Activity by using arrange
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))


##4. Rename the labels of dataset with user descriptive variable names.

#   leading t or f is based on time or frequency measurements.
#Body = related to body movement.
#Gravity = acceleration of gravity
#Acc = accelerometer measurement
#Gyro = gyroscopic measurements
#Jerk = sudden movement acceleration
#Mag = magnitude of movement
#mean and SD are calculated for each subject for each activity for each mean and SD measurements. 
#The units given are g's for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the 
#corresponding jerks.


names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))



##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#   write to text file on disk
write.table(dataTable, "new_tidy_table.txt", row.name=FALSE)


