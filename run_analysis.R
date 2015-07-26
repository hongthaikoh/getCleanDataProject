# Author: Hong Thai Koh (26 July 2015) 
# This project is to prepare tidy data based on UCI HAR dataset
# The steps to follow are:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

# load dplyr package
library(dplyr)

# merge train_X and train_Y to train_Data
setwd("d:/DataScience/Coursera/UCI HAR Dataset/train")
file_list_train <- list.files(pattern=".*.txt")
train_data <- do.call(cbind, lapply(file_list_train, read.table))

# merge test_X and test_Y to test_Data
setwd("d:/DataScience/Coursera/UCI HAR Dataset/test")
file_list_test <- list.files(pattern=".*.txt")
test_data <- do.call(cbind, lapply(file_list_test, read.table))

#merge train_Data and test_Data to become dataset_UCI
dataset_UCI <- rbind(train_data, test_data)

#retrieve mean and std deviation for all rows
apply(train_data, 1, mean)
apply(train_data, 1, sd)
apply(test_data, 1, mean)
apply(test_data, 1, sd)

#appropriately match value 1-6 in train_Y, test_Y data with activity name
dataset_UCI$V1[dataset_UCI$V1 == 1] <- "WALKING"
dataset_UCI$V1[dataset_UCI$V1 == 2] <- "WALKING UPSTAIRS"
dataset_UCI$V1[dataset_UCI$V1 == 3] <- "WALKING DOWNSTAIRS"
dataset_UCI$V1[dataset_UCI$V1 == 4] <- "SITTING"
dataset_UCI$V1[dataset_UCI$V1 == 5] <- "STANDING"
dataset_UCI$V1[dataset_UCI$V1 == 6] <- "LAYING"

#reading column name from features.txt
features <- read.table("d:/DataScience/Coursera/UCI HAR Dataset/features.txt")
feature_subject <- rbind(features[,c(1,2)], matrix(c(562, "activity", 563, "subject"), nrow = 2, byrow = TRUE))

#bind feature_subject to column names
colnames(dataset_UCI) <- feature_subject[,2]

#transform new tidy data by grouping mean value according to activity
activity_mean <- aggregate(dataset_UCI$activity, dataset_UCI, mean)

#transform new tidy data by grouping mean value according to subject
subject_mean <- aggregate(activity_mean$subject, activity_mean, mean)
new_tidy_table <- subject_mean[,c(564, 565)]

#export tidy data to text file
write.table(new_tidy_table, file = "d:/DataScience/Coursera/new_tidy_table.txt", row.names = F, quote = F)








