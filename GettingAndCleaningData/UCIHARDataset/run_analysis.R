library(dplyr)

# Load: activity labels
activityLabels <- read.table("activity_labels.txt")

# Load: data column names
features <- read.table("features.txt")

#Question 1
# Merges the training and the test sets to create one data set.
# read train and test data and labels
trainData <- read.table("train/X_train.txt")
testData <- read.table("test/X_test.txt")
trainLabels <- read.table("train/Y_train.txt")
testLabels <- read.table("test/Y_test.txt")
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
# merge the data
data <- rbind(trainData, testData)


# Question 2
# Extracts only the measurements on the mean and standard deviation for each measurement.
extract_features <- grepl("mean|std", features)
data_subset1 <- data[, extract_features]
data_subset1$subject <- rbind(subject_train, subject_test)
data_subset1$labels <- rbind(trainLabels, testLabels)


# Question 3
# Uses descriptive activity names to name the activities in the data set
names(data) <- c(features, labels)

# Question 4
# Appropriately labels the data set with descriptive variable names.
names(activityLabels) <- c("activityID", "activityLabel")

# Question 5
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each
# activity and each subject.
tidy_data <- aggregate(data_subset1[, 3:ncol(data_subset1)], by=list(subject = data_subset1$subject, 
                               label = data_subset1$labels), mean)