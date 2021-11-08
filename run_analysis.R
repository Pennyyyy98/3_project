## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## load packages
library(data.table)
library(reshape2)
## get the data
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

## load activity labels and features
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index","featureName"))
## extract the mean and std
featureWanted <- grep("(mean|std)\\(\\)", features[, featureName])
measurements <- features[featureWanted, featureName]
measurements <- gsub("\\(\\)", "", measurements)

## load train datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("subjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

## load test  datasets
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("subjectNum"))
test <- cbind(testSubjects, testActivities, test)

## merge
all <- rbind(train, test)

## convert classLabels to activityName
all[["activity"]] <- factor(all[,activity]
                            , levels = activityLabels[["classLabels"]]
                            , labels = activityLabels[["activityName"]])
all[["subjectNum"]] <- as.factor(all[,subjectNum])
all <- reshape2::melt(data = all, id = c("subjectNum", "activity"))
all <- reshape2::dcast(data = all, subjectNum + activity ~ variable, fun.aggregate = mean)

data.table::fwrite(x = all, file = "tidyData.txt", quote = FALSE)