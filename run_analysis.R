## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## libraries
library(plyr)
library(reshape2)

## set working directory
setwd("~/R/Coursera/Project3")

## input files
X_testFile <- "./UCI HAR Dataset/test/X_test.txt"                   # test data
X_trainFile <- "./UCI HAR Dataset/train/X_train.txt"                # train data
y_testFile <- "./UCI HAR Dataset/test/y_test.txt"                   # test activity data
y_trainFile <- "./UCI HAR Dataset/train/y_train.txt"                # train activity data
subject_testFile <- "./UCI HAR Dataset/test/subject_test.txt"       # test subject data
subject_trainFile <- "./UCI HAR Dataset/train/subject_train.txt"    # train subject data
featuresFile <- "./UCI HAR Dataset/features.txt"                    # variable names
activityLabelsFile <- "./UCI HAR Dataset/activity_labels.txt"       # activity labels

## load variable names
features <- read.table(featuresFile, sep=" ")

## load test and train data
what <- rep(list(numeric()),561)
ltestData <- scan(X_testFile, what=what)
testData <- as.data.frame(ltestData, optional=TRUE)
names(testData) <- features$V2              # set data variable names from features set

ltrainData <- scan(X_trainFile, what=what)
trainData <- as.data.frame(ltrainData, optional=TRUE)
names(trainData) <- features$V2             # set data variable names from features set

testActData <- read.fwf(y_testFile, widths=1)
trainActData <- read.fwf(y_trainFile, widths=1)
testSubjectData <- read.fwf(subject_testFile, widths=2)
trainSubjectData <- read.fwf(subject_trainFile, widths=2)
actLabelsData <- read.table(activityLabelsFile, sep=" ")

## merge data files
allData <- rbind(testData, trainData)
allActData <- rbind(testActData, trainActData)
allSubjectData <- rbind(testSubjectData, trainSubjectData)

## extract columns from data set which contains "mean()" or "std()" in column name
cnames <- colnames(allData)
with_mean <- cnames %in% grep("mean()", cnames, fixed=TRUE, value=TRUE)     # vector of column names with "mean()"
with_std <- cnames %in% grep("std()", cnames, fixed=TRUE, value=TRUE)       # vector of column names with "std()"
req_cols <- with_mean | with_std                                    # vector of column names with "mean()" or "std()"

subData <- allData[ ,req_cols]              # subset of the original data set by column names

## add activity column into data set
names(allActData)[1] <- "actCode"           # rename column
subData <- cbind(allActData, subData)

## join data set with activity labels set
names(actLabelsData) <- c("actCode","actLabel")    # rename columns
subData <- merge(subData, actLabelsData, by.x="actCode", by.y="actCode")

## add subject column into data set
names(allSubjectData) <- "subject"          # rename column
subData <- cbind(allSubjectData, subData)

## reshape data set
meltData <- melt(subData, id.vars=c("actCode","actLabel","subject"))

## calculate mean for every activity and subject
meanData <- ddply(meltData, c("actCode", "actLabel", "subject", "variable"), summarise, mean=mean(value))

## write the result set into file
write.table(meanData, file="./result.txt", row.names=FALSE)