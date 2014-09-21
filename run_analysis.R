library(utils)
library(data.table)

if(!file.exists("./data")) {
    dir.create("./data")
}

## download and unzip data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "./data/dataset.zip"
download.file(url = fileUrl, destfile = fileName, method = "curl")
unzip(fileName)

## change working dir to folder with data
setwd("./UCI HAR Dataset/")

### 1. Merges the training and the test sets to create one data set.

## reading data from files
features <- read.table("./features.txt", header = FALSE)
activityLabels <- read.table("./activity_labels.txt", header = FALSE)
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
xTrain <- read.table("./train/X_train.txt", header = FALSE)
xTest <- read.table("./test/X_test.txt", header = FALSE)
yTrain <- read.table("./train/y_train.txt", header = FALSE)
yTest <- read.table("./test/y_test.txt", header = FALSE)

## assign column names for imported data
colnames(activityLabels) <- c("activityId", "activityType")
colnames(subjectTrain) <- c("subjectId")
colnames(subjectTest) <- c("subjectId")
colnames(xTrain) <-  features[ ,2]
colnames(xTest) <-  features[ ,2]
colnames(yTrain) <- c("activityId")
colnames(yTest) <- c("activityId")

## combining data
trainingData <- cbind(yTrain, subjectTrain, xTrain)
testData <- cbind(yTest, subjectTest, xTest)
finalData <- rbind(trainingData, testData)

colNames <- colnames(finalData)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

finalData <- finalData[logicalVector == TRUE]

### 3. Use descriptive activity names to name the activities in the data set
finalData = merge(finalData, activityLabels, by='activityId', all.x=TRUE);

### 4. Appropriately label the data set with descriptive activity names. 
colNames  = colnames(finalData); 

# Cleaning up the variable names
for (i in 1:length(colNames))
{
    colNames[i] <- gsub("\\()","",colNames[i])
    colNames[i] <- gsub("-std$","StdDev",colNames[i])
    colNames[i] <- gsub("-mean","Mean",colNames[i])
    colNames[i] <- gsub("^(t)","time",colNames[i])
    colNames[i] <- gsub("^(f)","freq",colNames[i])
    colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) <- colNames

### 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
finalDataNoActivityType <- finalData[,names(finalData) != 'activityType']
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],
                      by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),
                      mean)

tidyData <- merge(tidyData,activityLabels,by='activityId',all.x=TRUE)
write.table(tidyData, './tidy_data.txt',row.name=FALSE,sep='\t')
