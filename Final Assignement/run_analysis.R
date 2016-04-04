## Getting and Cleaning Data course as part of Data Science Program by Johns Hopkins University
## Assigment week 4. Work with real data from Samsung accelerometers
## Date: Apr 2 2016

## Files have been downloaded and place in the working directory
## Data can be downloaded from 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## STEPS #####
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set
##     with the average of each variable for each activity and each subject.

## Loading Libraries
library(dplyr)

## Setting-up working directory
## References to personal information is removed

wdpath <- "D:/Users/xxx/Documents/yyy/Big data/Data Science Program/3. Getting and Cleaning Data/w4"

setwd(wdpath)

## Data files Directory is -UCI HAR Dataset-
FileDir <- "./UCI HAR Dataset"

##0. Reading the files Subject and Activities from Train and Test from the respective directories
subjecttrainfile <- paste(FileDir,"/train/subject_train.txt",sep="")
subjecttestfile <- paste(FileDir,"/test/subject_test.txt",sep="")
activitytrainfile <- paste(FileDir,"/train/y_train.txt",sep="")
activitytestfile <- paste(FileDir,"/test/y_test.txt",sep="")
trainfile <- paste(FileDir,"/train/x_train.txt",sep="")
testfile <- paste(FileDir,"/test/x_test.txt",sep="")


## Subject
dataframeSubjectTrain <- data.table(read.table(subjecttrainfile))
dataframeSubjectTest <- data.table(read.table(subjecttestfile))
## Activity
dataframeActivityTrain <- data.table(read.table(activitytrainfile))
dataframeActivityTest <- data.table(read.table(activitytestfile))
## Data
dataframeTrain <- data.table(read.table(trainfile))
dataframeTest <- data.table(read.table(testfile))

## Files Read

## 1. Merge Train and Test Files
dataframeTotalSubject <- rbind(dataframeSubjectTrain,dataframeSubjectTest)
dataframeTotalActivity <- rbind(dataframeActivityTrain,dataframeActivityTest)
dataframeTotalData <- rbind(dataframeTrain,dataframeTest)

## before merging the columns, rename the 1st columns of Subject and Activity df
setnames(dataframeTotalSubject,"V1","subjectid")
setnames(dataframeTotalActivity,"V1","activityid")

dfMerged <- cbind(dataframeTotalSubject,dataframeTotalActivity,dataframeTotalData)

## End of 1. Files Merged into 1 dataframe


## 2. Extract the measurements on the mean and std deviation for each measurement.
## In features.txt it is found the list of measurements, therefore it is required
## to get a vector of the elements that are for mean or std and match with dfMerged
featuresfile <- paste(FileDir,"/features.txt",sep="")
dataframeMeasures <- data.table(read.table(featuresfile))

## Set meaningful column names
setnames(dataframeMeasures,names(dataframeMeasures),c("measure.num","measure.name"))

## Filter those columns with std() and mean() and update the dataframemeasures
dataframeMeasures <- dataframeMeasures[grepl("mean\\(\\)|std\\(\\)",measure.name)]

## Add a column with the column corresponding column name in the format "V"+"measure.num"
dataframeMeasures$columnheadername <- dataframeMeasures[,paste0("V", measure.num)]

## Filter the columns
columnstoextract <- c("subjectid","activityid",dataframeMeasures$columnheadername)
dfMergedExt <- dfMerged[,columnstoextract,with=FALSE]


## End of 2. Extract mean and std


## 3. Add descriptive activity names to name activities in the data set.
activitiesfile <- paste(FileDir,"/activity_labels.txt",sep="")
dataframeActivities <- data.table(read.table(activitiesfile))
##setting meaningful names
setnames(dataframeActivities,names(dataframeActivities),c("activityid","activityname"))

##merging the dataframes
dfMergedExtAct <- merge(dfMergedExt,dataframeActivities,by="activityid",all.x = TRUE)

## Removing activityid
dfMergedExtAct <- select(dfMergedExtAct,-c(1))

## Moving Activity names, Position(68) at the beggining of the dataframe
dfMergedExtAct <- select(dfMergedExtAct,c(68),everything())

## End of 3. Add descriptive activity names


## 4. Set meaningful names
setnames(dfMergedExtAct,names(dfMergedExtAct),c("activityname","subjectid",as.character(dataframeMeasures$measure.name)))

## End of 4. Appropriate names for the variables


## 5. Create a new tydy data set with the average of each variable for each activity and each subject

dfAverage <- dfMergedExtAct %>% 
   group_by(activityname,subjectid) %>%
    summarize_each(funs(mean(.,na.rm=TRUE)))

## End of 5. Create a new tydy data set with the averages
