## Create one R script called run_analysis that does the following.

## 1. Merges the training and the test sets to create one data set.

## 2. Extracts only the measurements on the mean and standard deviation
##for each measurement.

## 3. Uses descriptive activity names to name the activities in the dataset.

## 4. Appropriately labels the dataset with descriptive variable names.

## 5. From the data set in step 4, creates a second, independent tidy data 
##set with the average of each variable for each activity and each subject.

##Download files from the data website
fileName <- "UCIdata.zip"
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Dataset"

## If file does not exist, download to working directory
if(!file.exists(fileName)){
  download.file(url,fileName, mode = "wb") 
}

##Unzip DataSet to the data directory if the directory does not exist
if(!file.exists(dir)){
  unzip("UCIdata.zip", files = NULL, exdir=".")
}

##Load packages needed for script
library(dplyr)
library(data.table)
library(tidyr)

##Change the file path to data/UCI HAR Dataset where the data is located
filePath <- "./data/UCI HAR Dataset"

##Read the subject files
dtSubjTrain <- tbl_df(read.table(file.path(filePath, "train", "subject_train.txt")))
dtSubjTest  <- tbl_df(read.table(file.path(filePath, "test" , "subject_test.txt" )))

##Read the activity files
dtActTrain <- tbl_df(read.table(file.path(filePath, "train", "Y_train.txt")))
dtActTest  <- tbl_df(read.table(file.path(filePath, "test" , "Y_test.txt" )))

##Read the data files
dtTrain <- tbl_df(read.table(file.path(filePath, "train", "X_train.txt" )))
dtTest  <- tbl_df(read.table(file.path(filePath, "test" , "X_test.txt" )))

#### 1. Merges the training and the test sets to create one data set.
##Merge the subject and activity files for the training and the test sets by row.

##The following will bind and rename the variables as "subj" and "actNum"
alldtSubj <- rbind(dtSubjTrain, dtSubjTest)
setnames(alldtSubj, "V1", "subj")
alldtAct <- rbind(dtActTrain, dtActTest)
setnames(alldtAct, "V1", "actNum")

##Combine the data in the training and test files
dtTable <- rbind(dtTrain, dtTest)

##Name variables according to the feature.txt
dtFeat <- tbl_df(read.table(file.path(filePath, "features.txt")))
setnames(dtFeat, names(dtFeat), c("featureNum", "featureName"))
colnames(dtTable) <- dtFeat$featureName

##Identify the column names for activity labels .txt file
actLabel <- tbl_df(read.table(file.path(filePath, "activity_labels.txt")))
setnames(actLabel, names(actLabel), c("actNum","actName"))

##Merge columns in the dataset
alldtSubjAct<- cbind(alldtSubj, alldtAct)
dtTable <- cbind(alldtSubjAct, dtTable)

#### 2. Extracts only the measurements on the mean and standard deviation
##for each measurement.

##Read the "features.txt" and extract the mean and standard deviation
dtFeatMeanStd <- grep("mean\\(\\)|std\\(\\)", dtFeat$featureName, value=TRUE)

##Take the mean and standard deviation measurements and add to "subj","actNum"
dtFeatMeanStd <- union(c("subj", "actNum"), dtFeatMeanStd)
dtTable <- subset(dtTable, select = dtFeatMeanStd)

#### 3. Uses descriptive activity names to name the activities in the dataset.

##Add the name of each activity to the data table
dtTable <- merge(actLabel, dtTable, by = "actNum", all.x = TRUE)
dtTable$actName <- as.character(dtTable$actName)
dtTable <- select(dtTable, -actNum)

##Add variable means sorted by subject and activity to the data table
dtTable$actName <- as.character(dtTable$actName)
dtAggr <- aggregate(. ~ subj - actName, data = dtTable, mean) 
dtTable <- tbl_df(arrange(dtAggr, subj, actName))

## Review labels for possible edits
head(str(dtTable),2)

#### 4. Adjust labels in the dataset with more easier to read descriptive 
##variable names.

names(dtTable) <- gsub("std()", "sd", names(dtTable))
names(dtTable) <- gsub("mean()", "mean", names(dtTable))
names(dtTable) <- gsub("^t", "time", names(dtTable))
names(dtTable) <- gsub("^f", "freq", names(dtTable))
names(dtTable) <- gsub("Acc", "Accelerometer", names(dtTable))
names(dtTable) <- gsub("Gyro", "Gyroscope", names(dtTable))
names(dtTable) <- gsub("Mag", "Magnitude", names(dtTable))
names(dtTable) <- gsub("BodyBody", "Body", names(dtTable))
names(dtTable) <- gsub("-", " ", names(dtTable))
names(dtTable) <- gsub("\\.", " ", names(dtTable))
names(dtTable) <- gsub("\\  ", " ", names(dtTable))
names(dtTable) <- gsub("\\  ", " ", names(dtTable))
names(dtTable) <- gsub("\\  ", " ", names(dtTable))
names(dtTable) <- gsub("tBody", "Body", names(dtTable))
names(dtTable) <- gsub("tGravity", "Gravity", names(dtTable))
names(dtTable) <- gsub("fBody", "Body", names(dtTable))
names(dtTable) <- gsub("BodyBody", "Body", names(dtTable))
names(dtTable) <- gsub("^\\s+|\\s+$", "", names(dtTable))

## Check labels after edits for clairity
head(str(dtTable),6)

#### 5. From the data set in step 4, creates a second, independent tidy data 
##set with the average of each variable for each activity and each subject.

# Group the data by subject and activity
dtTabletidy <- group_by(dtTable, subj, actName)

# Calculate the mean
tidymean <- summarise_all(dtTabletidy, funs(mean))

# Reapply the edited labels
colnames(tidymean) <- names(dtTable)

# Review first 5 rows and columes
tidymean[1:15, 1:5]

## Create tidy table
write.table(tidymean, file = "UCI_HAR_TidyData.txt", row.name=FALSE)

