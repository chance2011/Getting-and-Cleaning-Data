# Code Book

## Steps to cleaning the data.
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the dataset.
4. Appropriately labels the dataset with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


## 1. Merges the training and the test sets to create one data set.
* alldtSubj : bind all subject training and test data
* alldtAct  : bind all activity training and test data
* dtTable : bind training and test files
* dtFeat : name varibales according to the feature.txt file
* actLabel : indentify the column names for activity labels.txt files
* alldtSubjAct : merge all columns in the dataset 
* dtTable : call merged columns to new dataset called "dtTable"

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
* dtFeatMeanStd : reads the "features.txt" and extract the mean and standard deviation and add to the "dtTable"

## 3. Uses descriptive activity names to name the activities in the dataset.
This section purpose is to add the name of each activitey to the "dtTable" 

## 4. Appropriately labels the dataset with descriptive variable names.
* names(dtTable) : adjust the names in "dtTable" to read clearly

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
* dtTabletidy : group the data by subject and activity
* tidymean : calculate the mean using summarise function
