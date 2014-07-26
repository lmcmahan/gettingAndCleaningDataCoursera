# Getting and Cleaning Data 

## Introduction
This repository is created for the final project of the 'Getting and Cleaning Data' Coursera course. The repository consists of three files: (1) README.md, (2) CodeBook.md and (1) run_analysis.R script.  The
README.md file describes how the script works and the CodeBook.md contains description of the data variables. The run_analysis.R script is used to process and analyze the data set downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip.  A full description of the data set is available at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.  Following are the steps the script performs to clean up the downloaded raw data set, to analyze the resulting cleaned data set, and to generate a tidy data set according to the class project description.

### Step 1: Loading the features, training and testing data files into data frames in R 
- The training and testing data set, each has three data files: subject (i.e. subject\_x.txt), activity (i.e. y\_x.txt) and measurements (i.e. X\_x.txt) files.  For the testing data set, use read.table() to load the subject_test.txt, y_test.txt and X_test.txt files respectively into subjectTest, yTest and xTest data frames in R.  For the training data set, use read.table() to load the subject_train.txt, y_train.txt and X_train.txt files respectively into subjectTrain, yTrain and xTrain data frames in R
- use read.table() to load the features.txt file into R data frame and extract 561 measurement variable names from the second column of the data frame

### Step 2: Label data frames with descriptive variable names
- Label the column of the subjectTest and subjectTrain data frame with "subject"
- Label the column of the yTest and yTrain data frame with "activity"
- Use the 561 measurement variable names extracted from the features.txt file in step 1 above to label the 561 columns of the xTest and xTrain data frame

### Step 3: Merge the data frames to create one testing data frame and one training data frame
- Use cbind to join subjectTest (i.e corresponding to the subject variable), yTest (i.e corresponding to the activity variable) and xTest (i.e corresponding to the 561 measurement variables) data frames horizontally to create one testing data frame
- Similarly, use cbind to join subjectTrain (i.e corresponding to the subject variable), yTrain (i.e corresponding to the activity variable) and xTrain (i.e corresponding to the 561 measurement variables) data frames horizontally to create one training data frame

### Step 4: Merge the training and testing data frames into one data set
Use rbind to join the training and testing data frames vertically to create one final data frame.  str() shows the final data frame has a total of 10299 rows and 563 variables (1 subject variable + 1 activity variable + 561 measurement variables), which agrees with the number of instances and number of measurement attributes listed at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Step 5: Extract subject, activity and only the measurement variables on mean and standard deviation from the data set
Use grep() and data frame subsetting to extract subject variable, activity variable and only the measurement variables on mean and standard deviation from the data set.  str() shows the resulting extrated data frame has a total of 10299 rows and 81 variables (down from 563 variables). 

### Step 6: Use descriptive activity names to name the activities in the data set
Obtain the descriptive activity names corresponding to the activity numbers from the downloaded 'activity\_labels.txt' file.  Use gsub() to rename all the activtiy numbers under the activity variable in the data set to their corresponding descriptive activity names (i.e.  1 to WALKING, 2 to WALKING_UPSTAIRS, 3 to WALKING_DOWNSTAIRS, 4 to SITTING, 5 to STANDING, and 6 to LAYING).

### Step 7: Create a tidy data set with the average of each measurement variable for each activity and each subject
- Use melt() to set 'subject' and 'activity' variables in the data set to id and the remaing 79 measurement variables in the data set to measure.vars - This results in a melted data frame
- Apply dcast() to the melted data frame to get the average of each measurement variable for each activity and each subject - This results in a tidy data frame.  str() shows the resulting tidy data frame has a total of 180 rows (down from 10299 rows) and 81 variables
- Use write_table() to write the tidy data frame to a output text file - This is the output tidy data set submitted for part 1

## Final Remark: How to View the Output Tindy Data Set File in R
1. Use read.table() to load the output tidy data set file into data frame in R.<br>
   e.g. tidyData <- read.table("tidy data set file name")
2. Use str() to get total number of records and variables as well as variable attributes in the tidy data set.<br>
   e.g. str(tidyData)
3. use head and tail to view a few records in the tidy data set.<br>
   e.g. head(tidyData, 3)<br>
   e.g. tail(tidyData, 3)
