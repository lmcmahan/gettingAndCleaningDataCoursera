library(reshape2)

# Load features dataset into R data frame and extract the features
featuresTxt <- read.table("features.txt")
features <- featuresTxt[,2]

# Load 3 test datasets into R data frames
subjectTest <- read.table("./test/subject_test.txt")
yTest <- read.table("./test/y_test.txt")
xTest <- read.table("./test/X_test.txt")

# Label subject, y and x data frames with descriptive variable names (Add column names to subject and y data frames)
colnames(subjectTest) <- "subject"
colnames(yTest) <- "activity"
colnames(xTest) <- features
                       
# Join those 3 test data frames horizontally
testData <- cbind(subjectTest, yTest, xTest)

# Process 3 train datasets the same way as processing the 3 test datasets described above
subjectTrain <- read.table("./train/subject_train.txt")
yTrain <- read.table("./train/y_train.txt")
xTrain <- read.table("./train/X_train.txt")
colnames(subjectTrain) <- "subject"
colnames(yTrain) <- "activity"
colnames(xTrain) <- features
trainData <- cbind(subjectTrain, yTrain, xTrain)

# Merge train and test datasets
mergeTrainTestData <- rbind(trainData, testData)

# Extract subject, activity, mean and std columns from the mergeTrainTest dataset
means <- grep("mean",names(mergeTrainTestData))
stds <- grep("std",names(mergeTrainTestData))
means_stds = c(means, stds)
subject_activity <- c(1, 2)
subject_activity_mean_std <- sort(c(subject_activity,means_stds))
mergeTrainTestData_means_stds <- mergeTrainTestData[,subject_activity_mean_std]

# Uses descriptive activity names to name the activities in the data set
mergeTrainTestData_means_stds$activity <- gsub("1", "WALKING", mergeTrainTestData_means_stds$activity)
mergeTrainTestData_means_stds$activity <- gsub("2", "WALKING_UPSTAIRS", mergeTrainTestData_means_stds$activity)
mergeTrainTestData_means_stds$activity <- gsub("3", "WALKING_DOWNSTAIRS", mergeTrainTestData_means_stds$activity)
mergeTrainTestData_means_stds$activity <- gsub("4", "SITTING", mergeTrainTestData_means_stds$activity)
mergeTrainTestData_means_stds$activity <- gsub("5", "STANDING", mergeTrainTestData_means_stds$activity)
mergeTrainTestData_means_stds$activity <- gsub("6", "LAYING", mergeTrainTestData_means_stds$activity)

# Creates a tidy data set with the average of each variable for each activity and each subject
measureVars <- names(mergeTrainTestData_means_stds)[c(3:81)]
meltTrainTestData <- melt(mergeTrainTestData_means_stds,id=c("subject", "activity"),measure.vars=measureVars)
tidyTrainTestData <- dcast(meltTrainTestData, subject + activity ~ variable, mean)
write.table(tidyTrainTestData, "tidyTrainTestData.txt")
