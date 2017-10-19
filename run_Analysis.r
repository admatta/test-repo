# function Tidyset1 takes the directory where raw data from the project
# has been extracted and return tidydata set 1 which performs the following:
# 1) Merge trainging and test data 2) Extract measurement on mean and standard
# deviations 3) Use descriptive activity names to name the activities in the 
# data set 4) Label the data set with descriptive variable names
# This program in turn calls various user defined functions which are define
# later below

Tidydataset1 <- function(workingdirectory) {
    X <- merge_data(workingdirectory)
    X <- meansd_data(X, workingdirectory)
    X <- activities(X, workingdirectory)
    return(X)
}

# functionn Tidyset2 takes the first tidyset arrived uppon from steps 1-4 of
# project and returns the average of each variable for each activity and 
# each subject and activity
# function takes 2 arguements
#       1. Data set
#       2. Working directory where your data for project has been extracted
# function returns tidy data set with the average of each variable for each 
# activity and each subject.

Tidydataset2 <- function(X, workingdirectory) {
    trainfile <- paste(workingdirectory,"/test/subject_test.txt", sep = "")
    testfile <- paste(workingdirectory,"/train/subject_train.txt", sep="")
    trainsubject <- read.table(trainfile)
    testsubject <- read.table(testfile)
    Y <- rbind(trainsubject, testsubject)
    X$subject <- Y[,1]
    Z <- group_by(X, activity, subject) %>% summarise_all(funs(mean))
    return(Z)
}

# This function merges the train and test data
# it takes working directory where the raw data set is extracted locally as 
# an arguement
merge_data <- function(workingdirectory) {
    trainfile <- paste(workingdirectory, "/train", "/X_train.txt", sep="")
    testfile <- paste(workingdirectory, "/test", "/X_test.txt", sep="")
    X_train <- read.table(trainfile)
    X_test <- read.table(testfile)
    X <- rbind(X_train, X_test)
    return(X)
}

# This function extracts only the measurements on the mean and standard
# deviation for each measurement. It also puts the descriptive variable names
# function takes merged data set and working directory as arguements
meansd_data <- function(X, workingdirectory) {
    featurefile <- paste(workingdirectory,"/features.txt", sep="")
    features <- read.table(featurefile)
    meansdcolIndex <- c(grep("mean()", features[,2], fixed = TRUE),grep("std()", 
                                            features[,2], fixed = TRUE))
    Xmeansd <- X[,meansdcolIndex]
    colnames(Xmeansd) <- c(grep("mean()", features[,2], value = TRUE, 
        fixed = TRUE),grep("std()", features[,2], value = TRUE, fixed = TRUE))
    return(Xmeansd)
}

# This function adds the merge the activity column to the measurement data set
# and also replace the activity codes with the descriptive activity codes
activities <- function(X, workingdirectory) {
    trainfile <- paste(workingdirectory, "/train", "/y_train.txt", sep="")
    testfile <- paste(workingdirectory, "/test", "/y_test.txt", sep="")
    activitylabelsfile <- paste(workingdirectory, "/activity_labels.txt", sep = "")
    y_train <- read.table(trainfile)
    y_test <- read.table(testfile)
    activitylabels <- read.table(activitylabelsfile)
    y <- rbind(y_train, y_test)
    X$activity <- y[,1]
    X$activity <- activitylabels[match(X$activity, activitylabels$V1),2]
    return(X)
}