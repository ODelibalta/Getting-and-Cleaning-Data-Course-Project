## set the environment
setwd( "/home/odelibalta/Documents/Study/Coursera/DataScientist/3_Getting_and_Cleaning_Data/Week4/Project" )
if ( ! file.exists( "./data" ) ) 
      dir.create( "./data" )

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file( fileUrl, destfile = "./data/zipData.zip", method = "curl" )

###Unzip DataSet to /data directory
unzip(zipfile="./data/zipData.zip",exdir="./data")

## required stuff
library(data.table)
library(dplyr)
library(tidyr)


## we are going to work with the unzipped files 
## set the data files directory
## why do they put spaces in folder names ? beats me... 
filesPath <- "/home/odelibalta/Documents/Study/Coursera/DataScientist/3_Getting_and_Cleaning_Data/Week4/Project/data/UCI HAR Dataset"

## creating data frames by reading in the data
## following the file naming convention for my variables
# Read subject files
dfSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dfSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
dfActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "y_train.txt")))
dfActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "y_test.txt" )))

#Read data files.
dfTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dfTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))


## 
# 1. Merges the training and the test sets to create one data set.
## 
combinedSubject <- rbind(dfSubjectTrain, dfSubjectTest)
combinedActivity<- rbind(dfActivityTrain, dfActivityTest)
#combine the DATA training and test files
combinedTable <- rbind(dfTrain, dfTest)


# name variables according to feature  
# not a step but helps 
setnames(combinedActivity, "V1", "activityNum")
setnames(combinedSubject, "V1", "subject")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(combinedTable) <- dataFeatures$featureName

# more column labeling
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Step 1 merge training and test sets to create one data set 
alldataSubjAct<- cbind(combinedSubject, combinedActivity)
combinedTable <- cbind(alldataSubjAct, combinedTable)

##
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum" 
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
combinedTable<- subset(combinedTable,select=dataFeaturesMeanStd) 


## 
# 3. Uses descriptive activity names to name the activities in the data set
## 
combinedTable <- merge(activityLabels, combinedTable , by="activityNum", all.x=TRUE)
combinedTable$activityName <- as.character(combinedTable$activityName)

## create combinedTable with variable means sorted by subject and Activity
combinedTable$activityName <- as.character(combinedTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = combinedTable, mean) 
combinedTable<- tbl_df(arrange(dataAggr,subject,activityName))

##
# 4. Appropriately labels the data set with descriptive variable names.
##
names(combinedTable)<-gsub("std()", "SD", names(combinedTable))
names(combinedTable)<-gsub("mean()", "MEAN", names(combinedTable))
names(combinedTable)<-gsub("^t", "time", names(combinedTable))
names(combinedTable)<-gsub("^f", "frequency", names(combinedTable))
names(combinedTable)<-gsub("Acc", "Accelerometer", names(combinedTable))
names(combinedTable)<-gsub("Gyro", "Gyroscope", names(combinedTable))
names(combinedTable)<-gsub("Mag", "Magnitude", names(combinedTable))
names(combinedTable)<-gsub("BodyBody", "Body", names(combinedTable))

# for the sake of awesomeness lets see the new named data
head(str(combinedTable),6)

##
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## 
# Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() 
# using row.name=FALSE (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).
write.table(combinedTable, "TidyData.txt", row.name=FALSE)











