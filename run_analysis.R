## run_analysis.R
## SB2015student
library (plyr)
library (dplyr)

##### STEP 1 : READ and LEARN the common files for measurement names and activity labels
## read activity Labels
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
## read Features file into list  so we have measurement names
featuresData <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
colNameData <- featuresData$V2
## select names of colums with mean or std in them
colsWithMean <- colNameData [grep  ( "mean" , colNameData)]
colsWithStd <- colNameData [grep  ( "std" , colNameData)]

#####  STEP 2 : READ and CLEAN the data in the "test" folder
## read measurement flat file into data table
data <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
## assign the list to table column names 
colnames(data) <- colNameData
## read subject data and Activity Data
Subject<- scan("./UCI HAR Dataset/test/subject_test.txt", what="", sep="\n")
activityData<- scan("./UCI HAR Dataset/test/Y_test.txt", what="", sep="\n")
Activity <- revalue (activityData, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
## Combine the Subject and Activity Data with measurements
AllData <- cbind ( Subject, Activity, data)
## Coerce to data frame
myDF <- as.data.frame(AllData) 
## subset data for Means and Standard Deviations 
mySubset <- myDF[,c(1,2, which ((names(AllData) %in% colsWithMean) | (names(AllData) %in% colsWithStd)))]

#####  STEP 3 : READ and CLEAN the data in the "train" folder
## read measurement flat file into data table
data2 <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)
## assign the list to table column names 
colnames(data2) <- colNameData
## read subject data and Activity Data
Subject<- scan("./UCI HAR Dataset/train/subject_train.txt", what="", sep="\n")
activityData<- scan("./UCI HAR Dataset/train/Y_train.txt", what="", sep="\n")
Activity <- revalue (activityData, c("1"="WALKING", "2"="WALKING_UPSTAIRS", "3"="WALKING_DOWNSTAIRS", "4"="SITTING", "5"="STANDING", "6"="LAYING"))
## Combine the Subject and Activity Data with measurements
AllData2 <- cbind ( Subject, Activity, data2)
## Coerce to data frame
myDF2 <- as.data.frame(AllData2) 
## subset data for Means and Standard Deviations 
mySubset2 <- myDF2[,c(1,2, which ((names(AllData) %in% colsWithMean) | (names(AllData) %in% colsWithStd)))]

#####  STEP 4 : COMBINE the two clean datasets and summarize
## rbind the 2 datasets
myData <- rbind(mySubset, mySubset2)
## Assign readable and understandable Column Names
names(myData) <- gsub ("-mean()-", "Mean" , names(myData) , fixed=TRUE)
names(myData) <- gsub ("-mean()", "Mean" , names(myData) , fixed=TRUE)
names(myData) <- gsub ("-std()-", "Std" , names(myData) , fixed=TRUE)
names(myData) <- gsub ("-std()", "Std" , names(myData) , fixed=TRUE)
names(myData) <- gsub ("-meanFreq()-", "MeanFreq" , names(myData) , fixed=TRUE)
names(myData) <- gsub ("-meanFreq()", "MeanFreq" , names(myData) , fixed=TRUE)
## Summarize data
summaryData <- myData %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
## write output file
write.csv( summaryData, file="summaryData.csv",row.names=FALSE)
write.table (summaryData, file="summaryData.txt", eol = "\n",row.names=FALSE)