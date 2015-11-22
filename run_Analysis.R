#Read in column headers for x_train and x_test
varnames <- read.csv("./UCI HAR Dataset/features.txt", sep = " ", header = FALSE, stringsAsFactors = FALSE)
#Vectorizes variable names
vnames <- varnames[, 2]

#Read in training datasets
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/x_train.txt", header = FALSE, col.names = vnames) 
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
#put training datasets together
train <- cbind(subject_train, y_train, x_train)

#Read in test datasets
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("./UCI HAR Dataset/test/x_test.txt", header = FALSE, col.names = vnames)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
#put test datasets together
test <-cbind(subject_test, y_test, x_test)

#combine test and training datasets
full <- rbind(test, train)
#dimensions of full = 10299, 563

#only keep columns with mean or std in the name-- I included cols with "meanFreq" because it was not clear
#that we should exclude them
fnames <- names(full)
meancols <- grep("mean", fnames)
justmeans <- full[, meancols]
#justmeans contains subject and activity column
stdcols <- grep("std", fnames)
juststds <- full[, stdcols]

#keep activity and subject column from full
subact <- full[, 1:2]

#combine columns again, should have 33 (std) + 46 (means) + 1(subject) + 1(activity) = 81
meansstds <- cbind(subact, justmeans, juststds) 
#10299 obs, 81 vars

#use dataset to label activities
labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
merged <- merge(meansstds, labels, by.x = "V1.1", by.y = "V1", all = TRUE)
#remove redundant information, now that the datasets are merged
merged$V1.1 <- NULL
#There are no variable names duplicated.
merged_df <- tbl_df(merged)
#I renamed these variables to remove the duplication of Body in the variable name.
#I thought about renaming the other variables to be more easily understood, but
#I don't think it can be more easily understood without making the variable
# names much longer than they already are. 
merged_df <- rename(merged_df, Subject = V1, Activity = V2, 
                    fBodyAccJerkMag.mean.. = fBodyBodyAccJerkMag.mean.., 
                    fBodyAccJerkMag.meanFreq..= fBodyBodyAccJerkMag.meanFreq..,
                    fBodyGyroMag.mean.. = fBodyBodyGyroMag.mean..,
                    fBodyGyroMag.meanFreq.. = fBodyBodyGyroMag.meanFreq..,
                    fBodyGyroJerkMag.mean.. = fBodyBodyGyroJerkMag.mean..,
                    fBodyGyroJerkMag.meanFreq.. = fBodyBodyGyroJerkMag.meanFreq..)
                     

#This creates a dataset, grouped by Activity and Subject
merged_group <- group_by(merged_df, Activity, Subject)
#This will create a new dataset that averages the values in each column by Activity and Subject
merged_group_means <- summarise_each(merged_group, funs(mean), 2:78)