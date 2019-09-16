#download dataset
library(dplyr)
name <- "Coursera_final"
if(!file.exists(name)) {
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", name, method = "curl")
}
if(!file.exists("UCI HAR Dataset")) {
  unzip(name)
}

#Assign each data to variables
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activity <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
data_train_x <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
data_train_y <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
data_train_Subject <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
data_test_x <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
data_test_y <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
data_test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

#Merges the training and the test sets to create one data set
X <- rbind(data_train_x, data_test_x)
Y <- rbind(data_train_y, data_test_y)
subject <- rbind(data_train_Subject, data_test_subject)
merged_data <- cbind(subject, X, Y)

#Extracts only the measurements on the mean and standard deviation for each measurement. 

Mean_std_data <- merged_data %>% select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set

Mean_std_data$code <- activity[Mean_std_data$code, 2]

#Appropriately labels the data set with descriptive variable names. 

names(Mean_std_data)[2] = "activity"
names(Mean_std_data) <- gsub("Acc", "Accelerometer", names(Mean_std_data))
names(Mean_std_data)<-gsub("Gyro", "Gyroscope", names(Mean_std_data))
names(Mean_std_data)<-gsub("BodyBody", "Body", names(Mean_std_data))
names(Mean_std_data)<-gsub("Mag", "Magnitude", names(Mean_std_data))
names(Mean_std_data)<-gsub("^t", "Time", names(Mean_std_data))
names(Mean_std_data)<-gsub("^f", "Frequency", names(Mean_std_data))
names(Mean_std_data)<-gsub("tBody", "TimeBody", names(Mean_std_data))
names(Mean_std_data)<-gsub("-mean()", "Mean", names(Mean_std_data), ignore.case = TRUE)
names(Mean_std_data)<-gsub("-std()", "STD", names(Mean_std_data), ignore.case = TRUE)
names(Mean_std_data)<-gsub("-freq()", "Frequency", names(Mean_std_data), ignore.case = TRUE)
names(Mean_std_data)<-gsub("angle", "Angle", names(Mean_std_data))
names(Mean_std_data)<-gsub("gravity", "Gravity", names(Mean_std_data))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Tidy_data <- Mean_std_data %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Tidy_data, "TidyData.txt", row.names = FALSE)
