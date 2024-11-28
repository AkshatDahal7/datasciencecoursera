# Load necessary library
library(dplyr)

# Step 1: Merge the training and the test sets to create one data set
# Read in the data
features <- read.table("features.txt", col.names = c("index", "feature"))
activities <- read.table("activity_labels.txt", col.names = c("activityId", "activityName"))
subject_train <- read.table("train/subject_train.txt", col.names = "subject")
x_train <- read.table("train/X_train.txt", col.names = features$feature)
y_train <- read.table("train/y_train.txt", col.names = "activityId")
subject_test <- read.table("test/subject_test.txt", col.names = "subject")
x_test <- read.table("test/X_test.txt", col.names = features$feature)
y_test <- read.table("test/y_test.txt", col.names = "activityId")

# Combine training and test sets
subject <- rbind(subject_train, subject_test)
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)

# Combine subject, activity, and features into one data set
data_combined <- cbind(subject, y_data, x_data)

# Step 2: Extract only the measurements on the mean and standard deviation for each measurement
selected_features <- grep("mean\\(\\)|std\\(\\)", features$feature, value = TRUE)
data_filtered <- data_combined %>%
  select(subject, activityId, all_of(selected_features))

# Step 3: Use descriptive activity names to name the activities in the data set
data_filtered <- merge(data_filtered, activities, by = "activityId", all.x = TRUE)

# Step 4: Appropriately label the data set with descriptive variable names
names(data_filtered) <- gsub("^t", "Time", names(data_filtered))
names(data_filtered) <- gsub("^f", "Frequency", names(data_filtered))
names(data_filtered) <- gsub("Acc", "Accelerometer", names(data_filtered))
names(data_filtered) <- gsub("Gyro", "Gyroscope", names(data_filtered))
names(data_filtered) <- gsub("Mag", "Magnitude", names(data_filtered))
names(data_filtered) <- gsub("BodyBody", "Body", names(data_filtered))

# Step 5: Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
tidy_data <- data_filtered %>%
  group_by(subject, activityName) %>%
  summarise(across(where(is.numeric), mean))

# Write the tidy data set to a file
write.table(tidy_data, "tidy_data.txt", row.name = FALSE)

# Message to indicate success
cat("Tidy data set created as 'tidy_data.txt'\n")
