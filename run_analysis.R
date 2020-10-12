library(tidyverse)

### 1. Merges the training and the test sets to create one data set. ###

# import test datatset

subject_test <- read_csv("test/subject_test.txt", col_names = FALSE)

X_test <- read_table2("test/X_test.txt", col_names = FALSE)

y_test <- read_csv("test/y_test.txt", col_names = FALSE)

# import training datatset

subject_train <- read_csv("train/subject_train.txt", col_names = FALSE)

X_train <- read_table2("train/X_train.txt", col_names = FALSE)

y_train <- read_csv("train/y_train.txt", col_names = FALSE)

# merging data

colnames(subject_test) <- c("subject")
colnames(subject_train) <- c("subject")

colnames(y_test) <- c("activity_id")
colnames(y_train) <- c("activity_id")

subject <- rbind(subject_test,subject_train)
X <- rbind(X_test,X_train)
Y <- rbind(y_test,y_train)

data <- cbind(subject,Y,X)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. ###

stats <- summarise_at(data,vars(X1:X561), list( mean = mean, sd = sd))

### 3.Uses descriptive activity names to name the activities in the data set ###

activity_labels <- read_table2("activity_labels.txt", col_names = FALSE)

colnames(activity_labels) <- c("id","activity")

data <- inner_join(data, activity_labels, by = c("activity_id"="id")) %>% 
  select(subject,activity,X1:X561)


### 4.Appropriately labels the data set with descriptive variable names.

features <- read_table2("features.txt", col_names = FALSE)

colnames(features) <- c("id","libelle")

colnames(data) <- append(c("subject","activity"),paste0(features$id,features$libelle))

### 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

data_stats <- data %>% 
              group_by(subject,activity) %>% 
              summarise_all(mean, na.rm=TRUE )

write.table(data_stats, "data.txt", row.name=FALSE)  










