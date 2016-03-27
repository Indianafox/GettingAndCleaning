# Create a single tidy data frame from a test and train data frame then 
# create a further data frame that contains the mean for each subject
# by each activity for each column that represented a mean or standard
# deviation in the original data frame.

# Install and load dplyr package
install.packages("dplyr")
library(dplyr) # Used for piping, group_by and summarise_each

setwd("/Users/Documents/Coursera")

# Read in data containing column labels Raw data 
features <- read.table("features.txt")

# Read in Raw test data
X_test <- read.table("./test/X_test.txt") # test data
colnames(X_test) <- features$V2 # Allocate column names from features data table
Y_test <- read.table("./test/Y_test.txt")
colnames(Y_test) <- "Activity_Id" # eg. Walking
Combined_test <- cbind(Y_test,X_test)
subject_test <- read.table("./test/subject_test.txt")
colnames(subject_test) <- "Subject_Id"
Combined_test <- cbind(subject_test,Combined_test) # Add Subject Id column to start of Combined Test

# Read in Raw Train data
X_train <- read.table("./train/X_train.txt")
colnames(X_train) <- features$V2 # Allocate column names
Y_train <- read.table("./train/Y_train.txt")
colnames(Y_train) <- "Activity_Id"
Combined_train <- cbind(Y_train,X_train)
subject_train <- read.table("./train/subject_train.txt")
colnames(subject_train) <- "Subject_Id" # eg. Walking
Combined_train <- cbind(subject_train,Combined_train) # Add Subject Id column to start of Combined Train

# Join Combined_train with Combined_test dataset
Combined <- rbind(Combined_train,Combined_test)

# Read activity labels table
activity_labels <- read.table("activity_labels.txt")
colnames(activity_labels) <- c("Activity_Id","activity") # Add descriptive column name
# Create new column which contains a descriptive activity value based on the Activity ID column
Combined$Activity <- activity_labels[match(Combined$Activity_Id,activity_labels$Activity_Id),"activity"]
# Remove obsolete Activity_ID column from Combined data
Combined$Activity_Id <- NULL
#Combined <- sort(Combined,Combined$Activity_Id)

# Extract into new data frame, from Combined data, only the measurements on the mean and 
# standard deviation for each observation but still retain Subject Id and activity variables
Combined_Reduced_Cols <- Combined[,grepl("Subject_Id|Activity$|.mean().|.std().",colnames(Combined))]

# Reorder columns
Combined_Reduced_Cols <- Combined_Reduced_Cols[,c(1,81,2:80)]

# Output to a CSV file
write.table(Combined_Reduced_Cols,"Combined_Reduced_Cols.csv", row.names = FALSE)
# Rename column names to make them more descriptive

# Columns names starting with 't' renamed to start with 'time_'
colnames(Combined_Reduced_Cols) <- gsub("^t","time_",colnames(Combined_Reduced_Cols))
# Columns names starting with 'f' renamed to start with 'freq_'
colnames(Combined_Reduced_Cols) <- gsub("^f","freq_",colnames(Combined_Reduced_Cols))
# Columns names containing 'Acc' renamed to contain '_Accelerometer_'
colnames(Combined_Reduced_Cols) <- gsub("yAcc","y_Accelerometer_",colnames(Combined_Reduced_Cols))
# Columns names containing 'Gyro' renamed to contain '_Gyroscope_'
colnames(Combined_Reduced_Cols) <- gsub("yGyro","y_Gyroscope_",colnames(Combined_Reduced_Cols))
# Columns names containing 'Mag' renamed to contain 'Magnitude'
colnames(Combined_Reduced_Cols) <- gsub("Mag","_Magnitude",colnames(Combined_Reduced_Cols))
# Convert hyphens to underscores in all column names for better readability
colnames(Combined_Reduced_Cols) <- gsub("-","_",colnames(Combined_Reduced_Cols))
# Remove duplicate column name underscores in all column names for better readability
colnames(Combined_Reduced_Cols) <- gsub("__","_",colnames(Combined_Reduced_Cols))
# Remove column name parentheses for better readability
colnames(Combined_Reduced_Cols) <- gsub("\\(\\)","",colnames(Combined_Reduced_Cols))

# Produce average of each variable for each activity and each subject
Summary_Data <- Combined_Reduced_Cols %>% group_by(Subject_Id,Activity) %>%
                summarise_each(funs(mean))

# Output to a CSV file
write.table(Summary_Data,"Summary_Data.csv", row.names = FALSE)
