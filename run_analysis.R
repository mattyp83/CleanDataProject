# 1. Merges the training and the test sets to create one data set.

a <- read.table("train/X_train.txt")
b <- read.table("test/X_test.txt")
X <- rbind(a, b)

a <- read.table("train/subject_train.txt")
b <- read.table("test/subject_test.txt")
S <- rbind(a, b)

a <- read.table("train/y_train.txt")
b <- read.table("test/y_test.txt")
Y <- rbind(a, b)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
good_features_indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, good_features_indices]
names(X) <- features[good_features_indices, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
#names(X) <- tolower(names(X))  

# 3. Uses descriptive activity names to name the activities in the data set

acts <- read.table("activity_labels.txt")
acts[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = acts[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = acts[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_avgs.txt", row.name=FALSE)