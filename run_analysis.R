library(plyr)
#1. Merges the training and the test sets 

## load data
X_test = read.table("UCI\ HAR\ Dataset/test/X_test.txt")
Y_test = read.table("UCI\ HAR\ Dataset/test/Y_test.txt")
subject_test = read.table("UCI\ HAR\ Dataset/test/subject_test.txt")
X_train = read.table("UCI\ HAR\ Dataset/train/X_train.txt")
Y_train = read.table("UCI\ HAR\ Dataset/train/Y_train.txt")
subject_train = read.table("UCI\ HAR\ Dataset/train/subject_train.txt")

## merge horizontally
test_data = cbind(subject_test,Y_test,X_test)
train_data = cbind(subject_train,Y_train,X_train)

## merge vertically
data = rbind(train_data,test_data)

#2.extracts only the measurements on the mean and standard deviation for each measurement

## load features
features = read.table("UCI\ HAR\ Dataset/features.txt",col.names = c("idx","measurements"),stringsAsFactors = F)
off_set = 2

## find column index of mean and std measures
idx_mean = grep("\\bmean()\\b",features$measurements) #exclude meanFreq()
idx_std = grep("\\bstd()\\b",features$measurements)
idx = sort(append(idx_mean,idx_std))
col_to_extract = append(c(1,2),idx + off_set) 

## extract mead and std measures
extract_data = data[,col_to_extract]

#3.Uses descriptive activity names to name the activities in the data set

##load activity labels
labels = read.table("UCI\ HAR\ Dataset/activity_labels.txt",col.names = c("idx","name"),stringsAsFactors = F)

## substitute index to activity names

gsub2 <- function(pattern, replacement, x, ...) {
  for(i in 1:length(pattern))
    x <- gsub(pattern[i], replacement[i], x, ...)
  x
}

from = labels$idx
to = labels$name

extract_data[,2] = gsub2(from,to,extract_data[,2])

#4. Appropriately labels the data set with descriptive variable names. 
new_label = c(rep("",length(idx)))

## for label with XXX-mean/std()-X/Y/Z
for (i in append(c(1 : grep(166,idx)), c(grep(266,idx)):c(grep(429,idx)))) {
  feature_idx = idx[i]
  feature_name = features$measurements[feature_idx]
  feature_name_split = strsplit(feature_name,'-')
  is_mean = grepl("mean",feature_name_split)
  if (is_mean) {
    statistics = "mean"
  } else {
   statistics = "std"
  }
  
  new_label[i] = paste( feature_name_split[[1]][1], "_",statistics,"_",
                       feature_name_split[[1]][3],sep = "")
}
## for label with XXX-mean/std()
for (i in append(c(grep(201,idx) : grep(254,idx)),c(grep(503,idx):grep(543,idx))) ) {
  feature_idx = idx[i]
  feature_name = features$measurements[feature_idx]
  feature_name_split = strsplit(feature_name,'-')
  is_mean = grepl("mean",feature_name_split)
  if (is_mean) {
    statistics = "mean"
  } else {
    statistics = "std"
  }
  
  new_label[i] = paste(feature_name_split[[1]][1],"_",statistics, sep = "")
}

## assign new_label to column names of data frame
names(extract_data) = append(c("subject","activity"),new_label)
measure_names = names(extract_data)[3:length(extract_data)]

#5.From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.

tidy_data=aggregate(extract_data[,3:ncol(extract_data)],extract_data[c("subject","activity")], FUN = mean)

tidy_label = c(rep("",length(measure_names)))

for (i in 1:length(measure_names)-2) {
  tidy_label[i] = paste("Avg",measure_names[i],sep="")
}

tidy_label = append(c("subject","activity"),tidy_label)
names(tidy_data) = tidy_label

# write tidy data to tidy.csv
write.table(tidy_data,file = "tidy.txt",quote=FALSE, col.names = TRUE,row.names = FALSE,sep="\t")






