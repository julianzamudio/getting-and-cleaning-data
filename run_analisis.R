## Extracting the data
## description of the file need it
# train/X_train.txt': Training set.
#'train/y_train.txt': Training labels.
# 'test/X_test.txt': Test set.
# 'test/y_test.txt': Test labels.
# all the files were unzipped before start the process

library(plyr)
library(dplyr)

# reading all the data

setwd("GCdata/UCI HAR Dataset")
test <- read.table("./test/X_test.txt")
training<- read.table("./train/X_train.txt")
activity <- read.table("activity_labels.txt")
label.t<- read.table("./test/y_test.txt")
label.tr<- read.table("./train/y_train.txt")
subject_test <- read.table("./test/subject_test.txt")
subject_training <- read.table("./train/subject_train.txt")

# defining Labels and columns names

num <- activity$V1
description <- activity$V2
variables <- read.table("features.txt")
names(test) <- variables[,2]
names(training) <- variables[,2]
names(label.t) <- c("label")
names(label.tr) <- c("label")
names(subject_test) <- c("subject")
names(subject_training) <- c("subject")
test <- cbind(label.t,subject_test,test)
training <- cbind(label.tr,subject_training,training)

# subsetting and merging the data 

test$experiment = "test"
training$experiment = "training"
test <- select(test,c("experiment","label","subject",variables$V2[grep("[Mm]ean|[Ss]td",variables$V2)]))
training <- select(training,c("experiment","label","subject",variables$V2[grep("[Mm]ean|[Ss]td",variables$V2)]))
fdataset = merge(test,training, all= TRUE)

# including de activity labels


for (i in num) {
  
  desc = description[i]
  fdataset$label <-sub(i, desc,fdataset$label)
  
}

# Giving proper labels to the data set with descriptive variable names.

newnames <- sub("[T|t]Body[A]", "Body_acceleration", names(fdataset))
newnames <- sub("[T|t]Gravity[A]", "Gravity_acceleration", newnames)
newnames <- sub("[T|t]BodyGyro", "Body_gyroscope_velocity", newnames)
newnames <- sub("[F|f]Body[A]", "Fourier_Body_acceleration", newnames)
newnames <- sub("[F|f]Gravity[A]", "Foruier_Gravity_acceleration", newnames)
newnames <- sub("[F|f]BodyGyro", "Fourier_Body_gyroscope_velocity", newnames)
newnames <- sub("[F|f]BodyBodyGyro", "Fourier_Body_gyroscope_smaple_window", newnames)
newnames <- sub("[F|f]BodyBodyAcc", "Fourier_Body_aceleration_smaple_window", newnames)

names(fdataset) <- newnames

# creating the tidy data set with averages for all the variables

tdata <- aggregate(fdataset[,4:89], list(fdataset$label,fdataset$subject), mean)
colnames(tdata) <- newnames[-1]
write.csv(tdata, file = "tidydataset.csv")