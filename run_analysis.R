#Packages I need
library(plyr)
library(reshape2)

#Due to some network problem I manually downloaded the zip file and unzipped it.
#Set the working directory(UCI HAR Dataset) and read the files

xtestdata    <- read.table("./test/x_test.txt")
ytestdata    <- read.table("./test/y_test.txt")
testsubjects <- read.table("./test/subject_test.txt")
xtraindata    <- read.table("./train/x_train.txt")
ytraindata    <- read.table("./train/y_train.txt")
trainsubjects <- read.table("./train/subject_train.txt")

#merge
allActivityData <- rbind(xtestdata,xtraindata)
allLabelSets    <- rbind(ytestdata,ytraindata)
allSubjectCodes <- rbind(testsubjects,trainsubjects)

# 2
featurenames   <- read.table("./features.txt")
meanandstddevfeatures  <- grepl("(-std\\(\\)|-mean\\(\\))",featurenames$V2)
filteredActivityData <- allActivityData[, which(meanandstddevfeatures == TRUE)]

# 3
activityLabels  <- read.table("./activity_labels.txt")
activity <- as.factor(allLabelSets$V1)
levels(activity) <- activityLabels$V2

subject <- as.factor(allSubjectCodes$V1)

filteredActivityData <- cbind(subject,activity,filteredActivityData)


# 4
filteredfeatures <- (cbind(featurenames,meanandstddevfeatures)[meanandstddevfeatures==TRUE,])$V2

cleaner <- function(featurename) {
  tolower(gsub("(\\(|\\)|\\-)","",featurename))
}
filteredfeatures <- sapply(filteredfeatures,cleaner)

names(filteredActivityData)[3:ncol(filteredActivityData)] <- filteredfeatures

write.csv(filteredActivityData,file="Tidy.csv")
write.table(filteredActivityData, "Tidy.txt", sep="\t")


# 5
molten <- melt(filteredActivityData,id.vars=c("subject","activity"))
tidy <- dcast(molten,subject + activity ~ variable,mean)
write.table(tidy, "Tidy.mean.txt", sep="\t")
