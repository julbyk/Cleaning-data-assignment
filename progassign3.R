library(tidyr)
library(dplyr)

# load test and train datasets
for (file_path in c(file.path("./UCI HAR Dataset/test"),file.path("./UCI HAR Dataset/train"))){
    file_names = list.files(path = file_path, pattern="*.txt");
    list2env(
        lapply(setNames(file.path(file_path,file_names),make.names(gsub("*.txt$","",file_names))),
               read.table), envir = .GlobalEnv
    )
}

# combine training and test datasets
x_raw <- rbind(X_train,X_test);
y_raw <- rbind(y_train,y_test);
subject <- rbind(subject_train,subject_test);
names(subject) <- "subject"


# load activity label and feature tables
activity_labels <- read.table(file.path("./UCI HAR Dataset","activity_labels.txt"));
features <- read.table(file.path("./UCI HAR Dataset","features.txt"));

# features containing mean or standard deviation
select <- grepl("[Mm]ean\\(|[Ss]td\\(",features[,2]);
x <- x_raw[,select];

#assign variable name for data columns
names(x) <- features[select,2];

# assign activity name to every observation
actfunc <- function(x) {activity_labels[match(x,activity_labels[,1]),2]}
activity <- sapply(y_raw[,1],actfunc)

# combine all data into the final dataset
data <- cbind(subject,activity,x)
names(data[,1:2]) <- c("subject","activity")

#data summarized
sum_data <-
data %>%
    gather(feature,value,-c(subject,activity)) %>%
    group_by(subject,activity,feature) %>%
    summarize(mean=mean(value)) %>%
    spread(feature,mean)
