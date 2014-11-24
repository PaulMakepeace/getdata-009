##

## Obtain & unzip data:
## download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile="dataset.zip", method='curl')
## unzip('dataset.zip')
## setwd('UCI HAR Dataset/')

# Read in list of features (column 2) as factors
features <- read.table('features.txt', colClasses=c('numeric', 'factor'))[,2]
# Pick out the features list that have 'mean' or 'std' in their name
mean_std_features <- grep('mean|std', features)

# Activities: "walking", "walking upstairs", etc
activities <- read.table('activity_labels.txt',
                         col.names=c('activity_id', 'activity'))
activities$activity <- factor(activities$activity, labels =
                              tolower(sub('_', ' ', activities$activity)))

# Data is structured as rows of 561 variables corresponding to `features`
load_data <- function(dataset_type) {
    # file_name: make a file name like 'train/X_train.txt'
    file_name <- function(file_type) {
        paste(dataset_type, '/', file_type, '_', dataset_type, '.txt', sep='')
    }
    data_file <- file_name('X')
    activity_file <- file_name('y')
    subject_file <- file_name('subject')
    # Subset & name the data based on the `mean_std_features` we're interested in.
    data <- read.table(data_file)[,mean_std_features]
    names(data) <- features[mean_std_features]
    # Join the activities data on activity_id to produce a list of factors
    activities <- merge(read.table(activity_file, col.names='activity_id'),
                        activities)$activity
    subjects <- read.table(subject_file, col.names='subjectid',
                           colClasses='factor')
    cbind(subjects, activities, data)
}

all_data <- rbind(
    load_data('train'),
    load_data('test')
)

# Find the mean of columns avoiding the subjectid & activities columns
activity_mean <- function(data) {
    colMeans(data[!colnames(data) %in% c('subjectid', 'activities')])
}
# setwd('..')
averages_by_subject_activity <-
    sapply(split(all_data, sinteraction(all_data$activities, all_data$subjectid)),
           activity_mean)
