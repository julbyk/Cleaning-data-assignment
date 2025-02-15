# Cleaning-data-assignment

## Getting and Cleaning Data peer-graded assignment by Julia Bykova
### Description

The "Cleaning-data-assignment" project creates the tidy dataset and summarized dataset of the selected data from the Human Activity Recognition Using Smartphones Dataset.

The script file 'run_analysis.R' will use the data from the 'UCI HAR Dataset' folder in order to output 2 files with tidy datasets: 'output-data.txt' and 'output-sum-data.txt'. In order for the script to work correctly, the working directory has to contain the folder 'UCI HAR Dataset'. Therefore it is recommended to set the working directory to source file location (in RStudio open 'run_analysis.R' sript and set: 'Session' -> 'Set Working Directory' -> 'To Source File Location').

The 'run_analysis.R' files does the following:
* Imports the data from dataset that is contained in the folder.
* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement.
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names.
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
* Outputs files with dataset from step 4 and 5.

### The "Cleaning-data-assignment" repository contains following files:

* 'UCI HAR Dataset' folder:
	dataset data on Human Activity Recognition Using Smartphones Dataset Version 1.0.
	Description of the files is included in the folder, in UCI HAR Dataset/README.txt file.
	Online data archive with additional information is http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

* 'run_analysis.R':
	R script that manipulates the data to perform cleaning and tidying.

* 'Output-data-codebook.md':
	text file with description of the parameters of the output datasets.
	
* 'output-data.txt':
	output dataset of tidy data with 10299 observations and 68 variables.
	File is generated by the R script 'run_analysis.R'.
	Training and test datasets are merged, only mean and standard deviation of each measurement is provided, activity names are described by names, all variable names are labeled.

* 'output-sum-data.txt':
	summarized output dataset of tidy data with 180 observations and 68 variables.
	File is generated by the R script 'run_analysis.R'
	Dataset 'output-data.txt' is additionally summarized, with average of parameters for each activity and each subject.
	
