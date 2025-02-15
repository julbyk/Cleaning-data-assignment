# Code bood for 'run_analysis.R' and output files


## Variables created by 'run_analysis.R'
* subject_test: data frame with subject identifier for test dataset, size 2947x1, integer 1..30
* y_test: data frame with activity identifier for test dataset, size 2947x1, integer 1 .. 6
* x_test: data frame of measurements for test dataset,  size 2947x561, numeric, -1 .. 1
* subject_train: data frame with subject identifier for train dataset, size 7352x1, integer 1..30
* y_train: data frame with activity identifier for train dataset, size 7352x1, integer 1 .. 6
* x_train: data frame of measurements for train dataset,  size 7352x561, numeric, -1 .. 1
* subject: data frame with subject identifier for combined dataset, size 10299x1, integer 1..30
* y_raw: data frame with activity identifier for combined dataset, size 10299x1, integer 1 .. 6
* x_raw: data frame of measurements for combined dataset with all features,  size 10299x561, numeric, -1 .. 1
* activity: list of activities for combined dataset
* x: data frame of measurements for combined dataset with selected features,  size 10299x66, numeric, -1 .. 1

* select: logical vector with feature selection
* file_names: vector that stores the file names for reading from the 'UCI HAR Dataset' directory and subdirectories
* file_path: variable that stores the file path for ready for 'UCI HAR Dataset' subdirectory
* activity_labes: data frame with activity identifier and corresponding activity, size 6x2
* features: the list of names of observations

* data: tidy dataset, size 10299x561
* sum_data: tidy summarized dataset

## 'run_analysis.R' output
* 'output-data.txt' with output dataset from step 4
* 'output-sum-data.txt" with output dataset from step 5


## Variables in the output files 

* ### subject
	Identifier for every subject who carried out experiment
	integer,
		1..30

* ### activity
	Type of the activity studied at every experiment.
	factor with 6 levels,
		WALKING
		WALKING_UPSTAIRS
		WALKING_DOWNSTAIRS
		SITTING
		STANDING
		LAYING


* ### tBodyAcc-mean()-X
	The mean of body acceleration along X
	numeric,
		-1 to 1

* ### tBodyAcc-mean()-Y
	The mean of body acceleration along Y
	numeric,
		-1 to 1

* ### tBodyAcc-mean()-Z
	The mean of body acceleration along Z
	numeric,
		-1 to 1

* ### tBodyAcc-std()-X
	The standard deviation of body acceleration along X
	numeric,
		-1 to 1

* ### tBodyAcc-std()-Y
	The standard deviation of body acceleration along Y
	numeric,
		-1 to 1

* ### tBodyAcc-std()-Z
	The standard deviation of body acceleration along Z
	numeric,
		-1 to 1

* ### tGravityAcc-mean()-X
	The mean of gravity acceleration along X
	numeric,
		-1 to 1

* ### tGravityAcc-mean()-Y
	The mean of gravity acceleration along Y
	numeric,
		-1 to 1

* ### tGravityAcc-mean()-Z
	The mean of gravity acceleration along Z
	numeric,
		-1 to 1

* ### tGravityAcc-std()-X
	The standard deviation of gravity acceleration along X
	numeric,
		-1 to 1

* ### tGravityAcc-std()-Y
	The standard deviation of gravity acceleration along Y
	numeric,
		-1 to 1

* ### tGravityAcc-std()-Z
	The standard deviation of gravity acceleration along Z
	numeric,
		-1 to 1

* ### tBodyAccJerk-mean()-X	
	The mean of body acceleration jerk along X
	numeric,
		-1 to 1

* ### tBodyAccJerk-mean()-Y
	The mean of body acceleration jerk along Y
	numeric,
		-1 to 1

* ### tBodyAccJerk-mean()-Z
	The mean of body acceleration jerk along Z
	numeric,
		-1 to 1

* ### tBodyAccJerk-std()-X
	The standard deviation of body acceleration jerk along X
	numeric,
		-1 to 1

* ### tBodyAccJerk-std()-Y
	The standard deviation of body acceleration jerk along Y
	numeric,
		-1 to 1

* ### tBodyAccJerk-std()-Z
	The standard deviation of body acceleration jerk along Z
	numeric,
		-1 to 1

* ### tBodyGyro-mean()-X
	The mean of body acceleration by gyroscope along X
	numeric,
		-1 to 1

* ### tBodyGyro-mean()-Y
	The mean of body acceleration by gyroscope along Y
	numeric,
		-1 to 1

* ### tBodyGyro-mean()-Z
	The mean of body acceleration by gyroscope along Z
	numeric,
		-1 to 1

* ### tBodyGyro-std()-X
	The standard deviation of body acceleration by gyroscope along X
	numeric,
		-1 to 1

* ### tBodyGyro-std()-Y
	The standard deviation of body acceleration by gyroscope along Y
	numeric,
		-1 to 1

* ### tBodyGyro-std()-Z
	The standard deviation of body acceleration by gyroscope along Z
	numeric,
		-1 to 1

* ### tBodyGyroJerk-mean()-X
	The mean of body jerk by gyroscope along X
	numeric,
		-1 to 1

* ### tBodyGyroJerk-mean()-Y
	The mean of body jerk by gyroscope along Y
	numeric,
		-1 to 1

* ### tBodyGyroJerk-mean()-Z
	The mean of body jerk by gyroscope along Z
	numeric,
		-1 to 1

* ### tBodyGyroJerk-std()-X
	The standard deviation of body jerk by gyroscope along X
	numeric,
		-1 to 1

* ### tBodyGyroJerk-std()-Y
	The standard deviation of body jerk by gyroscope along Y
	numeric,
		-1 to 1

* ### tBodyGyroJerk-std()-Z
	The standard deviation of body jerk by gyroscope along Z
	numeric,
		-1 to 1

* ### tBodyAccMag-mean()
	The mean of body acceleration
	numeric,
		-1 to 1

* ### tBodyAccMag-std()
	The standard deviation of body acceleration
	numeric,
		-1 to 1

* ### tGravityAccMag-mean()
	The mean of gravity acceleration
	numeric,
		-1 to 1

* ### tGravityAccMag-std()
	The standard deviation of gravity acceleration
	numeric,
		-1 to 1

* ### tBodyAccJerkMag-mean()
	The mean of body acceleration jerk
	numeric,
		-1 to 1

* ### tBodyAccJerkMag-std()
	The standard deviation of body acceleration jerk
	numeric,
		-1 to 1

* ### tBodyGyroMag-mean()
	The mean of body acceleration by gyroscope
	numeric,
		-1 to 1

* ### tBodyGyroMag-std()
	The standard deviation of body acceleration by gyroscope
	numeric,
		-1 to 1

* ### tBodyGyroJerkMag-mean()
	The mean of body jerk by gyroscope
	numeric,
		-1 to 1

* ### tBodyGyroJerkMag-std()
	The standard deviation of body jerk by gyroscope
	numeric,
		-1 to 1

* ### fBodyAcc-mean()-X
	The mean of FFT body acceleration along X
	numeric,
		-1 to 1

* ### fBodyAcc-mean()-Y
	The mean of FFT body acceleration along Y
	numeric,
		-1 to 1

* ### fBodyAcc-mean()-Z
	The mean of FFT body acceleration along Z
	numeric,
		-1 to 1

* ### fBodyAcc-std()-X
	The standard deviation of FFT body acceleration along X
	numeric,
		-1 to 1

* ### fBodyAcc-std()-Y
	The standard deviation of FFT body acceleration along Y
	numeric,
		-1 to 1

* ### fBodyAcc-std()-Z
	The standard deviation of FFT body acceleration along Z
	numeric,
		-1 to 1

* ### fBodyAccJerk-mean()-X
	The mean of FFT body acceleration jerk along X
	numeric,
		-1 to 1

* ### fBodyAccJerk-mean()-Y
	The mean of FFT body acceleration jerk along Y
	numeric,
		-1 to 1

* ### fBodyAccJerk-mean()-Z
	The mean of FFT body acceleration jerk along Z
	numeric,
		-1 to 1

* ### fBodyAccJerk-std()-X
	The standard deviation of FFT body acceleration jerk along X
	numeric,
		-1 to 1

* ### fBodyAccJerk-std()-Y
	The standard deviation of FFT body acceleration jerk along Y
	numeric,
		-1 to 1

* ### fBodyAccJerk-std()-Z
	The standard deviation of FFT body acceleration jerk along Z
	numeric,
		-1 to 1

* ### fBodyGyro-mean()-X
	The mean of FFT body acceleration by gyroscope along X
	numeric,
		-1 to 1

* ### fBodyGyro-mean()-Y
	The mean of FFT body acceleration by gyroscope along Y
	numeric,
		-1 to 1

* ### fBodyGyro-mean()-Z
	The mean of FFT body acceleration by gyroscope along Z
	numeric,
		-1 to 1

* ### fBodyGyro-std()-X
	The standard deviation of FFT body acceleration by gyroscope along X
	numeric,
		-1 to 1

* ### fBodyGyro-std()-Y
	The standard deviation of FFT body acceleration by gyroscope along Y
	numeric,
		-1 to 1

* ### fBodyGyro-std()-Z
	The standard deviation of FFT body acceleration by gyroscope along Z
	numeric,
		-1 to 1

* ### fBodyAccMag-mean()
	The mean of FFT body acceleration
	numeric,
		-1 to 1

* ### fBodyAccMag-std()
	The standard deviation of FFT body acceleration
	numeric,
		-1 to 1

* ### fBodyBodyAccJerkMag-mean()
	The mean of FFT body acceleration jerk
	numeric,
		-1 to 1

* ### fBodyBodyAccJerkMag-std()
	The standard deviation of FFT body acceleration jerk
	numeric,
		-1 to 1

* ### fBodyBodyGyroMag-mean()
	The mean of FFT acceleration by gyroscope
	numeric,
		-1 to 1

* ### fBodyBodyGyroMag-std()
	The standard deviation of FFT acceleration by gyroscope
	numeric,
		-1 to 1

* ### fBodyBodyGyroJerkMag-mean()
	The mean of FFT body acceleration jerk by gyroscope
	numeric,
		-1 to 1

* ### fBodyBodyGyroJerkMag-std()
	The standard deviation of FFT body acceleration jerk by gyroscope
	numeric,
		-1 to 1
