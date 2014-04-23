The HAR Project Codebook
========================

## 0. Introduction

The Human Activity Recognition database is built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, the researchers captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

A data analysis is run, in order to achieve the following results:
- Merge the training and the test sets to create one overall data set.
- Extracts only the measurements on the mean and standard deviation for each measurement. 
- Appropriately label the data set with descriptive activity names. 
- Create a new, tidy, aggregate data set with the average of each variable for each activity and each subject.

## 1. Raw Data

The dataset includes the following files:

### Main folder

- 'README.txt'

- 'features_info.txt': Shows descriptive information about the variables used on the feature vector.

- 'features.txt': It contains the name of the 561 measured features.

- 'activity_labels.txt': Links the 6 class labels with their activity name.

#### "train" folder

- 'X_train.txt': The Training set: 7352 rows x 561 columns. Each row contains the measurements for the corresponding  feature, as listed in features.txt

- 'y_train.txt': The Training labels: 7352 rows x 1 column. Each row contains the numerical code for the corresponding  activity, as listed in activity_labels.txt

- 'subject_train.txt': Each of the 7352 row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

#### "test" folder

- 'X_test.txt': The Test set: 7352 rows x 561 columns. Each row contains the measurements for the corresponding  feature, as listed in features.txt

- 'y_test.txt': The Test labels: 7352 rows x 1 column. Each row contains the numerical code for the corresponding  activity, as listed in activity_labels.txt

- 'subject_test.txt': Each of the 7352 row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

### Unused data

Both "train" and "test" contain a subfolder, called "Inertial Signals", which contains the original unpreprocessed data. Such data were not used in my analysis.

## 2. The R script

The script provided in this repo performs the cleaning of the raw data, in order to obtain a tidy dataset, as stated in the paragraph 0.

It consists of 7 tasks (6 as two collapse), which are performed sequentially:

- Getting and unzipping the archives.
- Merging the raw datasets; includes adding variable names and adding the two variables ACTIVITY and SUBJECT from the corresponding files. "Merging" is implemented as "appending", not as "JOINing the SQL way", such as with the `merge()` function, as the two datasets have the same schema and refer to two independent sets of measurement of the same quantities.
- Extracting only the desired variables; the choice is implemented by looking at the variable name and by considering only variables related to "mean()" or "std()".
- Adding descriptive names to the listed activities.
- Creating the output tidy data set, carrying the average of each variable for each activity and each subject.
- Saving the results to a file.

```
## 0. Check if the remote file has already
##    been cached to the local directory.
##    If not, then download it. If the data,
##    set isn't extracted yet, then unzip it.

	require(downloader)
	dataDir <- paste(getwd(),"\\data",sep="")
	if (!file.exists(dataDir)) {
		message("Creating data folder...")
	    	dir.create(dataDir)
	}
	fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	destFile <- paste(dataDir,"\\har.zip",sep="")
	if (!file.exists(destFile)) {
		message("Downloading raw data...")
    		download(fileUrl, destFile, mode = "wb")
		message(paste("Data downloaded on ",date(),sep=""))
	}
	harDir   <- paste(dataDir,"\\UCI HAR Dataset",sep="")
	if (!file.exists(paste(harDir,"\\features.txt",sep=""))) {
		message("Extracting compressed data...")
		unzip(zipfile=destFile,exdir = dataDir)
	}

## 1. Merge the two downloaded datasets into
##    one overall dataset.

	features <- read.table(paste(harDir,"\\features.txt",sep=""))
	actLabels <- read.table(paste(harDir,"\\activity_labels.txt",sep=""))
	colTitles <- c(as.vector(features[,2]),c("ACTIVITY","SUBJECT"))	

	message("Reading training set...")
	trainDir <- paste(harDir,"\\train",sep="")
	trainSubj <- read.table(paste(trainDir,"\\subject_train.txt",sep=""))
	trainX <- read.table(paste(trainDir,"\\X_train.txt",sep=""))
	trainY <- read.table(paste(trainDir,"\\Y_train.txt",sep=""))
	trainX <- cbind(trainX,trainY)
	trainX <- cbind(trainX,trainSubj)
	colnames(trainX) <- colTitles

	message("Reading test set...")
	testDir  <- paste(harDir,"\\test",sep="")
	testSubj <- read.table(paste(testDir,"\\subject_test.txt",sep=""))
	testX <- read.table(paste(testDir,"\\X_test.txt",sep=""))
	testY <- read.table(paste(testDir,"\\Y_test.txt",sep=""))
	testX <- cbind(testX,testY)
	testX <- cbind(testX,testSubj)
	colnames(testX) <- colTitles
	
	message("Merging sets...")
	mergedData <- rbind(trainX,testX)

## 2. Extract only the measurements on the mean and standard deviation
##    for each measurement.

	message("Generating first tidy data set...")
	colsOk <- colTitles[
                           regexpr("-mean()", colTitles, fixed=TRUE) > 0 |
                           regexpr("-std()", colTitles, fixed=TRUE) > 0  |
                           colTitles %in% c("ACTIVITY","SUBJECT")
                ]
	xData <- mergedData[colsOk]
	
## 3. (4.) Add descriptive activity names to the first tidy dataset.

	message("Decoding activities...")
	activNames <- sapply(xData[,"ACTIVITY"],function (i){actLabels[i,2]})
	tidyData1 <- cbind(xData,activNames)
	colnames(tidyData1)[ncol(tidyData1)] <- "ACTIVITY_NAME"

## 5. Create a second, independent tidy data set with the average of each
##    variable for each activity and each subject.

	message("Generating second tidy data set...")
	tidyData2 <- aggregate(
				tidyData1[,seq(1,ncol(tidyData1)-3)],
				list(tidyData1$ACTIVITY_NAME, tidyData1$SUBJECT),
				FUN=mean
			 )
	colnames(tidyData2)[1] <- "ACTIVITY"
	colnames(tidyData2)[2] <- "SUBJECT"
	colnames(tidyData2)[-(1:2)] <- paste(
							"MEAN_OF_",
							colnames(tidyData2)[-(1:2)],
							sep=""
						 )
	tidyData2 <- tidyData2[order(tidyData2$ACTIVITY,tidyData2$SUBJECT),]
	rownames(tidyData2) <- seq(1:nrow(tidyData2))

## 6. Write results to file

	message("Saving results to file...")
	resDir <- paste(getwd(),"\\results",sep="")
	if (!file.exists(resDir)) {
		message("Creating results folder...")
	    	dir.create(resDir)
	}
	resFile2 <- paste(resDir,"\\harTidyData.txt",sep="")
	write.table(tidyData2,resFile2)

	message("Data successfully processed.")

```

## 3. Results

The computed dataset consists of 180 rows x 68 columns. We have exactly 180 rows, as the instance corresponds to the cartesian product of activities (6) and subjects (30).

The column names are:

- ACTIVITY
- SUBJECT
- MEAN_OF_tBodyAcc-mean()-X
- MEAN_OF_tBodyAcc-mean()-Y
- MEAN_OF_tBodyAcc-mean()-Z
- MEAN_OF_tBodyAcc-std()-X
- MEAN_OF_tBodyAcc-std()-Y
- MEAN_OF_tBodyAcc-std()-Z
- MEAN_OF_tGravityAcc-mean()-X
- MEAN_OF_tGravityAcc-mean()-Y
- MEAN_OF_tGravityAcc-mean()-Z
- MEAN_OF_tGravityAcc-std()-X
- MEAN_OF_tGravityAcc-std()-Y
- MEAN_OF_tGravityAcc-std()-Z
- MEAN_OF_tBodyAccJerk-mean()-X
- MEAN_OF_tBodyAccJerk-mean()-Y
- MEAN_OF_tBodyAccJerk-mean()-Z
- MEAN_OF_tBodyAccJerk-std()-X
- MEAN_OF_tBodyAccJerk-std()-Y
- MEAN_OF_tBodyAccJerk-std()-Z
- MEAN_OF_tBodyGyro-mean()-X
- MEAN_OF_tBodyGyro-mean()-Y
- MEAN_OF_tBodyGyro-mean()-Z
- MEAN_OF_tBodyGyro-std()-X
- MEAN_OF_tBodyGyro-std()-Y
- MEAN_OF_tBodyGyro-std()-Z
- MEAN_OF_tBodyGyroJerk-mean()-X
- MEAN_OF_tBodyGyroJerk-mean()-Y
- MEAN_OF_tBodyGyroJerk-mean()-Z
- MEAN_OF_tBodyGyroJerk-std()-X
- MEAN_OF_tBodyGyroJerk-std()-Y
- MEAN_OF_tBodyGyroJerk-std()-Z
- MEAN_OF_tBodyAccMag-mean()
- MEAN_OF_tBodyAccMag-std()
- MEAN_OF_tGravityAccMag-mean()
- MEAN_OF_tGravityAccMag-std()
- MEAN_OF_tBodyAccJerkMag-mean()
- MEAN_OF_tBodyAccJerkMag-std()
- MEAN_OF_tBodyGyroMag-mean()
- MEAN_OF_tBodyGyroMag-std()
- MEAN_OF_tBodyGyroJerkMag-mean()
- MEAN_OF_tBodyGyroJerkMag-std()
- MEAN_OF_fBodyAcc-mean()-X
- MEAN_OF_fBodyAcc-mean()-Y
- MEAN_OF_fBodyAcc-mean()-Z
- MEAN_OF_fBodyAcc-std()-X
- MEAN_OF_fBodyAcc-std()-Y
- MEAN_OF_fBodyAcc-std()-Z
- MEAN_OF_fBodyAccJerk-mean()-X
- MEAN_OF_fBodyAccJerk-mean()-Y
- MEAN_OF_fBodyAccJerk-mean()-Z
- MEAN_OF_fBodyAccJerk-std()-X
- MEAN_OF_fBodyAccJerk-std()-Y
- MEAN_OF_fBodyAccJerk-std()-Z
- MEAN_OF_fBodyGyro-mean()-X
- MEAN_OF_fBodyGyro-mean()-Y
- MEAN_OF_fBodyGyro-mean()-Z
- MEAN_OF_fBodyGyro-std()-X
- MEAN_OF_fBodyGyro-std()-Y
- MEAN_OF_fBodyGyro-std()-Z
- MEAN_OF_fBodyAccMag-mean()
- MEAN_OF_fBodyAccMag-std()
- MEAN_OF_fBodyBodyAccJerkMag-mean()
- MEAN_OF_fBodyBodyAccJerkMag-std()
- MEAN_OF_fBodyBodyGyroMag-mean()
- MEAN_OF_fBodyBodyGyroMag-std()
- MEAN_OF_fBodyBodyGyroJerkMag-mean()
- MEAN_OF_fBodyBodyGyroJerkMag-std()

### Meaning of the variables

- ACTIVITY is the name of the activity performed by the subject under test.
- SUBJECT is the identifier of the subject

All other variable start with "MEAN_OF_": they actually represent the mean value of the corresponding quantity, aggregated from the original data.
To understand what each variable means, the complete decoding guidelines are given here:

- "t" at the start, just after "MEAN_OF_" represents a time domain variable, while "f" represents a frequency domain variable, obtained with the FFT algorithm.
- "-mean()" or "-std()" tell us whether the original variable was a mean or a standard deviation measurement.
- "Body" represents a body (high frequency) component, while "Gravity" is used for low frequency components.
- "Acc" data come from accelerometer measurement, while "Gyro" come from a gyroscope.
- The body linear acceleration and angular velocity were derived in time to obtain "Jerk" signals. 
- "Mag" tells us that the measurement refers to the magnitude of the measured quantity, calculated using the Euclidean norm.
- "-XYZ" is used to denote 3-axial signals in the X, Y and Z directions.
