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
