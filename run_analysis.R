run_analysis <- function(){
     
     rm(list=ls())
     
     #0) Set project directory
     #The name of directory for downloading data.  You may change
     projDir <- './run_analy'
     
     #check if the directory exists.  If not, create it
     if(!dir.exists(projDir)) {dir.create(projDir)}
     
     
     #1) Download the zip file
     temp <- paste0(projDir, '/a.zip')
     fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
     download.file(fileUrl, temp)
     
     
     #2) unzip the file
     unzip(temp, exdir = projDir)
     
 
     #3) read the unzipped text files X_train.txt, subject_train.txt, y_train.txt, 
     #X_test.txt, subject_test.txt, y_test.txt, features.txt and merge them in 
     #one data set
     
     #create 3 data directories 
     testDir <- paste0(projDir, '/UCI HAR Dataset/test/')
     trainDir <- paste0(projDir, '/UCI HAR Dataset/train/')
     miscDir <- paste0(projDir, '/UCI HAR Dataset/')
     
     #read X_test txt files, combine subject column and y_test with X_test
     X_file <- paste0(testDir, 'X_test.txt')    
     subject <-paste0(testDir,'subject_test.txt')
     X_test <- cbind(read.table(subject), read.table(X_file))
     y_file <- paste0(testDir,'y_test.txt')
     X_test <- cbind(read.table(y_file),X_test)
     
     #read X_train txt files, combine subject column and y_train with X_train
     X_file <- paste0(trainDir,'X_train.txt')
     subject <-paste0(trainDir,'subject_train.txt')
     X_train <- cbind(read.table(subject),read.table(X_file))
     y_file <- paste0(trainDir,'y_train.txt')
     X_train <- cbind(read.table(y_file),X_train)
     
     #Merge X_test with X_train
     X_comb <- rbind(X_test,X_train)
     
     #read features.txt
     featTitles<-read.table(paste0(miscDir,'features.txt'))
     
     #add unique feature variable names to the top row of merged file
     #(make.unique adds .n to duplicate column names. 
     #dplyr select function does not work if a column name is not unique)
     colnames(X_comb) <- 
     c('activity','subject', make.unique(as.vector(featTitles[,2])))
     
     
     #4) select column that the names contain 'activity' or 'subject', or 'mean()', or 'std()'
     require(dplyr)
     X_selct <- X_comb %>% 
     dplyr::select(grep('activity|subject|mean\\()|std\\()',names(X_comb)))
     
     
     #5) Change activity identifier to descriptive text
     #read activity labels file
     actLabel <-read.table(paste0(miscDir,'activity_labels.txt'))
     #replace activity number with lower case descriptive text in X_selct column 1
     for (i in 1:nrow(actLabel)) 
     X_selct[,1]<-gsub(actLabel[i,1],tolower(actLabel[i,2]),X_selct[,1])
     
     #Extract column names 
     nameVec <- names(X_selct)
     
     
     #6) Replace column names containing '-' with '_' and remove '()'
     nameVec <- gsub('\\-','_',nameVec)
     nameVec <- gsub('\\()','',nameVec) 
     #nameVec <- gsub('[[:punct:]]','',nameVec)  -  this should work also
     
     #Write the new column names back to X_selct
     names(X_selct) <- nameVec
     
     
     #7) View and save the tidy file
     View(X_selct)
     write.table(X_selct, file = "./data/X_tidy.txt",  row.name=FALSE)

     
     #8) Calculate means (using dplyr group by & summarise_all, much faster than ver 2)
     require(dplyr)
     #remember not to use 'activity' but activity in group_by function
     X_means <- X_selct %>% group_by(activity, subject) %>% summarise_all(funs(mean))
     
     
     #9) View and save the new means file
     View(X_means)
     write.table(X_means, file = "./data/X_means.txt", row.name=FALSE)
     
     
     #Calculate means - ver 2 (slower, using reshape recast)
     #install.packages('reshape2')
     #require(reshape2)
     #X_means <-ddply(X_selct, c('activity','subject'), summarise, mean)
          
}

