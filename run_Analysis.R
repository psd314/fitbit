run_analysis <- function (){
    #read the names of features into R
    features <- read.table("./features.txt", stringsAsFactors = FALSE, col.names = c("number", "name")) 
    
    #combine raw test files into dataframe 
    raw.test.subj <- read.table("./test/subject_test.txt", header = FALSE, stringsAsFactors = FALSE,
                                col.names = "subject")
    raw.test.x <- read.table("./test/X_test.txt", header = FALSE, stringsAsFactors = FALSE,
                             col.names = features$name)
    raw.test.y <- read.table("./test/y_test.txt", header = FALSE, stringsAsFactors = FALSE,
                             col.names = "activity")
    test.combined <- cbind(raw.test.subj, raw.test.x, raw.test.y)
   
    
    # combine raw train files into dataframe
    raw.train.subj <- read.table("./train/subject_train.txt", header = FALSE, stringsAsFactors = FALSE,
                                 col.names = "subject")
    raw.train.x <- read.table("./train/X_train.txt", header = FALSE, stringsAsFactors = FALSE,
                              col.names = features$name)
    raw.train.y <- read.table("./train/y_train.txt", header = FALSE, stringsAsFactors = FALSE,
                              col.names = "activity")
    train.combined <- cbind(raw.train.subj, raw.train.x, raw.train.y)
    
    #combine test and training data into one dataframe
    raw.combined <- rbind(train.combined, test.combined)
    
    #convert labels from numbers to descriptive names
    act.labels <- c("walking", "walkingupstairs", "walkingdownstairs","sitting", "standing","laying")
    for (i in 1:length(act.labels)) {
        label <- act.labels[i]
        raw.combined$activity <- gsub(i,label, raw.combined$activity)
    }
    
    #subset combined data and select only measurements that take the mean or std (standard deviation)
    mean.cols <- grep("mean", names(raw.combined))
    std.cols <- grep("std", names(raw.combined))

    names(raw.combined) <- tolower(names(raw.combined))             #tidy names by changing to lower case
    names(raw.combined) <- gsub("[.]","",names(raw.combined))       #tidy names by removing extraneous punctuation

    
    data <- raw.combined %>% select(c(1,563, mean.cols, std.cols)) %>%
        arrange(subject,activity) %>% group_by(subject, activity) %>% summarize_each(funs(mean(.))) %>% print
    
    View(data)
    
    
    
    
}