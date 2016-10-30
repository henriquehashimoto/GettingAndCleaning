1) Merges the training and the test sets to create one data set.
------------------------------------

Concatenate the data tables.


```r
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)
```


Merge columns.


```r
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
```


Set key.


```r
setkey(dt, subject, activityNum)
```



2)Extracts only the measurements on the mean and standard deviation for each measurement.
--------------------------------------------

Read the `features.txt` file. This tells which variables in `dt` are measurements for the mean and standard deviation.


```r
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
```


Subset only measurements for the mean and standard deviation.


```r
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
```


Convert the column numbers to a vector of variable names matching columns in `dt`.


```r
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
```

```
##    featureNum       featureName featureCode
## 1:          1 tBodyAcc-mean()-X          V1
## 2:          2 tBodyAcc-mean()-Y          V2
## 3:          3 tBodyAcc-mean()-Z          V3
## 4:          4  tBodyAcc-std()-X          V4
## 5:          5  tBodyAcc-std()-Y          V5
## 6:          6  tBodyAcc-std()-Z          V6
```

```r
dtFeatures$featureCode
```

```
##  [1] "V1"   "V2"   "V3"   "V4"   "V5"   "V6"   "V41"  "V42"  "V43"  "V44" 
## [11] "V45"  "V46"  "V81"  "V82"  "V83"  "V84"  "V85"  "V86"  "V121" "V122"
## [21] "V123" "V124" "V125" "V126" "V161" "V162" "V163" "V164" "V165" "V166"
## [31] "V201" "V202" "V214" "V215" "V227" "V228" "V240" "V241" "V253" "V254"
## [41] "V266" "V267" "V268" "V269" "V270" "V271" "V345" "V346" "V347" "V348"
## [51] "V349" "V350" "V424" "V425" "V426" "V427" "V428" "V429" "V503" "V504"
## [61] "V516" "V517" "V529" "V530" "V542" "V543"
```


Subset these variables using variable names.


```r
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]
```



3)Uses descriptive activity names to name the activities in the data set
------------------------------

Read `activity_labels.txt` file. This will be used to add descriptive names to the activities.


```r
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
```


4)Appropriately labels the data set with descriptive variable names.
-----------------------------------------------------------------

Merge activity labels.


```r
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
```


Add `activityName` as a key.


```r
setkey(dt, subject, activityNum, activityName)
```


Melt the data table to reshape it from a short and wide format to a tall and narrow format.


```r
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
```


Merge activity name.


```r
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
    all.x = TRUE)
```


Create a new variable, `activity` that is equivalent to `activityName` as a factor class.
Create a new variable, `feature` that is equivalent to `featureName` as a factor class.


```r
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
```


Seperate features from `featureName` using the helper function `grepthis`.


```r
grepthis <- function(regex) {
    grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))
```


Check to make sure all possible combinations of `feature` are accounted for by all possible combinations of the factor class variables.


```r
r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
    "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2
```

```
## [1] TRUE
```



5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
----------------------

Create a data set with the average of each variable for each activity and each subject.


```r
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
    featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
```


Make codebook.


```r
knit("makeCodebook.Rmd", output = "codebook.md", encoding = "ISO8859-1", quiet = TRUE)
```

```
## [1] "codebook.md"
```

```r
markdownToHTML("codebook.md", "codebook.html")
```

