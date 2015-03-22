
## The data from this report comes from 
### http://groupware.les.inf.pucrio.br/har 
## The training data is available from
### https://d396qusza40orc.cloudfront.net/predmachlearn/pmltraining.csv
## The test data is available from
### https://d396qusza40orc.cloudfront.net/predmachlearn/pmltesting.csv

### To achieve a consistent result, 1234 is used as the pseudorandom seed. The caret and randomForest packages are needed to build the model.

### set env, read in data set and set pseudorandom seed to 1234

```r
setwd("C:/Users/momlsy/Documents/ass")
trainset <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
testset <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))
set.seed(1234)
```

## Coherence test

```r
all.equal(colnames(testset)[1:length(colnames(testset))-1], colnames(trainset)[1:length(colnames(trainset))-1])
```

```
## [1] TRUE
```

### Cleaning dataset 

```r
mytrainset <- trainset[,colSums(is.na(trainset)) == 0]
mytestset <- testset[,colSums(is.na(testset)) == 0]
```

### Filter irrelevant variables (columns) in data set

```r
trainset <- mytrainset[,-c(1:7)]
testset <- mytestset[,-c(1:7)]
```

## Partition training data set for cross validation
### Trainset data set is partitioned into 2 sets - training 60% & test 40%

```r
library(caret)
subsamples <- createDataPartition(y=trainset$classe, p=0.60, list=FALSE)
mysubtrain <- trainset[subsamples,]
mysubtest <- trainset[-subsamples,]
dim(mysubtrain)
```

```
## [1] 11776    53
```

```r
dim(mysubtest)
```

```
## [1] 7846   53
```

### Plotting bar chart using various classe levels from partitioned training data set

```r
plot(mysubtrain$classe, col="cyan", main="Bar Chart for the classe levels from partitioned Training data set", xlab="classe levels", ylab="Frequency")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

## Observation
### Base on the chart, level A has the most frequent occurrences while level D with the least occurrences.

### Model 1 - using Decision Tree

```r
library(rpart)
```

```r
model1 <- rpart(classe ~ ., data=mysubtrain, method="class")
predict1 <- predict(model1, mysubtest, type = "class")
```


```r
library(rpart.plot)
```

```r
rpart.plot(model1, main="Classification Tree", extra=102, under=TRUE, faclen=0)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
confusionMatrix(predict1, mysubtest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1980  212   21   72   31
##          B   85  862   72   90   98
##          C   56  153 1086  209  175
##          D   71  101  110  823   89
##          E   40  190   79   92 1049
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7392          
##                  95% CI : (0.7294, 0.7489)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6699          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8871   0.5679   0.7939   0.6400   0.7275
## Specificity            0.9401   0.9455   0.9085   0.9434   0.9374
## Pos Pred Value         0.8549   0.7142   0.6468   0.6893   0.7234
## Neg Pred Value         0.9544   0.9012   0.9543   0.9304   0.9386
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2524   0.1099   0.1384   0.1049   0.1337
## Detection Prevalence   0.2952   0.1538   0.2140   0.1522   0.1848
## Balanced Accuracy      0.9136   0.7567   0.8512   0.7917   0.8324
```

## Observation from model 1
### Report shows that the accuracy of model 1 using decision tree is 0.739

## Model 2 - using randomForest function

```r
library(randomForest)
```

```r
model2 <- randomForest(classe ~. , data=mysubtrain)
predict2 <- predict(model2, mysubtest, type = "class")
```
## Testing result on testing data set

```r
confusionMatrix(predict2, mysubtest$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2232   10    0    0    0
##          B    0 1503   12    0    0
##          C    0    5 1354   20    2
##          D    0    0    2 1264    2
##          E    0    0    0    2 1438
## 
## Overall Statistics
##                                           
##                Accuracy : 0.993           
##                  95% CI : (0.9909, 0.9947)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9911          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9901   0.9898   0.9829   0.9972
## Specificity            0.9982   0.9981   0.9958   0.9994   0.9997
## Pos Pred Value         0.9955   0.9921   0.9804   0.9968   0.9986
## Neg Pred Value         1.0000   0.9976   0.9978   0.9967   0.9994
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2845   0.1916   0.1726   0.1611   0.1833
## Detection Prevalence   0.2858   0.1931   0.1760   0.1616   0.1835
## Balanced Accuracy      0.9991   0.9941   0.9928   0.9911   0.9985
```
## Conclusion from model 2
### Statistic shows the accuracy of model 2 using randomForest is 0.993. The out-of-sample error is only 1-0.993=0.007. This shows that randomForest algorithm perform better accuracy than Decision Tree

## Applying the algorithm to 20 test cases

```r
predict <- predict(mod2, testset, type="class")
head(predict,20)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

## Write files for submission

```r
pml_write_files = function(x){
n = length(x)
for(i in 1:n){
filename = paste0("problem_id_",i,".txt")
write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}
pml_write_files(predict)
```


