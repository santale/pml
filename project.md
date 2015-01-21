Opening libraries and reading the files


```r
library(randomForest)
library(caret)
trainn<-read.csv("train.csv",na.strings=c("", "NA", "NULL"))
testt<-read.csv("test.csv",na.strings=c("", "NA", "NULL"))
dim(trainn)
```

```
## [1] 19622   160
```

```r
dim(testt)
```

```
## [1]  20 160
```

Eliminating the columns with NA factors


```r
na_train = sapply(trainn, function(x) {sum(is.na(x))})
table(na_train)
```

```
## na_train
##     0 19216 
##    60   100
```

```r
trainn<-trainn[,colSums(is.na(trainn))==0]
dim(trainn)
```

```
## [1] 19622    60
```

```r
na_test = sapply(testt , function(x) {sum(is.na(x))})
table(na_test )
```

```
## na_test
##   0  20 
##  60 100
```

```r
testt <-testt [,colSums(is.na(testt ))==0]
dim(testt )
```

```
## [1] 20 60
```
Eliminating the first seven columns as unnecessaries for the prediction


```r
trainn<-trainn[,-c(1:7)]
dim(trainn)
```

```
## [1] 19622    53
```

```r
testt<-testt[,-c(1:7)]
dim(testt)
```

```
## [1] 20 53
```

Partitionating data


```r
ttrain <- createDataPartition(y = trainn$classe, p=0.75, list=FALSE)
train1 <- trainn[ttrain,]
test1 <- trainn[-ttrain,]
```

Training random forest model


```r
model <- randomForest(classe~.,data=train1)
```

Calculating the confusion matrix


```r
confusionMatrix(predict(model,newdata=test1 [,-ncol(test1 )]),test1 $classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1394    5    0    0    0
##          B    1  943    7    0    0
##          C    0    1  848   13    1
##          D    0    0    0  790    2
##          E    0    0    0    1  898
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9937         
##                  95% CI : (0.991, 0.9957)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.992          
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9993   0.9937   0.9918   0.9826   0.9967
## Specificity            0.9986   0.9980   0.9963   0.9995   0.9998
## Pos Pred Value         0.9964   0.9916   0.9826   0.9975   0.9989
## Neg Pred Value         0.9997   0.9985   0.9983   0.9966   0.9993
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2843   0.1923   0.1729   0.1611   0.1831
## Detection Prevalence   0.2853   0.1939   0.1760   0.1615   0.1833
## Balanced Accuracy      0.9989   0.9958   0.9941   0.9910   0.9982
```

Making predictions


```r
answers  <- predict(model,newdata=testt)
print(answers )
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

Creating prediction files for the project


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./answers/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers )
```
