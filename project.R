trainn<-read.csv("train.csv",na.strings=c("", "NA", "NULL"))
testt<-read.csv("test.csv",na.strings=c("", "NA", "NULL"))
dim(trainn)
dim(testt)

na_train = sapply(trainn, function(x) {sum(is.na(x))})
table(na_train)

trainn<-trainn[,colSums(is.na(trainn))==0]
dim(trainn)

na_test = sapply(testt , function(x) {sum(is.na(x))})
table(na_test )

testt <-testt [,colSums(is.na(testt ))==0]
dim(testt )

trainn<-trainn[,-c(1:7)]
dim(trainn)

testt<-testt[,-c(1:7)]
dim(testt)

library(caret)

ttrain <- createDataPartition(y = trainn$classe, p=0.75, list=FALSE)
train1 <- trainn[ttrain,]
test1 <- trainn[-ttrain,]

library(randomForest)
model <- randomForest(classe~.,data=train1)


confusionMatrix(predict(model,newdata=test1 [,-ncol(test1 )]),test1 $classe)

answers <- predict(model,newdata=testt)
print(answers )

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./answers/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers )