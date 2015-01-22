urltrain <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
urltest <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

download.file(urltrain, destfile="./R/data/temp1.csv")
download.file(urltest, destfile="./R/data/temp2.csv")

training <- read.csv("./R/data/temp1.csv")
testing <- read.csv("./R/data/temp2.csv")

library(caret)
set.seed(1)

# remove any nzv in training 
nzv <- nearZeroVar(training,saveMetrics=TRUE)

#remove any variables with NA in tesitng (these can't contribute...)
nas <- data.frame(c(1))
for(i in 1:160) {
  nas[i,1] <- data.frame(sum(is.na((testing[,i]))))
}
nas2 <- data.frame(c(1))
for(i in 1:160) {
  nas2[i,1] <- data.frame(sum(is.na((training[,i]))))
}
colnames(nas2) <- "test"

nn2 <- cbind(nzv,nas,nas2)

exclude1 <- rownames(subset(nn, nzv==TRUE & c.1.==0))
exclude2 <- rownames(subset(nn, c.1.==20))
exclude <- c(exclude1, exclude2)

vars <- names(training) %in% exclude

training2 <- training[!vars]
training3 <- training2[,6:59]

modFit <-train(classe ~ ., data=training3, method="rpart")

#testing2 <- testing[!vars]
predict(modFit, testing)

modFit1 <-train(classe ~ ., data=training[,6:160], method="rpart")

#testing2 <- testing[!vars]
predict(modFit1, testing)

#modFit <-train(classe ~ ., data=data2, method="rf")

