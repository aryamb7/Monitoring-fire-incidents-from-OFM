

smp_size <- floor(0.75 * nrow(main_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(main_data)), size = smp_size)

train <- main_data[train_ind, ]
test <- main_data[-train_ind, ]
install.packages("writexl")
library(writexl)
write_xlsx(x=test,path="test.xlsx",col_names = TRUE)
write_xlsx(x=train,path="train.xlsx",col_names = TRUE)
write_xlsx(x=main_data,path="main_data.xlsx",col_names = TRUE)

main_data<-main_data[sample(nrow(main_data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(main_data)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- main_data[testIndexes, ]
  trainData <- main_data[-testIndexes, ]
  
}
write_xlsx(x=testData,path="testDatafold.xlsx",col_names = TRUE)
sapply(test, function(x) sum(is.na(x)))
sapply(train, function(x) sum(is.na(x)))

library(dplyr)

mutate(df, train)

PATH <- "C:\Users\User\Documents\big data project\FIRE MONITORING1\train"
df_train <- read.csv(PATH, sep = ",")

list_na <- colnames(df_train)[ apply(df_train, 8, anyNA) ]

sum(is.na(test))
sum(is.na(train))
sum(is.na(main_data))
na.omit(main_data)
any(is.na(main_data))




sum(is.na(main_data))
str(main_data)
complete.cases(main_data)
x<-na.omit(main_data)
str(x)

#checking how many missing values in test train and main data
sum(is.na(test))
sum(is.na(train))
sum(is.na(main_data))


#cleaning data
x<-na.omit(main_data)
str(x)


#checking if any missing values
sum(is.na(x))

# dividing the cleaned data x into training and testing data
smp_size <- floor(0.75 * nrow(x))
set.seed(123)
train_indx <- sample(seq_len(nrow(x)), size = smp_size)

trainx <- x[train_indx, ]
testx <- x[-train_indx, ]

library(writexl)
write_xlsx(x=testx,path="testx.xlsx",col_names = TRUE)
write_xlsx(x=trainx,path="trainx.xlsx",col_names = TRUE)
write_xlsx(x=x,path="x.xlsx",col_names = TRUE)





