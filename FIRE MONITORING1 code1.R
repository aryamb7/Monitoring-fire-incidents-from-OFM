

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

sum(is.na(testx))
sum(is.na(trainx))
sum(is.na(x))


#finding unique values in columns

table(trainx$`Final Incident Type`)

#bar plot vizualisation of Initial CAD Event type

library(ggplot2)
Incident <- table(trainx$`Initial CAD Event Type`)
Incident
barplot(Incident)

# to put bars in descending order

barplot(Incident[order(Incident, decreasing=TRUE)])
barplot(Incident,col=rainbow(50))

table(trainx$`Initial CAD Event Type`)

cal<-table(testx$`Call Source`)
cal
barplot(cal)

plot(x=trainx$'Incident Station Area',
     y=trainx$'Incident Ward',
     pch=19,cex=0.8,
     frame=FALSE,
     xlab="STATION AREA",
     ylab="INCIDENT WARD")


boxplot('Incident Ward'~'Incident Station Area',
        data=x,
        ylab="INCIDENT TYPE",
        frame=FALSE,
        col="lightgray")


#barplotting for incident ward
Incidentsw <- table(trainx$`Incident Ward`)
Incidentsw
barplot(Incidentsw)

library(ggplot2)

trainx <- within(trainx, 'Initial CAD Event Type' <- factor('Initial CAD Event Type', levels=names(sort(table('Initial CAD Event Type'), decreasing=TRUE))))
qplot('Initial CAD Event Type', data = trainx, geom = "bar", fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))


#barplotting for incident station area
Incidentsa <- table(trainx$`Incident Station Area`)
Incidentsa
trainx <- within(trainx, 'Incidentsa' <- factor('Incidentsa', levels=names(sort(table('Incidentsa'), decreasing=TRUE))))
qplot('Incidentsa', data = trainx, geom = "bar", fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))


#barplot(Incidentsa)

#barplotting for call source
Incidentsc <- table(trainx$`Call Source`)
Incidentsc
barplot(Incidentsc)


#barplotting for intersection
Incidentsi <- table(trainx$`Intersection`)
Incidentsi
barplot(Incidentsi)


#barplotting for Persons Rescued
Incidentsp <- table(trainx$`Persons Rescued`)
Incidentsp
barplot(Incidentsp)




#tableplot

require(ggplot2)
data(trainx)


tableplot(trainx)
tableplot(trainx, select = c(Final_Incident_Type,Initial_CAD_Event_Type,Incident_Station_Area,Incident_Ward), sortCol = Initial_CAD_Event_Type)


ggplot(data=trainx)+geom_point(mapping=aes(x= 'Final Incident Type' , y= 'Persons Rescued'))
library(ggplot2)
ggplot(data=testx)+
geom_point(mapping=aes(x= 'Final Incident Type' , y= 'Persons Rescued'))

