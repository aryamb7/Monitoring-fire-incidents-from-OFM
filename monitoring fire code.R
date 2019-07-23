
#dividing into train an test data
  smp_size <- floor(0.75 * nrow(main_data))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(main_data)), size = smp_size)
  train <- main_data[train_ind, ]
  test <- main_data[-train_ind, ]

#writing train and test data file
  install.packages("writexl")
  library(writexl)
  write_xlsx(x=test,path="test.xlsx",col_names = TRUE)
  write_xlsx(x=train,path="train.xlsx",col_names = TRUE)
  write_xlsx(x=main_data,path="main_data.xlsx",col_names = TRUE)

#omitting missing values
  datax<-na.omit(main_data)
  str(datax)
  view(datax)


#diving the cleaned data into test and train
  smp_size <- floor(0.75 * nrow(datax))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(datax)), size = smp_size)
  trainx <- datax[train_ind, ]
  testx <- datax[-train_ind, ]

#trying pie chart
  mytable <- table(trainx$`Call Source`)
  mytable
  lbls <- paste(names(mytable),"\n",mytable,sep="")
  pie(mytable,labels=lbls,
    main="piechart")

  library(plotrix)
  mytable <- table(trainx$`Call Source`)
  mytable
  lbls <- paste(names(mytable),"\n",mytable,sep="")
  pie3D(mytable,labels=lbls,explode = 0.1,
      main="piechart")

  library(plotrix)
  mytable1 <- table(trainx$`Final Incident Type`)
  mytable1
  lbls <- paste(names(mytable1),"\n",mytable1,sep="")
  pie3D(mytable1,labels=lbls,explode = 0.1,
      main="piechart")


#3d pie chart
  library(plotrix)
  mytable3 <- table(trainx$`Call Source`)
  mytable3
  sort(mytable3)
  lbls <- c("01 - 911","  03 - From Ambulance"," 601 - Trench Rescue (non fire)")
  pie3D(mytable3,labels=lbls,explode = 0.1,
      main="piechart")


#histogram one
  ggplot(trainx, aes(`Incident Station Area`)) + geom_histogram(binwidth = 5)+
  scale_x_continuous("Area", breaks = seq(50,500,by = 50))+
  scale_y_continuous("Count", breaks = seq(0,20000,by = 1000))+
  labs(title = "Histogram")

#sorting
  mytable4 <- table(trainx$`Incident Station Area`)
  sort(mytable4,decreasing = TRUE)
  mytable4


#histogram two
  ggplot(trainx, aes(`Incident Ward`)) + geom_histogram(binwidth = 2)+
  scale_x_continuous("Ward", breaks = seq(0,30,by = 1))+
  scale_y_continuous("Count", breaks = seq(0,50000,by = 10000))+
  labs(title = "Histogram")

#sorting
  mytable5 <- table(trainx$`Incident Ward`)
  sort(mytable5,decreasing = TRUE)
  mytable5


#qplot one
  trainx <- within(trainx, `Initial CAD Event Type` <- factor(`Initial CAD Event Type`, levels=names(sort(table(`Initial CAD Event Type`), decreasing=TRUE))))
  qplot(`Initial CAD Event Type`, data = trainx, geom = "bar", fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#sorting
  sort(table(trainx$`Initial CAD Event Type`),decreasing=TRUE)


#qplot two
  library(ggplot2)
  testx <- within(testx, `Initial CAD Event Type` <- factor(`Initial CAD Event Type`, levels=names(sort(table(`Initial CAD Event Type`), decreasing=TRUE))))
  qplot(`Initial CAD Event Type`, data = testx, geom = "bar", fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))


#qplot three
  trainx <- within(trainx, `Call Source` <- factor(`Call Source`, levels=names(sort(table(`Call Source`), decreasing=TRUE))))
  qplot(`Call Source`, data = trainx, geom = "bar", fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))


#doing factor operation
  str(trainx)
  trainx$`Call Source` <- factor(trainx$`Call Source`)
  trainx$`Incident Ward` <- factor(trainx$`Incident Ward`)


#findind min,max etc....
  summary(trainx$`Call Source`)

#trying tableplot
  library(tabplot)
  tableplot(test)

  tableplot(trainx,select=c(`Call Source`,`Persons Rescued`,`Initial Event Type`),sortCol=`Call Source`)


#scatterplot one
library(ggplot2)
  ggplot(trainx,aes(x=`Initial CAD Event Type`,y=`Persons Rescued`))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#scatterplot two
  ggplot(trainx,aes(x=`Initial CAD Event Type`,y=`Incident Ward`))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
         
  
         

