
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
  
  #checking number of NA's in train and test data
  sum(is.na(test))
  sum(is.na(train))
  
  

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
  
  
#checking NA values after omission
  sum(is.na(testx))
  sum(is.na(trainx))
  

#trying simple pie chart
  mytable <- table(trainx$`Call Source`)
  mytable
  lbls <- paste(names(mytable),"\n",mytable,sep="")
  pie(mytable,labels=lbls,
    main="piechart")
  
  #3d piechart for final incident type
  library(plotrix)
  mytable1 <- table(trainx$`Final Incident Type`)
  mytable1
  lbls <- paste(names(mytable1),"\n",mytable1,sep="")
  pie3D(mytable1,labels=lbls,labelcex=.8,explode = 0.1,
      main="piechart")


#3d pie chart for selected call sources
  library(plotrix)
  mytable3 <- table(trainx$`Call Source`)
  mytable3
  sort(mytable3)
  lbls <- paste(names(mytable3),"\n",mytable3,sep="")
  #lbls <- c(" 03 - From Ambulance"," 01 - 911","05 - Telephone from Monitoring Agency ","04 - From Police Services","","","","",""," 10 - No Alarm Received - No Response")
  pie3D(mytable3,labels=lbls,labelcex=.8,explode = 0.2,
  main="PIE CHART")
  
  #3d pie chart for all call sources
  library(plotrix)
  mytable3 <- table(trainx$`Call Source`)
  mytable3
  sort(mytable3)
  lbls <- paste(names(mytable3),"\n",mytable3,sep="")
  pie3D(mytable3,labels=lbls,labelcex=.8,explode = 0.2,
        main="PIE CHART")
  
  #3d pie chart for all incident wards
  library(plotrix)
  mytable4 <- table(trainx$`Incident Ward`)
  mytable4
  sort(mytable4)
  lbls <- paste(names(mytable4),"\n",mytable4,sep="")
  pie3D(mytable4,labels=lbls,labelcex=.8,explode = 0.2,
        main="PIE CHART FOR WARDS")

  
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
  scale_x_continuous("Ward", breaks = seq(1,25,by = 1))+
  scale_y_continuous("Count", breaks = seq(0,50000,by = 10000))+
  labs(title = "Histogram")
  
#histogram three
  ggplot(trainx, aes(`Call Source`)) + geom_histogram(binwidth = 5)+
    #scale_x_continuous("Call Source", breaks = seq(50,500,by = 50))+
    scale_y_continuous("Count of incidents", breaks = seq(0,20000,by = 1000))+
    labs(title = "HISTOGRAM FOR CALL SOURCE")

#sorting
  mytable5 <- table(trainx$`Incident Ward`)
  sort(mytable5,decreasing = TRUE)
  mytable5

#sorting
  sort(table(trainx$`Initial CAD Event Type`),decreasing=TRUE)
  sort(table(trainx$`Call Source`),decreasing=TRUE)
  sort(table(trainx$`Final Incident Type`),decreasing=TRUE)
  sort(table(trainx$`Intersection`),decreasing=TRUE)
  sort(table(trainx$`Persons Rescued`),decreasing=TRUE)
  
#qplot one
  trainx <- within(trainx, `Initial CAD Event Type` <- factor(`Initial CAD Event Type`, levels=names(sort(table(`Initial CAD Event Type`), decreasing=TRUE))))
  qplot(`Initial CAD Event Type`, data = trainx, geom = "bar", fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
  
#qplot two
  trainx <- within(trainx, `Final Incident Type` <- factor(`Final Incident Type`, levels=names(sort(table(`Final Incident Type`), decreasing=TRUE))))
  qplot(`Final Incident Type`, data = trainx, geom = "bar",ylab="count of incidents",main="FINAL INCIDENT TYPE",cex.main=2, fill = I("blue"))+
    theme(axis.text.x=element_text(angle=90,hjust=1))
  
#qplot three
  trainx <- within(trainx, `Call Source` <- factor(`Call Source`, levels=names(sort(table(`Call Source`), decreasing=TRUE))))
  qplot(`Call Source`, data = trainx, geom = "bar",ylab="count of incidents",main="CALL SOURCE",cex.main=2, fill = I("blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
  
#qplot four
  trainx <- within(trainx, `Persons Rescued` <- factor(`Persons Rescued`, levels=names(sort(table(`Persons Rescued`), decreasing=TRUE))))
  qplot(`Persons Rescued`, data = trainx, geom = "bar",ylab="count of incidents",main="PERSONS RESCUED",cex.main=2, fill = I("blue"))+
    theme(axis.text.x=element_text(angle=90,hjust=1))
  
#doing factor operation
  str(trainx)
  trainx$`Call Source` <- factor(trainx$`Call Source`)
  trainx$`Incident Ward` <- factor(trainx$`Incident Ward`)


#findind min,max etc....
  summary(trainx$`Call Source`)
  summary(trainx$`Incident Ward`)
  summary(trainx$`Persons Rescued`)
  summary(trainx$`Intersection`)
  
  
  
  
#trying tableplot
  library(tabplot)
  tableplot(test)
  tableplot(trainx,select=c(`Call Source`,`Persons Rescued`,`Initial Event Type`),sortCol=`Call Source`)


#scatterplot one
library(ggplot2)
  ggplot(trainx,aes(x=`Initial CAD Event Type`,y=`Persons Rescued`))+
  geom_point(aes(color="blue"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#scatterplot two
  ggplot(trainx,aes(x=`Incident Ward`,y=`Persons Rescued`))+
  geom_point(aes(color="Red"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))
  
#scatterplot three
   ggplot(trainx,aes(x=`Incident Station Area`,y=`Persons Rescued`))+
    geom_point(aes(color="Red"))+
    theme(axis.text.x=element_text(angle=90,hjust=1))

#scatterplot four
  library(ggplot2)
  ggplot(trainx,aes(x=`Final Incident Type`,y=`Persons Rescued`))+
    geom_point(aes(color="red"))+
    theme(axis.text.x=element_text(angle=90,hjust=1))
         
#finding unique values in columns
   table(trainx$`Final Incident Type`)
   table(trainx$`Call Source`)
   
  
#bar plot vizualisation of Initial CAD Event type
  library(ggplot2)
  Incident <- table(trainx$`Initial CAD Event Type`)
  Incident
  barplot(Incident,col=rainbow(30),ylab="Count of Incidents",ylim=c(0,400000),font.lab=2,main="INITIAL CAD EVENT TYPE", font.axis = 4, col.axis = "Black", las = 2,cex.lab=1,cex.main=2 ) 
  
  
#barplot of finalincident type
  library(ggplot2)
  Incidentfi <- table(trainx$`Final Incident Type`)
  Incidentfi
  barplot(Incidentfi[order(Incidentfi, decreasing = TRUE)],col=rainbow(30),ylab="Count of Incidents",ylim=c(0,400000),font.lab=2,main=" FINAL INCIDENT TYPE", font.axis = 4, col.axis = "Black", las = 2,cex.lab=1,cex.main=2 ) 
  
  
#barplot of call source
  library(ggplot2)
  Incidentcall <- table(trainx$`Call Source`)
  Incidentcall
  barplot(Incidentcall[order(Incidentcall, decreasing = TRUE)],col=rainbow(30),ylab="Count of calls",ylim=c(0,400000),font.lab=2,main="CALL SOURCE", font.axis = 4, col.axis = "Black", las = 2,cex.lab=1,cex.main=2)
  text(x=Incidentcall,labels=Incidentcall,srt=360,pos=3.8,xpd=TRUE,cex=.8)
  
  
#barplotting for incident ward
  Incidentw <- table(trainx$`Incident Ward`)
  Incidentw
  barplot(Incidentw[order(Incidentw, decreasing = TRUE)],col=rainbow(30),xlab="Ward Number",ylab="Count of incidents",ylim=c(0,50000),font.lab=2,main="INCIDENT WARD", font.axis = 4, col.axis = "Black", las = 2,cex.lab=1,cex.main=2)
  
#barplotting for incident station area
  Incidents <- table(trainx$`Incident Station Area`)
  Incidents
  barplot(Incidents[order(Incidents, decreasing = TRUE)],col=rainbow(30),xlab="Station Area",ylab="Count of incidents",ylim=c(0,20000),font.lab=2,main="INCIDENT STATION AREA", font.axis = 4, col.axis = "Black", las = 2,cex.lab=1,cex.main=2)
  
#barplotting for intersection
  Incidenti <- table(trainx$Intersection)
  Incidenti
  barplot(Incidenti[order(Incidenti, decreasing = TRUE)],col="pink",xlab="Intersection",ylab="Count of incidents",ylim=c(0,2000),font.lab=2,main="INTERSECTION", font.axis = 4, col.axis = "Black", las = 2,cex.lab=1,cex.main=2)
  
# to put bars in descending order
  barplot(Incidentfi[order(Incidentfi, decreasing=TRUE)])
  barplot(Incident,col=rainbow(50))
  
  
  

         

