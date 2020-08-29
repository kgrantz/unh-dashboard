#data to create the dummy .csv file. Very coarsely written code but use it if
#you want to tinker with the dummy data

#college column unique values
college <- data.frame(college=c("Durham","Concord"))

#personnel column unique values
personnel <- data.frame(person = c("Student","Faculty"))

#campus location column unique values
campus <- data.frame(campus=c("Off","On"))

#weeks column unique values
weeks <- data.frame(week_no=as.character(c(1:15)))


#for campus location tab; cross join everything to get all required combinations. Then make a level column
data_campus <- merge(college,campus) %>%
  merge(weeks)%>%
  mutate(level=campus) 


data_campus$id <-"campus"


#for personnel location tab; cross join everything to get all required combinations. Then make a level column
data_personnel <- merge(college,personnel) %>%
  merge(weeks)%>%
  mutate(level=person)

data_personnel$id <- "person"


#row bind the data of different tabs. We can instead have these as 2 datasets also
data <- rbind(data_campus[ ,c("college","id","level","week_no")],data_personnel[ ,c("college","id","level","week_no")])


#randomly generated values for all metrics
data$isolation <- sample(1:20,nrow(data),replace=TRUE)

data$quarantine <- sample(1:20,nrow(data),replace=TRUE)

data$symptomatic <- sample(1:10,nrow(data),replace=TRUE)


data$cases<- sample(1:20,nrow(data),replace=TRUE)
