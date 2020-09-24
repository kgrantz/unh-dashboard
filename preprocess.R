##install.packages("aweek")

library(tidyverse)
library(aweek)

## Load Data (USE THESE R OBJECT NAMES!)

#loading routinetesting and coalescing as (resultdate/collectdate)
routinetesting<- read_csv("raw_data/routinetesting.csv") %>%
  mutate(date=ifelse(is.na(resultsdate),
                     as.character(collectdate),
                     as.character(resultsdate))) %>%
  mutate(date=as.Date(date))%>%
  #recode the results
  mutate(result=recode(result,
                       Invalid = "Invalid / Rejected / Not Performed",
                       Rejected = "Invalid / Rejected / Not Performed",
                       `Test Not Performed` = "Invalid / Rejected / Not Performed",
                       `No Result` = "Invalid / Rejected / Not Performed",
                       `Inconclusive`="Inconclusive",
                       Positive= "Positive",
                       Negative="Negative",
                       .default=NA_character_
  ))

isolationquarantine<-read_csv("raw_data/isolationquarantine.csv")
individualdemographics <- read_csv("raw_data/individualdemographics.csv") %>%
  mutate(user_status=recode(user_status,
                            Employee="Employee / Staff",
                            Staff="Employee / Staff"                      
  ))

## Helper strings --------------- 

  # Save day of last data generation
  updated.date <- Sys.Date()

## Dashboard objects ------------
  
  #################
  ### HOME PAGE ###
  #################

  # overall epi curve:  ------ SOWMYA
    # week should be collection date where available, results date where collection date isn't available
    # use CDC default for epi-weeks (Sunday - Saturday) -- think there's an epiweek pkg/function
    # metrics = # new positive cases

 ## adding week number and week start date for date
routinetesting_w_week <- routinetesting %>%
                         mutate(week_no = date2week(date, week_start=7, floor_day=TRUE, numeric=TRUE))


#getting a continuous string of weeks
#from min week available
#to max week available
#to include rows with 0 where there are no week values

      min_week <- min(routinetesting_w_week$week_no)
      max_week <- max(routinetesting_w_week$week_no)
      cont_week <- data.frame(week_no = seq(min_week, max_week, 1))




## rolling up to week level
epi_curve_overall_week <- routinetesting_w_week[ ,c("result","week_no")] %>%
                          group_by(week_no) %>%
                          ## cases = count of positive results
                          summarise(cases = sum(result == "Positive")) %>%
                          right_join(cont_week) %>%
                          ## replacing NA with 0 - later check why we have NAs
                          mutate(cases=ifelse(is.na(cases),0,cases)) %>%
                          mutate(week_start_date=get_date(week_no, start=7))
                          






  # statewide conditions 
    # manually enter into a named vector with 14-day total incident cases, currently hospitalized, current restrictions, date updated
    # figure out if we can api in DHHS data?
  
  # data frame column campus
    # campus (durham, manchester, concord/law)
    # n_isol = active in isolation as of latest data
    # n_isol_sym = active in isolation + symptomatic as of latest data
    # n_quar = number actively in quarantine as of latest data
    # % quarantine beds occupied - not sure what these denominators are
    # % isolation beds occupied - not sure what these denominators are
    # case rate per 1000
    # number active cases currently? should be n_isol if they're actually isolating everbody
  
  # sanity checks
    if(n_isol_sym>n_isol){stop("this is wrong")}
  
  ###################
  ### CAMPUS PAGE ###
  ###################
  
  # campus epi curve --- SOWMYA
    # columns: week, var_type, var_level, count
    # week should be collection date where available, results date where collection date isn't available
    # use CDC default for epi-weeks (Sunday - Saturday) -- think there's an epiweek pkg/function
    # A fac_stu fac x
    # A fac_stu stu y
    # A on_off on   a
    # A on_off off  b
    # can add other metrics but not currently used in dashboard:
    # newly isolated, # newly quarantined, average # in isolation, average # quarantined, total # tested

   ##merging routine testing and demo table for additional info
routinetesting_w_week_demo <- routinetesting_w_week %>%
                              left_join(individualdemographics, by="uid")
                          


##table skeleton for campus curve
##we need to create table skeleton
##to keep rows with 0 cases when there is no information for any combination
##I know it looks like a tedious way to do it but its economic in case we
# get more levels

##check with Kyra and Forrest which campuses to keep
campus <- data.frame(campus= unique(individualdemographics$campus))

#personnel column unique values
personnel <- data.frame(user_status = c("Student","Faculty"))

#campus location column unique values
campus_location <- data.frame(campus_location=c("Off Campus","On Campus"))

#weeks column unique values
weeks <- data.frame(week_no=cont_week)

#for campus location tab; cross join everything to get all required combinations. Then make a level column

table_levels_campus <- merge(campus,campus_location) %>%
                       merge(weeks)%>%
                       mutate(level=campus_location, id="campus_location")


##TO DO : in dashboard server, change the filters for campus and campus location

##rolling up date level data to required levels
routinetesting_campus_location<- routinetesting_w_week_demo[ ,c("result","week_no","campus","campus_location")] %>%
                                 group_by(week_no, campus,campus_location) %>%
                                 ## cases = count of positive results
                                 summarise(cases = sum(result == "Positive"))%>%
                                 ##getting all the levels missed by roll up not having any data
                                 right_join(table_levels_campus) %>%
                                 ## replacing NA with 0 - later check why we have NAs if present in raw data
                                 mutate(cases=ifelse(is.na(cases),0,cases)) %>%
                                 mutate(week_start_date=get_date(week_no, start=7))




#for personnel location tab; cross join everything to get all required combinations. Then make a level column
table_levels_user <- merge(campus,personnel) %>%
                     merge(weeks)%>%
                     mutate(level=user_status, id="user_status")



##rolling up date level data to required levels
routinetesting_campus_personnel<- routinetesting_w_week_demo[ ,c("result","week_no","campus","user_status")] %>%
                                  group_by(week_no, campus,user_status) %>%
                                  ## cases = count of positive results
                                  summarise(cases = sum(result == "Positive"))%>%
                                  right_join(table_levels_user) %>%
                                  ## replacing NA with 0 - later check why we have NAs
                                  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
                                  mutate(week_start_date=get_date(week_no, start=7))



#row bind the data of different tabs. We can instead have these as 2 datasets also
#final table for epi curve 
routinetesting_demograph_epi_curve <- rbind(routinetesting_campus_location[ ,c("campus","id","level","week_no","week_start_date","cases")],
                                            routinetesting_campus_personnel[ ,c("campus","id","level","week_no","week_start_date","cases")])

    
  # campus specific numbers come from the thresholds data 
     
  
  # dorm-specific table for each campus ------FORREST
    #data frame with cumulative counts
    # column campus
    # column dorms (with off campus) -- intersection of (off_campus, campus)
    # column incident cases (new positive tests) in last 14 days
    # column % positive (# incident cases / # with at least one test result (positive/negative) in last 14 weeks) 
    # # quarantined
    # % all beds occupied
    # TO DO: decide if this is best "% positive" metric or want something different
    
  # student/faculty-specific table for each campus ------FORREST
  #data frame with cumulative counts
  # column campus
  # column user_status
  # column incident cases (new positive tests) in last 14 days
  # column % positive (# incident cases / # with at least one test result (positive/negative) in last 14 weeks) 
  # # quarantined
  # % all beds occupied
  # TO DO: decide if this is best "% positive" metric or want something different

#cases testing positive in the last 14 days
cases14 <- routinetesting %>% filter(result=="Positive") %>%
  left_join(individualdemographics) %>%
  #change the date
  mutate(date=ifelse(is.na(resultsdate),
                     as.character(collectdate),
                     as.character(resultsdate))) %>%
  mutate(date=as.Date(date)) %>%
  #only include those conducted in the last two weeks
  filter(date > (Sys.Date()-14)) %>%
  filter(date <= Sys.Date())


# those who are quarantined at the moment
quarantined <- isolationquarantine %>% 
  #limit to those which have an entry date before
  filter(quar_entrydate<=Sys.Date()) %>%
  #limit to those who have an exit day after
  filter(quar_exitdate>Sys.Date()) %>%
  distinct(uid)


# number of people who are being tested (should this be our denominator???)
censussf <- distinct(individualdemographics,uid,campus,user_status) %>%
  group_by(campus,user_status) %>%
  summarize(pop=n()) 

# number of those quarantined by each campus / user status
quarsf <- quarantined %>% left_join(individualdemographics) %>%
  group_by(campus,user_status) %>%
  summarize(quarantined=n())

#final table for student faculty
studentfaculty <- cases14 %>%
  #remove any cases that show up twice with multiple tests
  distinct(uid,campus,user_status)%>%
  group_by(campus,user_status) %>%
  summarize(cases=n()) %>%
  #include the zeroes
  full_join(censussf) %>%
  filter(campus %in% c("UNH Durham","UNH LAW",
                       "UNH Manchester"
  ))%>%
  filter(!is.na(user_status))%>%
  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  #calculate rate per 1000
  mutate(rate=cases/pop*1000)%>%
  #add the number of people quarantined
  left_join(quarsf) %>%
  mutate(quarantined=ifelse(is.na(quarantined),
                            0,quarantined)) 

  # testing "epi curve" ------------------------FORREST
    # data frame -- DAY LEVEL for plot
    # column campus
    # column date (day) -- date of results (fill in with date of collection where date of result is not needed)
    # column test_type -- factor: negative, positive, etc.
        ## TO DO -- figure out what all the levels mean -- e.g. is inconclusive "pending"?
        ## for now keep the levels they've provided until we figure out which ones can be merged together
    # column count -- number in each category

tecCampus1 <- routinetesting %>% left_join(individualdemographics) %>%
  #remove those with missing or free text responses or inconclusive
  filter(!is.na(result)) %>%
  #remove those not on UNH Durham, Manchester or Law
  filter(campus %in% c("UNH Manchester","UNH Durham","UNH LAW")) %>%
  #only include those conducted in the last two weeks
  filter(date > (Sys.Date()-14)) %>%
  filter(date <= Sys.Date()) %>%
  group_by(campus, date,result) %>%
  summarize(tests=n())


tecCampusfinal <- tecCampus1 %>%
                  #add zeroes to days missing for each category
                  full_join(expand_grid(campus=unique(tecCampus1$campus),
                                                       date=seq.Date(min(tecCampus1$date),
                                                                     Sys.Date(),by="days"),
                                                       result=unique(tecCampus1$result))) %>%
                  mutate(tests=ifelse(is.na(tests),0,tests))

  
  # data frame -- WEEK LEVEL for table
  # summarize the daily table by week to produce columns:
    # campus
    # week
    # n_submitted = sum of all tests collected within that week, regardless of current outcome (include not tested, etc.)
    # n_not_submitted = (all tests that should have been collected - all tests that were collected) 
    # % positive tests = # all positive tests / (# all positive tests + # all negative tests)
    # to figure out what they're doing about pooled samples - does the updated result of a pooled sample go in as a new row (so rows are tests) or are rows just samples
  
    pct_pos <- test_data %>% 
      group_by(date) %>% 
      summarise(n_pos = n_test[which(test_type=="Positive")],
                n_neg = n_test[which(test_type=="Negative")],
                n_tot = sum(n_test),
                n_not_subm = round(n_tot*0.1),
                pct_pos = n_pos/ (n_pos + n_neg),
                pct_pos_label = glue("{round(pct_pos*100,1)}%"))
 
  # testing delays
    # we can't really do this yet
    # eventually, data frame of 
      # campus
      # lab
      # median days from sample collection to test results returned in last 2 weeks
  
## Report/misc. objects ----------
  
  # leave notes here if you think of other things that would be helpful!
    
## Export  --------------- 
    filename <- paste0("data/",Sys.Date(),"/processed_data.Rdata")
    save(file=filename,
         list = c("updated.date"))
      
