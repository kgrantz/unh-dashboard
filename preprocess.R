
#### Environment + Data Load ----------------------- ####

require(tidyverse)
require(aweek)

updated.date <- Sys.Date()

state.updated.date <- as.Date("2020-09-23") 
# change this to character string if you do not update the state data

routinetesting<- read_csv("raw_data/routinetesting.csv") %>%
  mutate(date=ifelse(is.na(collectdate),
                     as.character(resultsdate),
                     as.character(collectdate))) %>%
  mutate(date=as.Date(date))%>%
  # recode the results
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

isolationquarantine <- read_csv("raw_data/isolationquarantine.csv")

individualdemographics <- read_csv("raw_data/individualdemographics.csv") %>%
  mutate(user_status=recode(user_status,
                            Employee="Employee / Staff",
                            Staff="Employee / Staff",
                            Unknown="Employee / Staff" ## per call 9/24, these are mostly contractors
  )) %>%
  ##defining column campus location as its not explicitly defined
  mutate(campus_location = ifelse(is.na(dorm),"Off Campus","On Campus"))




#### HOME PAGE ---------------------------------- ####

### Overall epi curve ------------------------------- 
### SOWMYA
# week should be collection date where available, results date where collection date isn't available
# use CDC default for epi-weeks (Sunday - Saturday) -- think there's an epiweek pkg/function
# metrics = # new positive cases

# adding week number and week start date for date
routinetesting_w_week <- routinetesting %>%
  mutate(week_no = date2week(date, week_start=7, floor_day=TRUE, numeric=TRUE))

# getting a continuous string of weeks
# from min week available
# to max week available
# to include rows with 0 where there are no week values
min_week <- min(routinetesting_w_week$week_no)
max_week <- max(routinetesting_w_week$week_no)
cont_week <- data.frame(week_no = seq(min_week, max_week, 1))

# rolling up to week level
epi_curve_overall_week <- routinetesting_w_week[ ,c("result","week_no")] %>%
  group_by(week_no) %>%
  ## cases = count of positive results
  summarise(cases = sum(result == "Positive")) %>%
  right_join(cont_week) %>%
  ## replacing NA with 0 - later check why we have NAs
  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  mutate(week_start_date=get_date(week_no, start=7))



### Statewide conditions ------------------------------- 
### Kyra
state_curr_cases <- 266 
state_curr_hosp <- 17
state_curr_cond <- "Limited Open"
# should be either "None", for no restrictions
#                  "Open", for restrictions on mass gatherings, otherwise open
#                  "Limited Open", for many restrictions but not shut-down
#                  "Limited", for stay-at-home order
# TO DO: these are poorly defined categories, should come up with something more precise
# TO DO: figure out if we can api into DHHS data somehow? otherwise write this as an `opt` statement in wrapper script


### Thresholds data frame ------------------------------- 
### Kyra
# campus (durham, manchester, concord/law)
# n_isol = active in isolation as of latest data
# n_isol_sym = active in isolation + symptomatic as of latest data
# n_quar = number actively in quarantine as of latest data
# % quarantine beds occupied - not sure what these denominators are
# % isolation beds occupied - not sure what these denominators are
# case rate per 1000
# number active cases currently? should be n_isol if they're actually isolating everbody

# Cases testing positive in the last 10 days
cases10 <- routinetesting %>% 
  filter(result=="Positive") %>%
  left_join(individualdemographics) %>%
  #change the date
  mutate(date=ifelse(is.na(collectdate),
                     as.character(resultsdate),
                     as.character(collectdate))) %>%
  mutate(date=as.Date(date)) %>%
  #only include those conducted in the last two weeks
  filter(date > (Sys.Date()-10)) %>%
  filter(date <= Sys.Date())

# # Currently quarantined
# quarantined <- isolationquarantine %>% 
#   #limit to those which have an entry date before
#   filter(quar_entrydate<=Sys.Date()) %>%
#   #limit to those who have an exit day after
#   filter(quar_exitdate>Sys.Date()) %>%
#   distinct(uid)

# those who are quarantined at the moment
quardf <- isolationquarantine %>% 
  #limit to those which have an entry date 
  filter(quar_entrydate<=Sys.Date()) %>%
  #limit to those who have an exit day after today or none listed
  filter(quar_exitdate>Sys.Date() |is.na(quar_exitdate)) %>%
  distinct(uid) %>%
  left_join(individualdemographics) %>%
  group_by(campus) %>%
  summarize(quarantined=n())


# those who are isolated at the moment
isodf <- isolationquarantine %>% 
  #limit to those which have an entry date 
  filter(iso_entrydate<=Sys.Date()) %>%
  #limit to those who have an exit day after or none listed
  filter(iso_exitdate>Sys.Date()|is.na(iso_exitdate>Sys.Date())) %>%
  distinct(uid) %>%
  left_join(individualdemographics) %>%
  group_by(campus) %>%
  summarize(isolated=n())


# number of people who are being tested (should this be our denominator???)
census <- distinct(individualdemographics,uid,campus,user_status) %>%
  group_by(campus) %>%
  summarize(pop=n()) 


#final table for threshold dataframe
threshdf <- cases10 %>%
  #remove any cases that show up twice with multiple tests
  distinct(uid,campus)%>%
  group_by(campus) %>%
  summarize(cases=n()) %>%
  #include the zeroes
  full_join(census) %>%
  filter(campus %in% c("UNH Durham","UNH LAW",
                       "UNH Manchester"))%>%
  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  #calculate rate per 1000
  mutate(rate=cases/pop*1000)%>%
  #add the number of people quarantined/ isolated
  left_join(quardf) %>%
  mutate(quarantined=ifelse(is.na(quarantined),
                            0,quarantined)) %>%
  left_join(isodf) %>%
  mutate(isolated=ifelse(is.na(isolated),
                         0,isolated)) %>%
  select(-pop)


### Sanity checks  ------------------------------- 
### All to add more
# if(n_isol_sym>n_isol){stop("this is wrong")}



#### CAMPUS PAGE -------------------------------- ####

### Campus epi curve ------------------------------- 
### SOWMYA
# columns: week, var_type, var_level, count
# week should be collection date where available, results date where collection date isn't available
# use CDC default for epi-weeks (Sunday - Saturday) -- think there's an epiweek pkg/function
# A fac_stu fac x
# A fac_stu stu y
# A on_off on   a
# A on_off off  b
# can add other metrics but not currently used in dashboard:
# newly isolated, # newly quarantined, average # in isolation, average # quarantined, total # tested

# merging routine testing and demo table for additional info
routinetesting_w_week_demo <- routinetesting_w_week %>%
  left_join(individualdemographics, by="uid")

# table skeleton for campus curve
# we need to create table skeleton
# to keep rows with 0 cases when there is no information for any combination
# I know it looks like a tedious way to do it but its economic in case we
# get more levels

# check with Kyra and Forrest which campuses to keep
campus <- data.frame(campus= unique(individualdemographics$campus))

# personnel column unique values
personnel <- data.frame(user_status = c("Student","Faculty"))

# campus location column unique values
campus_location <- data.frame(campus_location=c("Off Campus","On Campus"))

# weeks column unique values
weeks <- data.frame(week_no=cont_week)

# for campus location tab; cross join everything to get all required combinations. Then make a level column
table_levels_campus <- merge(campus,campus_location) %>%
  merge(weeks)%>%
  mutate(level=campus_location, id="campus_location")


# TO DO : in dashboard server, change the filters for campus and campus location

# rolling up date level data to required levels
routinetesting_campus_location<- routinetesting_w_week_demo[ ,c("result","week_no","campus","campus_location")] %>%
  group_by(week_no, campus,campus_location) %>%
  ## cases = count of positive results
  summarise(cases = sum(result == "Positive"))%>%
  ##getting all the levels missed by roll up not having any data
  right_join(table_levels_campus) %>%
  ## replacing NA with 0 - later check why we have NAs if present in raw data
  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  mutate(week_start_date=get_date(week_no, start=7))

# for personnel location tab; cross join everything to get all required combinations. Then make a level column
table_levels_user <- merge(campus,personnel) %>%
  merge(weeks)%>%
  mutate(level=user_status, id="user_status")

# rolling up date level data to required levels
routinetesting_campus_personnel<- routinetesting_w_week_demo[ ,c("result","week_no","campus","user_status")] %>%
  group_by(week_no, campus,user_status) %>%
  ## cases = count of positive results
  summarise(cases = sum(result == "Positive"))%>%
  right_join(table_levels_user) %>%
  ## replacing NA with 0 - later check why we have NAs
  mutate(cases=ifelse(is.na(cases),0,cases)) %>%
  mutate(week_start_date=get_date(week_no, start=7))

# row bind the data of different tabs. We can instead have these as 2 datasets also
# final table for epi curve 
routinetesting_demograph_epi_curve <- rbind(routinetesting_campus_location[ ,c("campus","id","level","week_no","week_start_date","cases")],
                                            routinetesting_campus_personnel[ ,c("campus","id","level","week_no","week_start_date","cases")])



### Campus specific numbers: included in thresholds data  ------------------------------- 
### included in thresholds data

### Dorm-specific campus table  ------------------------------- 
### FORREST
# data frame with cumulative counts
#   column campus
#   column dorms (with off campus) -- intersection of (off_campus, campus)
#   column incident cases (new positive tests) in last 10 days
#   column % positive (# incident cases / # with at least one test result (positive/negative) in last 10 days) 
#   # quarantined
#   % all beds occupied
# TO DO: decide if this is best "% positive" metric or want something different



# Currently quarantined from dorms
quardorm <- isolationquarantine %>% 
  #limit to those which have an entry date before
  filter(quar_entrydate<=Sys.Date()) %>%
  #limit to those who have an exit day after
  filter(quar_exitdate>Sys.Date()) %>%
  distinct(uid)%>%
  left_join(individualdemographics)%>%
  group_by(dorm)%>%
  summarize(quarantined=n())

# number of people who are being tested (should this be our denominator???)
censusdorm <- distinct(individualdemographics,uid,dorm,campus) %>%
  group_by(dorm,campus) %>%
  summarize(pop=n())

# dorm table
# TO DO: add row with "Off campus: Students" and "Off campus: Faculty/Staff/Emp"
dormdf <- cases10 %>% group_by(campus, dorm) %>%
  summarize(cases=n()) %>%
  full_join(censusdorm) %>%
  filter(!is.na(dorm)) %>%
  mutate(cases=ifelse(is.na(cases),0,cases))%>%
  mutate(rate=cases/pop*1000) %>%
  arrange(-rate) %>%
  left_join(quardorm) %>%
  mutate(quarantined=ifelse(is.na(quarantined),0,quarantined))



### Testing epi curve ------------------------------- 
### FORREST
# data frame -- DAY LEVEL for plot
# column campus
# column date (day) -- date of results (fill in with date of collection where date of result is not needed)
# column test_type -- factor: negative, positive, etc.
## TO DO -- figure out what all the levels mean -- e.g. is inconclusive "pending"?
## for now keep the levels they've provided until we figure out which ones can be merged together
# column count -- number in each category

tecCampus1 <- routinetesting_w_week %>% left_join(individualdemographics) %>%
  #remove those with missing or free text responses or inconclusive
  filter(!is.na(result)) %>%
  #remove those not on UNH Durham, Manchester or Law
  filter(campus %in% c("UNH Manchester","UNH Durham","UNH LAW")) %>%
  #only include those conducted in the last two weeks
  ## TO DO: I think we should show longer timeframe here
  filter(date > (Sys.Date()-14)) %>%
  filter(date <= Sys.Date()) %>%
  group_by(campus, date,result,week_no) %>%
  summarize(tests=n())


tecCampusfinal <- tecCampus1 %>%
  #add zeroes to days missing for each category
  full_join(expand_grid(campus=unique(tecCampus1$campus),
                        date=seq.Date(min(tecCampus1$date), 
                                      Sys.Date(),
                                      by="days"),
                        result=unique(tecCampus1$result))) %>%
  mutate(tests=ifelse(is.na(tests),0,tests))

### Testing table ------------------------------- 
# data frame -- WEEK LEVEL for table
# summarize the daily table by week to produce columns:
# campus
# week
# n_submitted = sum of all tests collected within that week, regardless of current outcome (include not tested, etc.)
# n_not_submitted = (all tests that should have been collected - all tests that were collected) 
# % positive tests = # all positive tests / (# all positive tests + # all negative tests)
# to figure out what they're doing about pooled samples - does the updated result of a pooled sample go in as a new row (so rows are tests) or are rows just samples
pct_pos <- tecCampusfinal %>%
  group_by(week_no, campus) %>%
  summarise(n_pos = sum(tests[which(result=="Positive")]),
            n_neg = sum(tests[which(result=="Negative")]),
            n_tot = sum(tests),
            pct_pos = n_pos/ ifelse((n_pos + n_neg)==0,1,(n_pos+n_neg)),
            pct_pos_label = paste0(ceiling(pct_pos*100),"%"))

pct_pos_daily <- tecCampusfinal %>%
  group_by(date, campus) %>%
  summarise(n_pos = sum(tests[which(result=="Positive")]),
            n_neg = sum(tests[which(result=="Negative")]),
            n_tot = sum(tests),
            pct_pos = n_pos/ ifelse((n_pos + n_neg)==0,1,(n_pos+n_neg)),
            pct_pos_label = paste0(ceiling(pct_pos*100),"%"))

### Testing delays ------------------------------- 
# we can't really do this yet
# eventually, data frame of 
# campus
# lab
# median days from sample collection to test results returned in last 2 weeks


#### MISC. OBJECTS ------------------------------- ####

# leave notes here if you think of other things that would be helpful!

#### EXPORT ------------------------------------ ####

dir.create(paste0("data/",Sys.Date()))

filename <- paste0("./data/",Sys.Date(),"/processed_data.Rdata")

save(file=filename,
     list = c("updated.date",
              "state.updated.date",
              "epi_curve_overall_week",
              "state_curr_cases",
              "state_curr_hosp",
              "state_curr_cond",
              "routinetesting_demograph_epi_curve",
              "routinetesting_campus_location",
              "routinetesting_campus_personnel",
              "threshdf",
              "dormdf",
              "tecCampusfinal",
              "pct_pos",
              "pct_pos_daily"
     ))