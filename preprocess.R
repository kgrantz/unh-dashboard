
#### Environment + Data Load ----------------------- ####

require(tidyverse)
require(aweek)
require(rvest)

updated.date <- Sys.Date()

routinetesting<- read_csv("raw_data/routinetesting.csv") %>%
  mutate(date=ifelse(is.na(collectdate),
                     as.character(resultsdate),
                     as.character(collectdate))) %>%
  mutate(date=as.Date(date))%>%
  # recode the results
  mutate(result_original = result,
         result = tolower(result)) %>%
  mutate(result_num=recode(result,
                           "negative" = 0,
                           "positive" = 1,
                           "test was positive provided letter to be on campus" = 1,
                           "inconclusive" = 2,
                           .default = 3)) %>%
  mutate(result = factor(result_num,
                         levels = 0:3,
                         labels = c("Negative", "Positive", "Inconclusive", "Invalid / Rejected / Not Performed")))

# Imputting missing quarantine/isolation dates
isolationquarantine <- read_csv("raw_data/isolationquarantine.csv") %>%
                       mutate(quar_entrydttm = as.POSIXct(quar_entrydate),
                              quar_exitdttm = as.POSIXct(quar_exitdate),
                              quar_exposuredttm = as.POSIXct(quar_exposuredate),
                              iso_entrydttm = as.POSIXct(iso_entrydate),
                              iso_exitdttm = as.POSIXct(iso_exitdate),
                              quar_entrydate = as.Date(quar_entrydate),
                              quar_exitdate = as.Date(quar_exitdate),
                              quar_exposuredate = as.Date(quar_exposuredate),
                              iso_entrydate = as.Date(iso_entrydate),
                              iso_exitdate = as.Date(iso_exitdate),
                              notifydttm = as.POSIXct(notifydate),
                              notifydate = as.Date(notifydate))

isolationquarantine <- isolationquarantine %>%
                       mutate(quar_exitdate = ifelse(is.na(quar_exitdate) & !is.na(quar_entrydate),
                                                     quar_entrydate + 14,
                                                     quar_exitdate)) %>%
                       mutate(iso_exitdate = ifelse(is.na(iso_exitdate) & !is.na(iso_entrydate),
                                                     iso_entrydate + 14,
                                                     iso_exitdate)) %>%
                       mutate(quar_exitdate = as.Date(quar_exitdate, origin="1970-01-01")) %>%
                       mutate(iso_exitdate = as.Date(iso_exitdate, origin="1970-01-01"))
  

individualdemographics <- read_csv("raw_data/individualdemographics.csv") %>%
  mutate(user_status_comb = recode(user_status,
                                   Employee="Faculty/Staff",
                                   Staff="Faculty/Staff",
                                   Faculty="Faculty/Staff",
                                   Unknown="Faculty/Staff")) %>%
  mutate(user_status=recode(user_status,
                            Employee="Employee / Staff",
                            Staff="Employee / Staff",
                            Unknown="Employee / Staff" ## per call 9/24, these are mostly contractors
  )) %>%
  ## defining column campus location as its not explicitly defined
  mutate(campus_location = ifelse(is.na(dorm),"Off Campus","On Campus")) %>%
  ## redefining campus labels. clunky, but flexible in case new campus labels get added
  mutate(campus_orig = toupper(campus)) %>%
  mutate(campus = recode(campus_orig,
                         "UNH DURHAM" = "UNH DURHAM",
                         "UNH LAW" = "UNH LAW",
                         "UNH MANCHESTER" = "UNH MANCHESTER",
                         .default = "OTHER"))

## remove demographic duplicates if they still exist
## favor entries that have complete campus, age, user_status where available
## logic: add number of missing values (of campus, age, user_status)
## arrange data so that NAs are last, then fill in NAs
## keep the row that had the fewest elements filled in
individualdemographics <- individualdemographics %>%
  group_by(uid) %>%
  mutate(n_missing = sum(is.na(c(user_status, sex, age, campus)), na.rm=TRUE)) %>%
  arrange(user_status) %>%
  fill(user_status) %>%
  arrange(sex) %>%
  fill(sex) %>%
  arrange(age) %>%
  fill(age) %>%
  arrange(campus) %>%
  fill(campus) %>%
  arrange(n_missing) %>%
  mutate(n = 1:n()) %>%
  ungroup() %>%
  filter(n==1)

#### HOME PAGE ---------------------------------- ####

### Overall epi curve ------------------------------- 

# adding week number and week start date for date
routinetesting_w_week <- routinetesting %>%
  mutate(week_no = lubridate::epiweek(date),
         year = lubridate::year(date))

# getting a continuous string of weeks
# from min week available
# to max week available
# to include rows with 0 where there are no week values
cont_day<- data.frame(date = seq.Date(as.Date("2020-08-02"),Sys.Date(),by="days"))

cont_week <- cont_day %>% 
             mutate(week_no = lubridate::epiweek(date),
                    year = lubridate::year(date)) %>%
             arrange(date) %>%
             group_by(week_no, year) %>%
             slice(1) %>%
             rename(week_start_date = date) %>%
             # removing 2021-01-01 -- nots start of epiweek
             filter(week_start_date != "2021-01-01")

# rolling up to week level
epi_curve_overall_week <- routinetesting_w_week %>%
  group_by(week_no, year) %>%
  ## cases = count of positive results
  summarise(cases = sum(result == "Positive", na.rm=TRUE)) %>%
  right_join(cont_week) %>%
  ## replacing NA with 0 - later check why we have NAs
  mutate(cases=ifelse(is.na(cases),0,cases))

# rolling up to daily level
epi_curve_overall_day <- routinetesting %>%
    group_by(date) %>%
    summarise(cases = sum(result == "Positive", na.rm=TRUE)) %>%
    right_join(cont_day) %>%
    mutate(cases=ifelse(is.na(cases),0,cases))
  




### Statewide conditions ------------------------------- 

# get state conditions website
statecondurl <- "https://www.nh.gov/covid19/index.htm"
statecondwebsite <- read_html(statecondurl)

# get state current cases and current hospitalizations
covid_num <- statecondwebsite %>%
  html_nodes("td") %>%
  html_text()

covid_num[4] <- as.numeric(gsub(",", "", covid_num[4]))
covid_num[5] <- as.numeric(gsub(",", "", covid_num[5]))


state_curr_cases <- covid_num[4] 
state_curr_hosp <- covid_num[5]
state_curr_cond <- "Limited Open"

statedatetimeupdated <- statecondwebsite %>%
  html_nodes("h4") %>%
  html_text()

# should be either "None", for no restrictions
#                  "Open", for restrictions on mass gatherings, otherwise open
#                  "Limited Open", for many restrictions but not shut-down
#                  "Limited", for stay-at-home order
# TO DO: these are poorly defined categories, should come up with something more precise
# TO DO: determine if we are keeping "state_curr_cond" in dashboard


### Thresholds data frame ------------------------------- 
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

# those who are quarantined at the moment
quardf <- isolationquarantine %>% 
  #limit to those which have an entry date 
  filter(quar_entrydate<=Sys.Date()) %>%
  #limit to those who have an exit day after today or none listed
  filter(quar_exitdate>Sys.Date()) %>%
  #keep one entry per UID, favor 'campus' location if exists
  group_by(uid) %>%
  arrange(location) %>%
  summarize(location = location[1]) %>%
  left_join(individualdemographics) %>%
  group_by(campus) %>%
  summarize(quarantined=n(),
            quarantined_in_bed = sum(location=="Campus", na.rm=TRUE))


# those who are isolated at the moment
isodf <- isolationquarantine %>% 
  #limit to those which have an entry date 
  filter(iso_entrydate<=Sys.Date()) %>%
  #limit to those who have an exit day after
  filter(iso_exitdate>Sys.Date()) %>%
  #keep one entry per UID, favor 'campus' location if exists
  group_by(uid) %>%
  arrange(location) %>%
  summarize(location = location[1]) %>%
  left_join(individualdemographics) %>%
  group_by(campus) %>%
  summarize(isolated=n(),
            isolated_in_bed = sum(location=="Campus", na.rm=TRUE))


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
  filter(!is.na(campus))%>%
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

threshdf <- threshdf[order(threshdf$campus), ]

#### CAMPUS PAGE -------------------------------- ####

### Campus epi curve ------------------------------- 
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
# for campus location tab; cross join everything to get all required combinations. Then make a level column
table_levels_campus <- expand_grid(
  campus = unique(individualdemographics$campus),
  campus_location=c("Off Campus","On Campus"),
  cont_week
 ) %>%
  mutate(level=campus_location, id="campus_location")

# rolling up date level data to required levels
routinetesting_campus_location<- routinetesting_w_week_demo %>%
  group_by(week_no, year, campus, campus_location) %>%
  ## cases = count of positive results
  summarise(cases = sum(result == "Positive", na.rm=TRUE))%>%
  ##getting all the levels missed by roll up not having any data
  right_join(table_levels_campus) %>%
  ## replacing NA with 0 - later check why we have NAs if present in raw data
  mutate(cases=ifelse(is.na(cases),0,cases))

# for personnel location tab; cross join everything to get all required combinations. Then make a level column
table_levels_user <- expand_grid(
  campus = unique(individualdemographics$campus),
  user_status_comb=c("Student","Faculty/Staff"),
  cont_week
  ) %>%
  mutate(level=user_status_comb, id="user_status")

# rolling up date level data to required levels
routinetesting_campus_personnel <- routinetesting_w_week_demo %>%
  group_by(week_no, year, campus, user_status_comb) %>%
  ## cases = count of positive results
  summarise(cases = sum(result == "Positive", na.rm=TRUE))%>%
  right_join(table_levels_user) %>%
  ## replacing NA with 0 - later check why we have NAs
  mutate(cases=ifelse(is.na(cases),0,cases))

# row bind the data of different tabs. We can instead have these as 2 datasets also
# final table for epi curve 
routinetesting_demograph_epi_curve <- rbind(routinetesting_campus_location[ ,c("campus","id","level","week_no", "year", "week_start_date","cases")],
                                            routinetesting_campus_personnel[ ,c("campus","id","level","week_no", "year", "week_start_date","cases")])



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
  mutate(quarantined=ifelse(is.na(quarantined),0,quarantined)) %>%
  arrange(desc(cases), desc(quarantined)) %>%
  mutate(quar_label=as.character(quarantined)) %>%
  mutate(quar_label=ifelse(quarantined>0 & quarantined<5, "<5", quar_label)) %>%
  mutate(cases_label=as.character(cases)) %>%
  mutate(cases_label=ifelse(cases>0 & cases<5, "<5", cases_label)) %>%
  select(-cases, -quarantined) %>%
  rename(cases = cases_label,
         quarantined = quar_label)
  


### Testing epi curve ------------------------------- 
### FORREST
# data frame -- DAY LEVEL for plot
# column campus
# column date (day) -- date of results (fill in with date of collection where date of result is not needed)
# column test_type -- factor: negative, positive, etc.
## TO DO -- figure out what all the levels mean -- e.g. is inconclusive "pending"?
## for now keep the levels they've provided until we figure out which ones can be merged together
# column count -- number in each category

tecCampus1 <- routinetesting_w_week %>% 
  left_join(individualdemographics) %>%
  #remove those with missing or free text responses or inconclusive
  filter(!is.na(result)) %>%
  #remove those not on UNH Durham, Manchester or Law
  filter(!is.na(campus)) %>%
  #only include those conducted in the last two weeks
  ## TO DO: I think we should show longer timeframe here
  filter(date > (Sys.Date()-21)) %>%
  filter(date <= Sys.Date()) %>%
  group_by(campus, date,result) %>%
  summarize(tests=n())

tecCampus_levels <- routinetesting_w_week %>% 
  left_join(individualdemographics) %>%
  filter(!is.na(result)) %>%
  filter(!is.na(campus)) %>%
  filter(date > (Sys.Date()-21)) %>%
  filter(date <= Sys.Date()) %>%
  group_by(campus, date, user_status_comb, result) %>%
  summarize(tests=n()) %>%
  rename(level = user_status_comb)

tecCampusfinal <- tecCampus1 %>%
  mutate(level = "Total") %>%
  bind_rows(tecCampus_levels) %>%
  ungroup() %>%
  tidyr::complete(campus, date, level, result) %>%
  mutate(tests=ifelse(is.na(tests),0,tests)) 
  # NOTE: this is filtered below to only include last 14 days for plot
  # but contains date on last 21 here in order to calculate 7-day average % pos

### Testing table ------------------------------- 
# data frame -- DAY LEVEL for display under graph, WEEK LEVEL for 2-week table
# campus
# week
# level (student/faculty-staff)
# n_submitted = sum of all tests collected within that week, regardless of current outcome (include not tested, etc.)
# n_not_submitted = (all tests that should have been collected - all tests that were collected) 
# % positive tests = # all positive tests / (# all positive tests + # all negative tests)
# to figure out what they're doing about pooled samples - does the updated result of a pooled sample go in as a new row (so rows are tests) or are rows just samples

# take rolling 7 day windows
rollsum <- tibbletime::rollify(sum, window = 7)

# Daily table for graph, with running 7-day % pos average
pct_pos_daily <- tecCampusfinal %>%
  group_by(campus, level, result) %>%
  arrange(date) %>%
  mutate(sum7 = rollsum(tests)) %>%
  ungroup() %>%
  filter(date > (Sys.Date()-14)) %>%
  group_by(date, campus, level) %>%
  summarise(n_pos_day = sum(tests[which(result=="Positive")]),
            n_neg_day = sum(tests[which(result=="Negative")]),
            n_tot_day = sum(tests),
            pct_pos_day = n_pos_day/ ifelse((n_pos_day + n_neg_day)==0,1,(n_pos_day+n_neg_day)),
            pct_pos_day_label = paste0(ceiling(pct_pos_day*100),"%"),
            n_pos_wk = sum(sum7[which(result=="Positive")]),
            n_neg_wk = sum(sum7[which(result=="Negative")]),
            pct_pos_wk = n_pos_wk/ ifelse((n_pos_wk + n_neg_wk)==0,1,(n_pos_wk+n_neg_wk)),
            pct_pos_wk_label = paste0(ceiling(pct_pos_wk*100),"%"))

# Week table for overall display 
# show # tests submitted, # people submitted, # tests per person, # (%) tests with valid results to dashboard
lab_weekly_table_levels <- routinetesting_w_week %>% 
                           left_join(individualdemographics) %>%
                           filter(!is.na(result)) %>%
                           filter(!is.na(campus)) %>%
                           filter(date > (Sys.Date()-14)) %>%
                           filter(date <= Sys.Date()) %>%
                           group_by(campus, user_status) %>%
                           summarize(n_test=n(),
                                     n_ppl_tested=n_distinct(uid),
                                     n_valid_res=sum(result=="Positive" | result=="Negative")) %>%
                           rename(level = user_status)

lab_weekly_table <- routinetesting_w_week %>% 
                     left_join(individualdemographics) %>%
                     filter(!is.na(result)) %>%
                     filter(!is.na(campus)) %>%
                     filter(date > (Sys.Date()-14)) %>%
                     filter(date <= Sys.Date()) %>%
                     group_by(campus) %>%
                     summarize(n_test=n(),
                               n_ppl_tested=n_distinct(uid),
                               n_valid_res=sum(result=="Positive" | result=="Negative")) %>%
                     mutate(level = "Total") %>%
                     bind_rows(lab_weekly_table_levels) %>%
                     mutate(n_test_per_pers = round(n_test/n_ppl_tested,2))

  ## filter tec object to be just last 14 days
  tecCampusfinal <- tecCampusfinal %>%
                    filter(date > (Sys.Date()-14))
    
#### MISC. OBJECTS ------------------------------- ####

# leave notes here if you think of other things that would be helpful!

#### EXPORT ------------------------------------ ####

dir.create(paste0("data/",Sys.Date()))

filename <- paste0("./data/",Sys.Date(),"/processed_data.Rdata")

save(file=filename,
     list = c("updated.date",
              "statedatetimeupdated",
              "epi_curve_overall_week",
              "epi_curve_overall_day",
              "state_curr_cases",
              "state_curr_hosp",
              "state_curr_cond",
              "statedatetimeupdated",
              "routinetesting_demograph_epi_curve",
              "routinetesting_campus_location",
              "routinetesting_campus_personnel",
              "threshdf",
              "dormdf",
              "tecCampusfinal",
              "pct_pos_daily",
              "lab_weekly_table"
     ))
