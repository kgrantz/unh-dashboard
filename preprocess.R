library(tidyverse)

## Load Data (USE THESE R OBJECT NAMES!)
routinetesting<- read_csv("raw_data/routinetesting.csv")
isolationquarantine<-read_csv("raw_data/isolationquarantine.csv")
individualdemographics <- read_csv("raw_data/individualdemographics.csv") 

## Helper strings --------------- 

  # Save day of last data generation
  updated.date <- Sys.Date()

## Dashboard objects ------------
  
  #################
  ### HOME PAGE ###
  #################

  # overall epi curve:
    # week should be collection date where available, results date where collection date isn't available
    # use CDC default for epi-weeks (Sunday - Saturday) -- think there's an epiweek pkg/function
    # metrics = # new positive cases

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
  
  # campus epi curve
    # columns: week, var_type, var_level, count
    # week should be collection date where available, results date where collection date isn't available
    # use CDC default for epi-weeks (Sunday - Saturday) -- think there's an epiweek pkg/function
    # A fac_stu fac x
    # A fac_stu stu y
    # A on_off on   a
    # A on_off off  b
    # can add other metrics but not currently used in dashboard:
        # newly isolated, # newly quarantined, average # in isolation, average # quarantined, total # tested
    
  # campus specific numbers come from the thresholds data
  
  # dorm-specific table for each campus
    #data frame with cumulative counts
    # column campus
    # column dorms (with off campus) -- intersection of (off_campus, campus)
    # column incident cases (new positive tests) in last 14 days
    # column % positive (# incident cases / # with at least one test result (positive/negative) in last 14 weeks) 
    # # quarantined
    # % all beds occupied
    # TO DO: decide if this is best "% positive" metric or want something different
    
  # testing "epi curve" ------------------------FORREST
    # data frame -- DAY LEVEL for plot
    # column campus
    # column date (day) -- date of results (fill in with date of collection where date of result is not needed)
    # column test_type -- factor: negative, positive, etc.
        ## TO DO -- figure out what all the levels mean -- e.g. is inconclusive "pending"?
        ## for now keep the levels they've provided until we figure out which ones can be merged together
    # column count -- number in each category
    
  
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
      