############################################################################
# Packages
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)



policies_raw_v1 <- read_csv("E:/Anissa Policies/femaPolicies_S.csv")

policies_raw<-policies_v2 %>% mutate(GEOID = str_pad(censusTract,11, pad = "0"))
# GEOID extraction 
input <- read_csv("studyAreaTracts.csv", 
                  col_types = cols(...1 = col_skip()))

## We want to filter the policies set, it is very large 
geoid_study <- levels(factor(studyAreaTracts$GEOID))


## Filter by geo study
policies_raw<- policies_raw_v1 %>%
  dplyr::filter(GEOID %in% geoid_study)

# split raw policies by GEOIDs and store in a list
system.time(policies_split<- split(policies_raw, as.factor(policies_raw$GEOID)))

# loop through the list 
## takes 7 min
system.time(
  for (i in 1:length(policies_split)) {
    # extract dataframe from the list
    policies_split[[i]] <- policies_split[[i]] %>% # create true_end column
      dplyr::mutate(true_end = case_when(!is.na(cancellationDateOfFloodPolicy)  ~ cancellationDateOfFloodPolicy,
                                         TRUE ~ policyEffectiveDate)) %>% # expand the grid
      expand(nesting(id, GEOID, policyEffectiveDate, true_end, policyCount), #edits here 
             month = seq.Date(from = ymd("2010-01-01"), 
                              to = ymd("2019-12-31"), 
                              by = "month")) %>% # calculate total policies/month
      dplyr::mutate(inrange = month %within% (policyEffectiveDate %--% true_end)) %>% 
      dplyr::group_by(month,GEOID) %>%
      dplyr::filter(inrange == TRUE)%>%
      dplyr::summarize(policies = sum(policyCount)) %>%
      ungroup()
  })


policies_calc <- bind_rows(policies_split) # bringing the list as a full dataframe

# Creating year column for joining to housing units
policies_calc2 <- policies_calc2 %>%
  mutate(year = case_when((month >="2010-01-01" & month <= "2010-12-01") ~ 2010,
                          (month >="2011-01-01" & month <= "2011-12-01") ~ 2011,
                          (month >="2012-01-01" & month <= "2012-12-01") ~ 2012,
                          (month >="2013-01-01" & month <= "2013-12-01") ~ 2013,
                          (month >="2014-01-01" & month <= "2014-12-01") ~ 2014,
                          (month >="2015-01-01" & month <= "2015-12-01") ~ 2015,
                          (month >="2016-01-01" & month <= "2016-12-01") ~ 2016,
                          (month >="2017-01-01" & month <= "2017-12-01") ~ 2017,
                          (month >="2018-01-01" & month <= "2018-12-01") ~ 2018,
                          (month >="2019-01-01" & month <= "2019-12-01") ~ 2019))

# Summarizing policies in a year

policies<- policies_calc2%>%
  group_by(year, GEOID) %>% 
  summarise(sumpolicies = sum(policies)) 

policies%>%
  group_by(year)%>%
  summarise(total =sum(sumpolicies)) ### How many per year? HUZZAH 

# Creating a time period column 
policies <- policies %>%
  mutate(timePeriod = case_when((year >= "2010") & (year <= "2014") ~ "2010_2014",
                                (year >= "2015") & (year <= "2019") ~ "2015_2019"))


# summarizing by time period 
p<- policies%>%
  group_by(timePeriod, GEOID) %>% 
  summarise(policies = mean(sumpolicies)) 

# Bring in ACS Housing units
ACS_housingUnits <- read_csv("ACS_housingUnits.csv")


# Filtering Sample id
ACS_housingUnits<- ACS_housingUnits %>%
  filter(GEOID %in% geoid_study)

# Creating a time period column 
hunits <- ACS_housingUnits %>%
  mutate(timePeriod = case_when((year >= "2010") & (year <= "2014") ~ "2010_2014",
                                (year >= "2015") & (year <= "2019") ~ "2015_2019"))
# summarizing by timeperiod 
h<- hunits%>%
  group_by(timePeriod, GEOID) %>% 
  summarise(aveUnits = mean(hu)) 


# Prepping data types
p$timePeriod<-as.character(p$timePeriod)
p$GEOID<-as.character(p$GEOID)

h$timePeriod<-as.character(h$timePeriod)
h$GEOID<-as.character(h$GEOID)

# Joining the ACS year
coverage<- left_join(p,h, by = c("GEOID", "timePeriod")) 


# Averaging the time periods, 14% ABOVE 1%
coverage <- coverage %>%
  group_by(timePeriod, GEOID) %>%
  dplyr::mutate(cvg = policies/aveUnits)

### HU