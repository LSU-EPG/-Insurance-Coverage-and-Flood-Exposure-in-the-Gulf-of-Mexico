## Load Packages
library(tidyverse)

### 3 we are bringing in a prepossessed set. 

policies_calc_v1 <- read_csv("policies_calc_v1.csv", 
                             col_types = cols(...1 = col_skip()))
# We selected a boundary from a shapefile and we are using this list of geoids for all filters. 
## This is so we do not have holes

geoid_study <- levels(factor(studyAreaTracts$GEOID))

# filtering the set for a join 
policies<- policies_calc_v1 %>%
  filter(GEOID %in% geoid_study) %>% # Study area 
  filter(Time.Period == "2010_2014" | Time.Period == "2015_2019") %>% # Selecting time periods of interest
  group_by(GEOID, year) %>% #Grouping for calculations 
  summarise(ave_policies = mean(policies)) %>% # We just calculate the average number of polices in the time period 
  mutate(across(where(is.numeric),round,0))

## Bring in Housing Units 

ACS_housingUnits <- read_csv("ACS_housingUnits.csv", 
                             col_types = cols(...1 = col_skip()))

#Filter on study area, create time period, get mean housing units
units<- ACS_housingUnits %>%
  filter(GEOID %in% geoid_study) %>%
  filter(year != 2020)


### Join 

test<-left_join(policies,ACS_housingUnits, by = c("GEOID", "year"))

# Create the time period column

test<-test %>% mutate(Time.Period = case_when((year >= "2010") & (year <= "2014") ~ "2010_2014",
                                              (year >= "2015") & (year <= "2019") ~ "2015_2019"))
# Calculate
Coverage<- test %>%
  group_by(GEOID,year) %>%
  mutate(year.cov = ave_policies/HU) %>%
  group_by(GEOID, Time.Period) %>%
  summarise(Coverage = mean(year.cov))
  
write.csv(Coverage,"coverage.csv")
  