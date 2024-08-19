
library(tidycensus)
library(purrr)

## Building House age 
states<- c("Alabama","Florida","Louisiana","Mississippi","Texas" )
### Load first time period. 2010-2014 = year 2014
v1 <- load_variables(2014, "acs5", cache = TRUE)


## Load census data for first time period 
med_age_house_2010 <- map_dfr(
  states,
  ~ get_acs(
    geography = "tract",
    variables = "B25035_001",
    state = .,
    year = 2014))

colnames(med_age_house_2010)[4]<- "House.Age"

med_age_house_2010<- med_age_house_2010 %>%
  mutate(Time.Period = "2010_2014")%>%
  select(GEOID,Time.Period,House.Age)

## Load second time period.2014-2019 =year 2019
v2 <- load_variables(2019, "acs5", cache = TRUE)

## Load census data for second time period 
med_age_house_2015 <- map_dfr(
  states,
  ~ get_acs(
    geography = "tract",
    variables = "B25035_001",
    state = .,
    year = 2019))
colnames(med_age_house_2015)[4]<- "House.Age"

med_age_house_2015<- med_age_house_2015 %>%
  mutate(Time.Period = "2015_2019")%>%
  select(GEOID,Time.Period,House.Age)

## Now can we out them together

house<- rbind(med_age_house_2010,med_age_house_2015)
 ## Whohoo!

#SAVE IT

write.csv(house,"medHouseAge2010-2019.csv")
