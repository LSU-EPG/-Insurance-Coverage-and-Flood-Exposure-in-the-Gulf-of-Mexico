library(tidyverse)
library(tidycensus)

ACSlist <- load_variables(2010,"acs5")

#API Key
census_api_key("b6844db29933c9dce9e13fa37f1d015281001b95")

#Define Years

years <- lst(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) 

# Define States

my_states<- c("AL","FL","LA","TX","MS")


# Define Variables
my_vars <- c(
  pop = "B01003_001",
  hinc = "B19013_001",
  whtnh = "B03002_003",
  blknh = "B03002_004",
  hisp = "B03002_012",
  ohu = "B25003_001",
  own = "B25003_002"
)

# loop over list of years and get 1 year acs estimates
multi_year_data_raw <- map_dfr(
  years,
  ~ get_acs(
    geography = "tract",
    variables = my_vars,
    state = my_states,
    year = .x,
    survey = "acs5",
    geometry = FALSE
  ),
  .id = "year"  # when combining results, add id var (name of list item)
) 

multi_year_data <- multi_year_data_raw %>%
  select(-moe,-NAME) %>%  # shhhh
  group_by(year,GEOID) %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  mutate(pnhwht = whtnh/pop,
         pnhblk = blknh/pop,
         phisp = hisp/pop,
         pown = own/ohu) %>%
  select(-c(whtnh,blknh,hisp,own,ohu))