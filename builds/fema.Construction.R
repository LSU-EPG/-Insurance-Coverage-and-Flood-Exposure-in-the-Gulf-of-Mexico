######################################################################
##A.Hyde 4/4/2024
## Building yearly events set + census + risk + policies set
######################################################################

# pad 
ogCovars_plusHyde<- ogCovars_plusHyde %>% mutate(GEOID = str_pad(GEOID,11, pad = "0"))
#Filter years, do one by one. For some reason I could not do c(). weird
ogCovars_plusHyde<- ogCovars_plusHyde %>% filter(year!="2021")
#Save Please
write.csv(ogCovars_plusHyde,"gulf_2010covariates.csv")
########
# Bring in clean sets

library(readr)
gulf_2010covariates <- read_csv("gulf_2010covariates.csv", 
                                col_types = cols(...1 = col_skip()))
library(readr)
gulf_2010covariates <- read_csv("gulf_2010covariates.csv", 
                                col_types = cols(...1 = col_skip()))
# Turn the year data type to year
gulf_2010covariates$year<-as.character(gulf_2010covariates$year)
gulf_coverage$year<-as.character(gulf_coverage$year)

## Join

input<- left_join(gulf_coverage,gulf_2010covariates, by = c("GEOID", "year")) 

write.csv(input,"yearly_2010_input.csv")
################

# Bring in fematable

femaTable4$GEOID<-femaTable4$tractid

femaTable<-femaTable4 %>% select(GEOID,fipsfive,year_month,declarations,claims,cz_name)

# Creating year column for joining to housing units
femaTable <- femaTable%>%
  mutate(year = case_when((year_month >="2010-01-01" & year_month <= "2010-12-01") ~ 2010,
                          (year_month >="2011-01-01" & year_month <= "2011-12-01") ~ 2011,
                          (year_month >="2012-01-01" & year_month <= "2012-12-01") ~ 2012,
                          (year_month >="2013-01-01" & year_month <= "2013-12-01") ~ 2013,
                          (year_month >="2014-01-01" & year_month <= "2014-12-01") ~ 2014,
                          (year_month >="2015-01-01" & year_month <= "2015-12-01") ~ 2015,
                          (year_month >="2016-01-01" & year_month <= "2016-12-01") ~ 2016,
                          (year_month >="2017-01-01" & year_month <= "2017-12-01") ~ 2017,
                          (year_month >="2018-01-01" & year_month <= "2018-12-01") ~ 2018,
                          (year_month >="2019-01-01" & year_month <= "2019-12-01") ~ 2019))

femaTable_v2<-femaTable %>%
  mutate(Time.Period = case_when(year >= 2010 & year<= 2014 ~ "2010_2014",
                                 year>=2015 & year <= 2019 ~ "2015_2019"))

femaTable_v2<- femaTable_v2 %>% mutate(floodevent = case_when(claims >= 25 ~ "1",
                                           .default = "0"))

femaTable_3 <- femaTable_v2 %>%
  group_by(Time.Period, GEOID)%>%
  mutate(timeperiod_claims = sum(claims),
         timeperiod.events = sum(floodevent)) %>%
  select(GEOID,Time.Period,fipsfive,timeperiod_claims, timeperiod.events,cz_name) %>%
  distinct()
           

geoid_study <- levels(factor(studyAreaTracts$GEOID))
write.csv(test,"fema_.csv")
### Filter raw policies 
test<- femaTable_3 %>%
  filter(GEOID %in% geoid_study)

write.
######################################################
# the column names do not match
colnames(yearly_2010_input)[1]<-"year_month"
# Join
input<- left_join(yearly_2010_input,femaTable_2, by = c("GEOID", "year_month","year")) 

# Adding labels
colnames(input)[17] <- "Pop.Density"
