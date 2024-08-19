### We need to contruct a table 
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)

# We are going to start with the Census data 
covariates<- read_csv("covariatesData_v0_03.csv")
# How many tracts are here?

print(length(unique(covariates$GEOID)))

#73082

covariates_v2<-covariates %>%
  filter(year != '2000') %>%
  mutate(Time.Period = case_when(year >= 2010 & year<= 2014 ~ "2010_2014",
                                 year>=2015 & year <= 2019 ~ "2015_2019"),
         Land.Area = area/2.59e+6,
         PopDensity = pop/Land.Area) 

covariates_v3 <- covariates_v2 %>%
  group_by(GEOID,Time.Period) %>%
  summarise(Income = mean(hinc),
            Black = mean(pnhblk),
            Hispanic = mean(phisp),
            Pop.Density = mean(PopDensity),
            Renters = mean(1-pown))
  
#### We need to create the population density variable 
covariates_v4<- covariates_v3 %>%
  mutate(Density = case_when(Pop.Density <= 1000 ~ "Very Low Density(Rural)",
                                 Pop.Density >= 1000 & Pop.Density <= 2999 ~ "Low Density(Suburban)",
                                 Pop.Density >= 3000 & Pop.Density <= 4999 ~ "Moderate Density(Suburban)",
                                .default = "High Denisty(Urban)"))
#################################################################################################                                 
tenA<- tenA %>%
  mutate(Density = case_when(Pop.Density <= 1000 ~ "Very Low Density(Rural)",
                             Pop.Density >= 1000 & Pop.Density <= 2999 ~ "Low Density(Suburban)",
                             .default = "Moderate and High Density(Suburban and Urban)"))
                            
#### Cleaning 

write.csv(covariates_v4,"covars.csv")


#####