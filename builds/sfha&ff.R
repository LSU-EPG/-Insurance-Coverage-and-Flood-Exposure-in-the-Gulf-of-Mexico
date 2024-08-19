## Flood Factor adn Special Flood hazard Area Build 
## Packages 
library(dplyr)
library(tidyverse)
## Load raw data
GulfstatesFF <- read_csv("C:/Users/ahyde8/OneDrive - Louisiana State University/A.Hyde Thesis/Analysis_3/Analysis 4/Raw Data/GulfstatesFF.csv", 
                         col_types = cols(...1 = col_skip()))

#### Load Study Area

studyAreaTracts <- read_csv("C:/Users/ahyde8/OneDrive - Louisiana State University/A.Hyde Thesis/Analysis_3/Analysis 4/miscellaneous/studyAreaTracts.csv")

## Create Study area function

geoid_study <- levels(factor(studyAreaTracts$GEOID))


## We want to do a few things 
test<- GulfstatesFF %>%
  filter(GEOID %in% geoid_study) %>% #Filter based on shapefile boundary
  mutate(SFHA = rowSums(select(.,AE,AH,A,VE,AO))) %>% #get % sfha by adding zones https://www.fema.gov/glossary/special-flood-hazard-area-sfha
  select(GEOID,SFHA,`sfha comparison`) %>%
  rename("Flood.Factor" = `sfha comparison`) # Select the columns of choice
 
## Save

write.csv(test,"sfhaPlusff.csv")

##############################

e.claim<-e.claim %>%
  mutate(Risk.Diff = SFHA - Flood.Factor)
         