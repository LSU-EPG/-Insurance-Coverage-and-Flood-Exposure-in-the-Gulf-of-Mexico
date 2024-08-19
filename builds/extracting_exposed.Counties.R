#Packages
library(glmmTMB)
library(readr)
library(dplyr)
library(gtsummary)
library(sjPlot)

### Load Global input
period_inputv4 <- read_csv("C:/Users/ahyde8/OneDrive - Louisiana State University/A.Hyde Thesis/Analysis_3/Analysis 4/period_inputv4.csv", 
                           col_types = cols(...1 = col_skip()))

### Run the global models 
g.coverage <-glmmTMB(Coverage ~ scale(Pop.Density) +
                       scale(Income) + 
                       scale(Black) + 
                       scale(Hispanic) + 
                       scale(Renters) +
                       scale(SFHA) +
                       scale(House.Age) +
                       Time.Period +
                       #scale(Coverage)+
                       (1 | Comutting.Zone:County), #random effect
                     data = period_inputv4,
                     family = tweedie(link = "log"))

g.claims<-glmmTMB(Claims ~ scale(Pop.Density) +
                       scale(Income) + 
                       scale(Black) + 
                       scale(Hispanic) + 
                       scale(Renters) +
                       scale(SFHA) +
                       scale(House.Age) +
                       Time.Period +
                       scale(Coverage)+
                       (1 | Comutting.Zone:County), #random effect
                     data = period_inputv4,
                     family = poisson(link = "log"))

#### Extract the top exposed counties
random<-ranef(g.claims)

random<-as.data.frame(random) # turn it into a data frame

randomSelect<-random %>%
  dplyr::select(grp, condval) %>%
  dplyr:: rename("cz_County" = "grp",
                 "estimate" = "condval")%>%
  tidyr::separate(cz_County, c("Comutting.Zone","County"),":") %>%
  dplyr::select(-Comutting.Zone)

####### Join to the global set

period_inputv4<-left_join(period_inputv4,randomSelect,by = "County")

## Create the column that identifies

test<- period_inputv4 %>%
  mutate(exposed = estimate>= 0.48)
#### Lets write this one 

write.csv(test, "period_inputV4.csv")
  