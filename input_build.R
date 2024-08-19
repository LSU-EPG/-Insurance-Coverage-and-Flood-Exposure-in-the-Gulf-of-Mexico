## Lets Start with the covars 

a<-left_join(fema_,covars,by = c("GEOID","Time.Period"))

b<-left_join(a,coverage,by = c("GEOID", "Time.Period"))

c<-left_join(b,medHouseAge2010_2019,by = c("GEOID", "Time.Period"))

d<-left_join(c,sfhaPlusff,by= "GEOID")

sum(is.na(c$Claims))
sum(is.na(c$Income))
sum(is.na(c$Pop.Density))
sum(is.na(c$Black))
sum(is.na(x))

x <- d %>% 
  na.omit(select(Income:Renters))

write.csv(x,"period_inputv6.csv")

y<-e %>% filter(Time.Period == "2010_2014")

z<- e %>% filter(Time.Period == "2015_2019")
