tenA <- ten %>% 
  mutate(Built = case_when(
    House.Age >=1950 & House.Age<= 1969 ~ "1950-1969",
    House.Age >=1970 & House.Age<= 1989 ~ "1970-1989",
    House.Age >=1990 ~ "After 89",
    TRUE~ "Before 1950"
    
    
    
  ))
House.Age$Built <- as.factor(House.Age$Built)
levels(House.Age$Built)

House.Age$Built<-relevel(House.Age$Built, ref = "1950-1969")


write.csv(House.Age,"medHouseAge2010-2019.csv")
