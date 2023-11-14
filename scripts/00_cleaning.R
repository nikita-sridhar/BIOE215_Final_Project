library(tidyverse)

all_data <- read_csv(here::here("./data/Spatz_etal_2023_eventdata_PNAS.csv"))

select_data <- all_data %>% 
  select(Family, Order, Social_Attraction, SA_Implementation, 
         Trans_Implementation,Social_Attraction_Start,Translocation, 
         Translocation_Start, Breeding_PreOpCat, Visitation_Yr1_SinceOperation,
         Breeding_Yr1, Data_Quality, Visitation_PreOperation, Visitation_Operation) %>%
  filter(SA_Implementation == c("Partially Achieved","Achieved" ),
         Trans_Implementation == c("Partially Achieved","Achieved" ),
         Visitation_PreOperation != "Unknown",
         Visitation_Operation != "Unknown",
         Breeding_PreOpCat == 0,
         Breeding_PostOpCat != "Unknown",
         Social_Attraction_Start < 2018,
         Translocation_Start < 2018,
         Data_Quality == 1
         )


         
         