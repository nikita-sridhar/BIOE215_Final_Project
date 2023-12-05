library(tidyverse)
library(ggplot2)

all_data <- read_csv(here::here("./data/raw_data.csv"))


#breeding, removing families with less than 5 responses
breeding_families <- all_data %>% 
  select(EventID, Family, Order, SA_Implementation, 
         Trans_Implementation,Social_Attraction_Start,Translocation, 
         Translocation_Start, Breeding_PreOpCat,Breeding_PostOpCat, 
         Breeding_Yr1, Data_Quality, AllStartYr, method) %>%
  filter(Breeding_PostOpCat != "Unknown", Breeding_PostOpCat != 0) %>% 
  filter(Breeding_PreOpCat == 0) %>% 
  filter(AllStartYr < 2018) %>% 
  filter(Data_Quality == 1) 

badfamilies <- breeding_families %>% 
  group_by(Family) %>% 
  summarize(n_obs = n()) %>% 
  filter(n_obs < 5)
#badfamilies is now my list of the families to remove from the plots/final data frames that I make the plots with





#breeding, Social Attraction only
breeding_SA <- all_data %>% 
  select(EventID, Family, Order, Social_Attraction, SA_Implementation, 
         Trans_Implementation,Social_Attraction_Start,Translocation, 
         Translocation_Start, Breeding_PreOpCat,Breeding_PostOpCat, 
         Visitation_Yr1_SinceOperation,
         Breeding_Yr1, Data_Quality, Visitation_PreOperation, Visitation_Operation, method) %>%
  filter(method == "Social_Attraction") %>% 
  filter(Data_Quality == 1) %>%
  filter(SA_Implementation == "Partially Achieved" | SA_Implementation == "Achieved") %>% 
  filter(Social_Attraction_Start < 2018) %>% 
  filter(Breeding_PostOpCat != "Unknown", Breeding_PostOpCat != 0) %>% 
  filter(Breeding_PreOpCat == 0)
  
#breeding, translocation only
breeding_trans <- all_data %>% 
  select(EventID, Family, Order, Social_Attraction, SA_Implementation, 
         Trans_Implementation,Social_Attraction_Start,Translocation, 
         Translocation_Start, Breeding_PreOpCat,Breeding_PostOpCat, 
         Visitation_Yr1_SinceOperation,
         Breeding_Yr1, Data_Quality, Visitation_PreOperation, Visitation_Operation, method) %>%
  filter(method == "Translocation") %>% 
  filter(Data_Quality == 1) %>%
  filter(Trans_Implementation == "Partially Achieved" | Trans_Implementation == "Achieved") %>% 
  filter(Translocation_Start < 2018) %>% 
  filter(Breeding_PostOpCat != "Unknown", Breeding_PostOpCat != 0) %>% 
  filter(Breeding_PreOpCat == 0)

#breeding, SA and trans combined
breeding_SATrans <- all_data %>%
  select(EventID, Family, Order, Social_Attraction, SA_Implementation, 
         Trans_Implementation,Social_Attraction_Start,Translocation, 
         Translocation_Start, Breeding_PreOpCat,Breeding_PostOpCat, 
         Visitation_Yr1_SinceOperation,
         Breeding_Yr1, Data_Quality, Visitation_PreOperation, Visitation_Operation, method) %>%
  filter(method == "Both") %>% 
  filter(Data_Quality == 1) %>%
  filter(Breeding_PostOpCat != "Unknown", Breeding_PostOpCat != 0) %>% 
  filter(Breeding_PreOpCat == 0) %>% 
  filter(Trans_Implementation %in% c("Partially Achieved", "Achieved")) %>% 
  filter(SA_Implementation %in% c("Partially Achieved", "Achieved")) %>% 
  filter(Translocation_Start < 2018 | Social_Attraction_Start < 2018)


#get the chunk that will make the SA panel 
chunkSA <- breeding_SA %>% 
  mutate(years_post = Breeding_Yr1 - Social_Attraction_Start) %>% 
  mutate(plotpanel = "Social_Attraction") %>% 
  select(EventID, years_post, Family, Order, plotpanel)
  
#get the chunk that will make the trans and SA panel
chunkTandSA <- breeding_SATrans %>% 
  mutate(years_post = Breeding_Yr1 - pmin(Translocation_Start, Social_Attraction_Start)) %>% 
  mutate(plotpanel = "Trans_and_SA") %>% 
  select(EventID, years_post, Family, Order, plotpanel)
  
#get the chunk that will make the All Data panel
#first make a  chunk with the trans only
tempchunk <- breeding_trans %>% 
  mutate(years_post = Breeding_Yr1 - Translocation_Start) %>% 
  select(EventID, years_post, Family, Order)
#now combine the three and overwrite plot panel to "all data"
chunkAllData <- bind_rows(chunkSA, chunkTandSA, tempchunk) %>% 
  mutate(plotpanel = "All_Data")

  

#now join the three chunks
plotdataframe <- bind_rows(chunkAllData, chunkSA, chunkTandSA) %>% 
  mutate(Common_Name = case_when(Family == "Procellariidae" ~ "Shearwaters",
                            Family == "Phalacrocoracidae" ~ "Cormorants",
                            Family == "Laridae" ~ "Gulls & Terns",
                            Family == "Alcidae" ~ "Auks",
                            Family == "Diomedeidae" ~ "Albatrosses"))

#looks good, each EventID is in there twice (once for all data, once for its own thing)


#anti-join to remove bad families
plotdataframe <- plotdataframe %>% 
  anti_join(badfamilies, by = "Family")















