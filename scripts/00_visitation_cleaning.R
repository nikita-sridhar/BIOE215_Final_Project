library(tidyverse)

#11/21: allstartyr has start year that's earliest (what's needed for "both". and if it's just translocationm
#SA is NA for start years, so allstartyr is the correct starting point for either both, trans only, or SA only)
all_data <- read_csv(here::here("./data/raw_data.csv"))
visitation_select_data <- all_data %>%
  select(EventID, Family, Order, SA_Implementation, 
         Trans_Implementation, 
         Visitation_Yr1_SinceOperation,
         Data_Quality, Visitation_PreOperation, Visitation_Operation, method, AllStartYr) %>%
  filter(Visitation_PreOperation == "No", 
         Visitation_Operation == "Yes",
         AllStartYr < 2018,
         Data_Quality == 1) %>% 
  mutate(Family = if_else(Family %in% c("Hydrobatidae", "Oceanitidae"),  "Petrel", Family))
  

bad_fam <- visitation_select_data %>%
  group_by(Family) %>%
  summarise(num_fam = n()) %>%
  filter(num_fam < 5)
  

#selection/filter for JUST social attraction visitation events
visitation_SA <- visitation_select_data %>% 
  filter(SA_Implementation == "Partially Achieved" | SA_Implementation == "Achieved",
         method == "Social_Attraction") %>%
  mutate(plotpanel = "Social_Attraction")

#selection/filter for JUST translocation  events (which will be 
#used for all later)
visitation_trans <- visitation_select_data %>% 
  filter(Trans_Implementation == "Partially Achieved" | Trans_Implementation == "Achieved" ,
         method == "Translocation") %>%
  mutate(plotpanel = "Trans")
  
#selection/filter for BOTH translocation and social attraction visitation events
visitation_both <- visitation_select_data %>% 
  filter(SA_Implementation == "Partially Achieved" | SA_Implementation == "Achieved",
         Trans_Implementation == "Partially Achieved" | Trans_Implementation == "Achieved" ,
         method == "Both") %>%
  mutate(plotpanel = "Trans_and_SA")

#create all_data by rbinding just to have
visitation_all <- rbind(visitation_SA, visitation_trans, visitation_both) %>%
  mutate(plotpanel = "All_Data")

#for our all panel, need to combine the three dataframes we've just made
#need to remove code that removes 5 families from prev code, but apply that post rbind

all_data_clean <- rbind(visitation_SA, visitation_both, visitation_all) %>%
  mutate(yrs_since_start = Visitation_Yr1_SinceOperation - AllStartYr,
         outcome = "visitation") %>%
  anti_join(bad_fam, by = "Family") %>%
  mutate(Family = case_when(Family == "Procellariidae" ~ "Shearwater",
                            Family == "Phalacrocoracidae" ~ "Cormorant",
                            Family == "Laridae" ~ "Gulls & Terns",
                            Family == "Alcidae" ~ "Auks"))


             