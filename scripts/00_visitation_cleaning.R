library(tidyverse)

#11/21: allstartyr has start year that's earliest (what's needed for "both". and if it's just translocationm
#SA is NA for start years, so allstartyr is the correct starting point for either both, trans only, or SA only)
all_data <- read_csv(here::here("./data/Spatz_etal_2023_eventdata_PNAS.csv"))
visitation_select_data <- all_data %>%
  select(Family, Order, SA_Implementation, 
         Trans_Implementation, 
         Visitation_Yr1_SinceOperation,
         Data_Quality, Visitation_PreOperation, Visitation_Operation, method, AllStartYr)

#selection/filter for JUST social attraction visitation events
visitation_SA <- visitation_select_data %>% 
  filter(SA_Implementation == "Partially Achieved" | SA_Implementation == "Achieved",
         Visitation_PreOperation != "Unknown",
         Visitation_Operation != "Unknown",
         Visitation_Operation != "No",
         AllStartYr < 2018,
         Data_Quality == 1,
         method == "Social_Attraction") 

n_fam_visitation_SA <- visitation_SA %>%
  group_by(Family) %>%
  summarise(num_fam = n()) %>%
  filter(num_fam < 5)

visitation_SA <- visitation_SA %>%
  anti_join(n_fam_visitation_SA, by = "Family")


#selection/filter for JUST translocation  events (which will be 
#used for all later)
visitation_trans <- visitation_select_data %>% 
  filter(Trans_Implementation == "Partially Achieved" | Trans_Implementation == "Achieved" ,
         Visitation_PreOperation != "Unknown",
         Visitation_Operation != "Unknown",
         Visitation_Operation != "No",
         AllStartYr < 2018,
         Data_Quality == 1,
         method == "Translocation") 

n_fam_visitation_trans <- visitation_trans %>%
  group_by(Family) %>%
  summarise(num_fam = n()) %>%
  filter(num_fam < 5)

visitation_trans <- visitation_trans %>%
  anti_join(n_fam_visitation_trans, by = "Family")
  

#selection/filter for BOTH translocation and social attraction visitation events
visitation_both <- visitation_select_data %>% 
  filter(SA_Implementation == "Partially Achieved" | SA_Implementation == "Achieved",
         Trans_Implementation == "Partially Achieved" | Trans_Implementation == "Achieved" ,
         Visitation_PreOperation != "Unknown",
         Visitation_Operation != "Unknown",
         Visitation_Operation != "No",
         AllStartYr < 2018,
         Data_Quality == 1,
         method == "Both")

n_fam_visitation_both <- visitation_both %>%
  group_by(Family) %>%
  summarise(num_fam = n()) %>%
  filter(num_fam < 5)

visitation_both <- visitation_both %>%
  anti_join(n_fam_visitation_both, by = "Family")


#for our all panel, need to combine the three dataframes we've just made
#need to remove code that removes 5 families from prev code, but apply that post rbind

all_data_clean <- rbind(visitation_SA, visitation_trans, visitation_both) %>%
  mutate(yrs_since_start = Visitation_Yr1_SinceOperation - AllStartYr)

#plot 
#all data
ggplot(all_data_clean, aes(x=yrs_since_start, y=Family, col=Family)) +
  geom_boxplot(horizontal = TRUE) +
  geom_jitter(alpha = 0.7, width = 0.9) +
  ggtitle("Visiting outcome all data") +
  theme_classic()

             