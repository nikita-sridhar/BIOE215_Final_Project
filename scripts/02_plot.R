
library(cowplot)

#visitation plot

visitation_plot <- ggplot(all_data_clean, aes(x=yrs_since_start, y=Family, col=Family)) +
  geom_boxplot(horizontal = TRUE) +
  facet_wrap(~plotpanel, scales="free") +
  geom_jitter(alpha = 0.7, width = 0.9) +
  ggtitle("Visiting Outcome") +
  theme_classic()+
  labs (y = "", x = "")+
  theme(legend.position = "none") +
  theme(strip.background = element_blank())

visitation_plot
# breeding plot

breeding_plot <- ggplot(plotdataframe, aes(x=years_post, y=Common_Name, col=Common_Name)) +
  geom_boxplot(horizontal = TRUE) +
  facet_wrap(~plotpanel, scales = "free") +
  geom_jitter(alpha = 0.7, width = 0.9) +
  ggtitle("Breeding Outcome") +
  theme_classic()+
  labs(y="", x="Years after implementation")+
  theme(strip.text = element_blank())+
  theme(legend.position = "none")+
  theme(strip.background = element_blank())

output_plot <- plot_grid(visitation_plot, breeding_plot, ncol = 1, labels = c("A", "B"))
