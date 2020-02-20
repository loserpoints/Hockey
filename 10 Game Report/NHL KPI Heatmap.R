### to do
## fix legend

#load dependencies

library(tidyverse)
library(ggthemes)
library(scales)
library(extrafont)


### load fonts for viz

loadfonts(device = "win")

### get data for chart

heatmap <- report_data [3] %>%
  
  data.frame(.)

#Generate team KPI heat map

ggplot(heatmap, aes(x = Team, y = Verbose, fill = Season_Value_Z_Score)) +
  
  geom_tile() +
  
  facet_wrap(
    ~Group, 
    ncol = 1, 
    strip.position = "left", 
    scales = "free_y"
    ) +
  
  theme_few() +
  
  scale_fill_gradient2(
    low = "orange", 
    mid = "gray93", 
    high = "dodgerblue4", 
    limits = c(-3,3), 
    midpoint = 0, 
    labels=c("Worst", "", "", "Avg", "", "", "Best"), 
    oob = squish, na.value = "black"
    ) +
  
  labs(caption = "data via naturalstattrick.com, chart by @loserpoints") +
  
  theme(legend.title=element_blank()) +
  
  theme(axis.title = element_blank()) +
  
  theme(
    strip.placement = "outside", 
    strip.background = element_rect(fill = "gray36"), 
    strip.text = element_text(color = "white", face = "bold", size = 15, family = "Trebuchet MS")
    ) +
  
  scale_x_discrete(position = "top") +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 0.25, face = "bold", size = 14, family = "Trebuchet MS")) +
  
  theme(axis.text.y = element_text(face = "bold", size = 14, family = "Trebuchet MS")) +
  
  theme(plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5), family = "Trebuchet MS"))


ggsave("Viz/NHL_KPI_Heatmap_Totals.png", width = 21.333, height = 10.667)
