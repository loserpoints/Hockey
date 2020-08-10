library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(ggforce)


### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


### read shot data

shots <- read_csv("data/shots_1920.csv") %>%
  
  filter(!is.na(pred_goal)) %>%
  
  select(team = event_team, shooter = event_player_1, xG = pred_goal)


shooting <- read_csv("data/skater_data_1720.csv") %>%
  
  mutate(shooting_impact = (G-ixG)/iFF) %>%
  
  select(shooter = EH_ID, shooting_impact)


team_shooting <- 
  
  left_join(shots, shooting) %>%
  
  mutate(adjusted_xG = xG + shooting_impact) %>%
  
  group_by(team) %>%
  
  mutate(total_xG = sum(xG), total_adjusted_xG = sum(adjusted_xG, na.rm = T)) %>%
  
  select(team, xG = total_xG, adjusted_xG = total_adjusted_xG) %>%
  
  ungroup() %>%
  
  distinct() %>%
  
  mutate(gap = adjusted_xG - xG) %>%
  
  arrange(gap) %>%
  
  mutate(team = factor(team, levels = team))


### plot shot distributions

ggplot(team_shooting, aes(x = team)) +
  
  geom_link(aes(y = xG, yend = adjusted_xG, xend = team, color = stat(index), size = stat(index))) +
  
  
  coord_flip() +
  
  ylab("\nExpected Goals") +
  
  xlab("") +
  
  scale_color_gradient(low = "gray81", high = "dodgerblue3") +
  
  theme_ipsum_ps() +
  
  ggtitle("2019-2020 NHL Team Shooter Quality Impact on Expected Goals", 
          subtitle = "Data via Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "none")


ggsave(filename = "viz/shooter_impact_on_team_xG.jpg", height = 21.333, width = 10.666)
