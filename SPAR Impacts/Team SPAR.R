### load required packages

library(tidyverse)
library(rvest)
library(extrafont)
library(hrbrthemes)


### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


### filter team seasons for specfic team

wings_spar <- rbind(skater_spar, goalie_spar) %>%
  
  filter(Season != "12-13") %>%
  
  group_by(Team, Season) %>%
  
  summarize_all(sum) %>%
  
  mutate(Season = paste0("20", substr(Season, 4, 5))) %>%
  
  left_join(., standings %>% select(Team, Season, Points = PTS), by = c("Team", "Season")) %>%
  
  mutate(Replacement_Level = Points - SPAR)


### filter player data for specific team season

wings_1920 <- wings_spar %>%
  
  filter(Team == "DET", Season == 2020) %>%
  
  ungroup() %>%
  
  select(SPAR)


### get team SPAR for vertical line on plot

wings_1920_spar <- wings_1920$SPAR


### plot team SPAR on distribution

ggplot(wings_spar, aes(x = SPAR)) +
  
  geom_density(aes(y = ..count..), fill = "firebrick3", alpha = 0.6, color = "gray36") +
  
  geom_vline(xintercept = wings_1920_spar, color = "gray45", linetype = "dashed", size = 1.5) +
  
  geom_label(x = 61, y = 11, label = "This year's Red Wings are the worst team in the data set by SPAR by far", family = "IBM Plex Sans", size = 5, label.size = NA, label.padding = unit(1, "lines")) +
  
  ylab("") +
  
  xlab("\nStandings Points Above Replacement") +
  
  theme_ipsum_ps() +
  
  ggtitle("Just how bad were the 2019-2020 Red Wings", 
          subtitle = "Using Standings Points Above Replacement (SPAR) from Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5))


ggsave(filename = "viz/wings_distro.jpg", height = 10.666, width = 21.333)
