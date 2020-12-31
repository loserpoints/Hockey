###### load required packages

library(tidyverse)
library(extrafont)
library(hrbrthemes)
library(scales)


###### set working directory

setwd("C:/Users/Alan/Documents/R/Projects/Hockey/Tanking")

###### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


###### load data from local

skaters <- read_csv("data/skaters.csv")
team_stats <- read_csv("data/team_stats.csv")
team_standings <- read_csv("data/team_standings.csv")
draft_table <- read_csv("data/draft_table.csv")


###### get team toi totals for merging

team_toi <- team_stats %>%
  
  select(team = Team, season = Season, team_toi = TOI)


###### get team points percentage for merging

team_points <- team_standings %>%
  
  select(team = Team, season = Season, team_points_per = "Points%")


###### get players per draft for merging

players_selected <- draft_table %>%
  
  select(draft_year = year, players_selected)


###### get draft pick totals by team

draft_totals <- skaters %>%
  
  select(team = Team, season = Season, draft_year = "Draft Yr", pick_number = "Draft Ov", toi = TOI) %>%
  
  left_join(., team_toi, by = c("team", "season")) %>%
  
  left_join(., players_selected, by = c("draft_year")) %>%
  
  mutate(pick_score = ifelse(is.na(pick_number), 1, abs(players_selected - pick_number +1)),
         pick_weight = pick_score*toi/team_toi) %>%
  
  select(team, season, pick_weight) %>%
  
  group_by(team, season) %>%
  
  summarize_all(sum) %>%
  
  ungroup() %>%
  
  mutate(pick_index = rescale(pick_weight, to = c(1, 100))) %>%
  
  left_join(., team_points, by = c("team", "season"))


### get pearson's r for relationship between variables

cor_label <- cor(draft_totals$pick_weight, draft_totals$team_points_per)


ggplot(draft_totals, aes(pick_weight, team_points_per)) +
  
  geom_point(size = 4, color = "dodgerblue3") +
  
  geom_smooth(method = "lm", color = "darkorange2", size = 1.5) +
  
  annotate("text", x = 1000, y = 78, label = paste0("r = ", round(cor_label, 2)), family = "IBM Plex Sans", size = 6, ) +
  
  xlab("\nTOI Weighted by Draft Slot") +
  
  ylab("Team Points Percentage\n") +
  
  theme_ipsum_ps() +
  
  ggtitle(
    paste0("NHL Team Performance and Skater Ice Time Weighted by Draft Slot Since 07-08"),
    subtitle = "") +
  
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 18, hjust = 0.5),
    axis.title.y = element_text(size = 18, hjust = 0.5),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey90"),
    legend.position = "none"
  )


ggsave("viz/results_by_pick_weighted_toi.png", width = 21.333, height = 10.666)
