### load required packages

library(tidyverse)
library(ggthemes)
library(extrafont)


### load fonts for viz

loadfonts(device = "win")


### query local mysql db for shot data

shots_db <-
  
  dbConnect(
    MariaDB(),
    user = "root",
    password = password,
    dbname = "nhl_shots_eh",
    host = "localhost"
  )


shots_query <- "SELECT * FROM shots"

shots_table <- dbSendQuery(shots_db, shots_query)


### calculate adjusted xG and goal values for each shot

goal_danger <- dbFetch(shots_table) %>%
  
  mutate(goalie = ifelse(event_team == home_team, away_goalie, home_goalie)) %>%
  
  select(goalie, season, event_type, event_team, home_team, away_team, pred_goal, goal_home_weight, goal_away_weight, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  filter(!is.na(pred_goal), !is.na(goalie), !is.na(goal_home_weight)) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         goal = ifelse(event_team == home_team, goal*goal_home_weight, goal*goal_away_weight),
         pred_goal = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight)) %>%
  
  select(goalie, season, pred_goal, goal)


### choose goalie to plot

select_goalie <- "ANDREI.VASILEVSKIY"

gsax <- goal_danger %>%
  
  filter(goalie == select_goalie)


### arrange full career data for plotting

gsax_career_total <- gsax %>%
  
  mutate(gsax = pred_goal - goal) %>%
  
  arrange(goalie, pred_goal) %>%
  
  group_by(goalie) %>%
  
  mutate(total_gsax = cumsum(gsax)) %>%
  
  ungroup()


### arrange season by season data for plotting

gsax_career_by_season <- gsax %>%
  
  mutate(gsax = pred_goal - goal) %>%
  
  arrange(goalie, pred_goal) %>%
  
  group_by(goalie, season) %>%
  
  mutate(total_gsax = cumsum(gsax)) %>%
  
  ungroup()


### plot individual goalie career gsax by shot danger

ggplot(gsax_career_total, aes(pred_goal, total_gsax)) +
  
  geom_line(color ="dodgerblue3", size = 1) +
  
  geom_point(color = "darkorange2", size = 1.5) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray81") +
  
  theme_few() +
  
  xlab("\nExpected shooting percentage") +
  
  ylab("Cumulative goals saved above expected") +
  
  ggtitle("Career Cumulative Goals Saved Above Expected by Shot Danger", subtitle = "All data via evolving-hockey.com") +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        legend.position = "right")

ggsave("career_gsax.png", width = 21.333, height = 10.666)


### plot individual goalie career gsax by shot danger by season

ggplot(gsax_career_by_season, aes(pred_goal, total_gsax)) +
  
  facet_wrap(~season, nrow = 2) +
  
  geom_line(color ="dodgerblue3", size = 1) +
  
  geom_point(color = "darkorange2", size = 1.5) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray81") +
  
  theme_few() +
  
  xlab("\nExpected shooting percentage") +
  
  ylab("Cumulative goals saved above expected") +
  
  ggtitle("Career Cumulative Goals Saved Above Expected by Shot Danger", subtitle = "All data via evolving-hockey.com") +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        strip.background = element_rect(fill = "gray36"),
        strip.text = element_text(size = 14, face = "bold", color = "white", family = "Trebuchet MS"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        legend.position = "right")

ggsave("career_gsax_faceted.png", width = 21.333, height = 10.666)
