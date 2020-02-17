library(tidyverse)
library(ggthemes)
library(extrafont)
library(googledrive)
library(googlesheets4)

loadfonts(device = "win")

adj <- read_csv("score_venue_adjustments.csv")

pbp1 <- read_csv("shots_1920.csv")
pbp2 <- read_csv("shots_1819.csv")
pbp3 <- read_csv("shots_1718.csv")
pbp4 <- read_csv("shots_1617.csv")
pbp5 <- read_csv("shots_1516.csv")
pbp6 <- read_csv("shots_1415.csv")
pbp7 <- read_csv("shots_1314.csv")

pbp <- rbind(pbp1, pbp2, pbp3, pbp4, pbp5, pbp6, pbp7)

score_state <- pbp %>%
  
  mutate(event_id = row_number()) %>%
  
  select(event_id, home_score, away_score) %>%
  
  mutate(home_leading = ifelse(home_score > away_score, "leading", NA),
         home_tied = ifelse(home_score == away_score, "tied", NA),
         home_trailing = ifelse(home_score < away_score, "trailing", NA),
         away_leading = ifelse(home_score < away_score, "leading", NA),
         away_tied = ifelse(home_score == away_score, "tied", NA),
         away_trailing = ifelse(home_score > away_score, "trailing", NA)) %>%
  
  select(-home_score, -away_score) %>%
  
  pivot_longer(-c(event_id, away_leading, away_tied, away_trailing), names_to = "measure", values_to = "home_score_state") %>%
  
  filter(!is.na(home_score_state)) %>%
  
  select(-measure) %>%
  
  pivot_longer(-c(event_id, home_score_state), names_to = "measure", values_to = "away_score_state") %>%
  
  filter(!is.na(away_score_state)) %>%
  
  select(-measure)


pbp <- cbind(pbp, score_state) %>%
  
  mutate(score_state = ifelse(event_team == home_team, home_score_state, away_score_state)) %>%
  
  left_join(adj, by = c("game_strength_state" = "strength", "score_state" = "score_state"))



goal_danger <- pbp %>%
  
  mutate(goalie = ifelse(event_team == home_team, away_goalie, home_goalie)) %>%
  
  select(goalie, season, event_type, event_team, home_team, away_team, pred_goal, goal_home_weight, goal_away_weight, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  filter(!is.na(pred_goal), !is.na(goalie), !is.na(goal_home_weight)) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         goal = ifelse(event_team == home_team, goal*goal_home_weight, goal*goal_away_weight),
         pred_goal = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight)) %>%
  
  select(goalie, season, pred_goal, goal)




select_goalie <- "ANDREI.VASILEVSKIY"

gsax <- goal_danger %>%
  
  filter(goalie == select_goalie)




gsax_career_total <- gsax %>%
  
  mutate(gsax = pred_goal - goal) %>%
  
  arrange(goalie, pred_goal) %>%
  
  group_by(goalie) %>%
  
  mutate(total_gsax = cumsum(gsax)) %>%
  
  ungroup()


gsax_career_by_season <- gsax %>%
  
  mutate(gsax = pred_goal - goal) %>%
  
  arrange(goalie, pred_goal) %>%
  
  group_by(goalie, season) %>%
  
  mutate(total_gsax = cumsum(gsax)) %>%
  
  ungroup()



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
