### load required packages

library(tidyverse)
library(extrafont)
library(RMariaDB)

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

period_xg <- dbFetch(shots_table) %>%
  
  filter(!is.na(pred_goal), game_strength_state == "5v5", game_period < 4) %>%
  
  select(season, game_id, game_period, event_team, home_team, away_team, pred_goal, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  mutate(team_xg = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight),
         opponent = ifelse(event_team == home_team, away_team, home_team)) %>%
  
  select(season, game_id, game_period, team = event_team, opponent, team_xg) %>%
  
  group_by(season, game_id, game_period, team, opponent) %>%
  
  summarize_all(sum) %>%
  
  group_by(season, game_id, game_period) %>%
  
  mutate(total_xg = sum(team_xg), xg_share = team_xg/total_xg, xg_diff = team_xg-(total_xg-team_xg)) %>%
  
  ungroup() %>%
  
  pivot_longer(-c(season, game_id, game_period, team, opponent), names_to = "metric", values_to = "value") %>%
  
  filter(metric == "xg_share" | metric == "xg_diff") %>%
  
  mutate(metric = gsub("xg_share", "Expected Goal Share", metric),
         metric = gsub("xg_diff", "Expected Goal Differential", metric),
         metric = factor(metric, levels = c("Expected Goal Share", "Expected Goal Differential")))


lightning_game <- data.frame(season = 20192020, 
                             game_id = NA, 
                             game_period = 2, 
                             team = "T.B", 
                             opponent = "DAL", 
                             metric = c("Expected Goal Share", "Expected Goal Differential"), 
                             value = c(0.984, 1.26),
                             x = c(0.88, 2.3),
                             y = c(1.065, 0.5))


ggplot(period_xg, aes(value)) +
  
  facet_wrap(~metric, scales = "free") +
  
  geom_density(fill = "gray81") +
  
  geom_vline(data = lightning_game, aes(xintercept = value), linetype = "dashed", color = "dodgerblue3", size = 1.5) +
  
  geom_text(data = lightning_game, aes(x = x, y = y, label = paste0(label = "Tampa Bay Lighnting\nSecond Period\nGame 2 2020 SCF\n", value)), size = 4, family = "Trebuchet MS") +
  
  theme_ipsum_ps() +
  
  ggtitle("Distribution of NHL single period 5v5 adjusted expected goal share and differential", 
          subtitle = "Distributions based on regular season data via Evolving Hockey") +
  
  xlab("") +
  
  ylab("") +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"))

ggsave(filename = "viz/lightning_2020.png", width = 21.333, height = 10.66)

