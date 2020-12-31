### load required packages

library(tidyverse)
library(RMariaDB)
library(hrbrthemes)
library(extrafont)
library(ggrepel)

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


### create table with xG and goals by arena by game by season

arena_xG <- dbFetch(shots_table) %>%
  
  mutate(opp_team = ifelse(event_team == home_team, away_team, home_team)) %>%
  
  filter(!is.na(pred_goal), opp_team == home_team) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0)*goal_away_weight,
         xG = pred_goal*pred_goal_away_weight) %>%
  
  select(season, game_date, team = opp_team, xG, goal) %>%
  
  group_by(season, game_date, team) %>%
  
  summarize_all(sum, na.rm = T) %>%
  
  group_by(season, team) %>%
  
  mutate(game_number = row_number(),
         running_xG = cumsum(xG),
         running_goals = cumsum(goal),
         goal_divergence = running_xG - running_goals) %>%
  
  ungroup()


### filter for final numbers for labelling

labels <- arena_xG %>%
  
  group_by(team, season) %>%
  
  filter(game_number == max(game_number)) %>%
  
  group_by(season) %>%
  
  arrange(season, -goal_divergence) %>%
  
  mutate(rank = row_number(),
         n = n()) %>%
  
  filter(rank < 4 | rank > (n - 3)) %>%
  
  mutate(color = 
           case_when(
             rank < 4 ~ "dodgerblue3",
             rank > (n-3) ~ "darkorange2"
           ),
         team_season = paste0(team, "_", season),
         team_season = factor(team_season, levels = team_season)) %>%
  
  ungroup()


### add colors to arena dataframe

colors <- arena_xG %>%
  
  group_by(team, season) %>%
  
  filter(game_number == max(game_number)) %>%
  
  group_by(season) %>%
  
  arrange(season, -goal_divergence) %>%
  
  mutate(rank = row_number(),
         n = n()) %>%
  
  mutate(color = 
           case_when(
             rank < 4 ~ "dodgerblue3",
             rank > (n-3) ~ "darkorange2",
             TRUE ~ "gray81"
           )) %>%
  
  select(team, season, max_divergence = goal_divergence, color) %>%
  
  ungroup()

  
arena_xG_plot <-
  
  left_join(arena_xG, colors, by = c("team" = "team", "season" = "season")) %>%
  
  replace_na(list(color = "gray81")) %>%
  
  mutate(team_season = paste(team, "_", season)) %>%
  
  arrange(-max_divergence) %>%
  
  mutate(team_season = factor(team_season, levels = unique(team_season))) %>%
  
  ungroup()


colors_sort <- arena_xG_plot %>%
  
  select(team_season, max_divergence, color) %>%
  
  distinct() %>%
  
  arrange(-max_divergence) %>%
  
  mutate(team_season = factor(team_season, levels = unique(team_season))) %>%
  
  ungroup()


### plot xG and goals by game by arena


ggplot(arena_xG_plot, aes(game_number, goal_divergence, color = team_season)) +
  
  facet_wrap(~season, nrow = 2) +
  
  geom_line() +
  
  geom_label_repel(data = labels, aes(label = team), color = labels$color) +
  
  theme_ipsum_ps() +
  
  ggtitle("Goal Divergence from Expected by Arena by Season", 
          subtitle = "Data via Evolving Hockey") +
  
  xlab("") +
  
  ylab("") +
  
  scale_color_manual(values = colors_sort$color) +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"),
        legend.position = "none")

ggsave(filename = "viz/arena_xG_divergence.png", width = 21.333, height = 10.66)
