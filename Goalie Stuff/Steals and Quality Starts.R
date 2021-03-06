### load required packages

library(tidyverse)
library(ggthemes)
library(extrafont)
library(googledrive)
library(googlesheets4)
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


### calculate game by game gsax for all games in the data set

gsax <- dbFetch(shots_table) %>%
  
  mutate(goalie = ifelse(event_team == home_team, away_goalie, home_goalie),
         team = ifelse(event_team == home_team, away_team, home_team)) %>%
  
  select(goalie, team, season, game_date, event_type, event_team, home_team, away_team, pred_goal, goal_home_weight, goal_away_weight, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  replace_na(list(goal_home_weight = 1, goal_away_weight = 1, pred_goal_home_weight = 1, pred_goal_away_weight = 1)) %>%
  
  filter(!is.na(pred_goal), !is.na(goalie)) %>%

  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         goal = ifelse(event_team == home_team, goal*goal_home_weight, goal*goal_away_weight),
         pred_goal = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight)) %>%
  
  select(goalie, team, season, game_date, pred_goal, goal) %>%
  
  group_by(goalie, team, season, game_date) %>%
  
  summarize_all(sum) %>%
  
  mutate(gsax = pred_goal - goal) %>%
  
  ungroup()
  

### calculate goal differentials by game from pbp data

goal_diff <- pbp %>%
  
  select(game_id, game_date, season, game_strength_state, event_team, event_type, home_team, away_team) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0)) %>%
  
  select(-event_type) 


team_goals <- goal_diff %>%
  
  filter(!grepl("vE", game_strength_state)) %>%
  
  select(game_id, game_date, season, team = event_team, team_goals = goal) %>%
  
  group_by(game_id, game_date, team, season) %>%
  
  summarize_all(sum) %>%
  
  ungroup()


opp_goals <- goal_diff %>%
  
  filter(!grepl("vE", game_strength_state)) %>%
  
  select(game_id, game_date, season, opp = event_team, opp_goals = goal) %>%
  
  group_by(game_id, game_date, opp, season) %>%
  
  summarize_all(sum) %>%
  
  ungroup()


goal_diff <- 
  
  inner_join(team_goals, opp_goals, by = c("game_id", "game_date", "season")) %>%
  
  filter(team != opp) %>%
  
  mutate(goal_diff = team_goals - opp_goals)


### join the gsax and goal differential data and calculate steals and quality starts

goalie_games <- 
  
  left_join(gsax, goal_diff, by = c("game_date", "season", "team")) %>%
  
  mutate(steals = ifelse(team_goals > opp_goals, 
                            ifelse(gsax > goal_diff, 1, 0), 0),
         steal_degree = ifelse(team_goals > opp_goals, 
                                 ifelse(gsax > goal_diff, gsax - goal_diff, 0), 0),
         quality_starts = ifelse(gsax > 0, 1, 0))


### turn game by game data into season totals

goalie_seasons <- goalie_games %>%
  
  select(goalie, season, quality_starts, steals, steal_degree, gsax) %>%
  
  group_by(goalie, season) %>%
  
  mutate(games = 1) %>%
  
  summarize_all(sum) %>%
  
  mutate(qual_start_share = quality_starts/games,
         steal_share = steals/games)


### turn game by game data into career totals

goalie_careers <- goalie_games %>%
  
  select(goalie, season, quality_starts, steals, steal_degree, gsax) %>%
  
  group_by(goalie) %>%
  
  mutate(games = 1) %>%
  
  summarize_all(sum) %>%
  
  mutate(qual_start_share = quality_starts/games,
         steal_share = steals/games)


### format data for plot of current season

goalie_plot_current <- goalie_seasons %>%
  
  filter(season == 20192020) %>%
  
  arrange(-games) %>%
  
  ungroup() %>%
  
  top_n(45, games)


### format data for plot of last five seasons

goalie_plot_last_five <- goalie_games %>%
  
  filter(season >= 20152016) %>%
  
  select(goalie, season, quality_starts, steals, steal_degree, gsax) %>%
  
  mutate(games = 1) %>%
  
  group_by(goalie) %>%
  
  summarize_all(sum) %>%
  
  mutate(qual_start_share = quality_starts/games,
         steal_share = steals/games) %>%
  
  arrange(-games) %>%
  
  ungroup() %>%
  
  top_n(45, games)
  

### plot scatter plot of steals and quality shares for this season

ggplot(goalie_plot_current, aes(qual_start_share, steal_share, label = goalie)) +
  
  geom_point(aes(color = games), size = 6) +
  
  geom_label_repel(family = "Trebuchet MS") +
  
  geom_vline(xintercept = mean(goalie_plot_current$qual_start_share), linetype = "dashed", color = "gray 72") +
  
  geom_hline(yintercept = mean(goalie_plot_current$steal_share), linetype = "dashed", color = "gray 72") +
  
  theme_few() +
  
  xlab("\nQuality Start %") +
  
  ylab("Steal %\n") +
  
  ggtitle("Goalie Quality Starts and Steals 2019-2020", subtitle = "All data via evolving-hockey.com") +
  
  scale_color_gradient2(low = "darkorange2", 
                        mid = "gray72", 
                        high = "dodgerblue3", 
                        limits = c(min(goalie_plot_current$games), max(goalie_plot_current$games)), 
                        midpoint = median(goalie_plot_current$games),
                        name = "Games") +
  
  scale_x_continuous(labels = scales::percent) +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold", family = "Trebuchet MS"))

ggsave("goalie_plot.png", width = 21.333, height = 10.666)



### plot scatter plot of steals and quality shares for last five seasons

ggplot(goalie_plot_last_five, aes(qual_start_share, steal_share, label = goalie)) +
  
  geom_point(aes(color = games), size = 6) +
  
  geom_label_repel(family = "Trebuchet MS") +
  
  geom_vline(xintercept = mean(goalie_plot_last_five$qual_start_share), linetype = "dashed", color = "gray 72") +
  
  geom_hline(yintercept = mean(goalie_plot_last_five$steal_share), linetype = "dashed", color = "gray 72") +
  
  theme_few() +
  
  xlab("\nQuality Start %") +
  
  ylab("Steal %\n") +
  
  ggtitle("Goalie Quality Starts and Steals Last Five Seasons", subtitle = "All data via evolving-hockey.com") +
  
  scale_color_gradient2(low = "darkorange2", 
                        mid = "gray72", 
                        high = "dodgerblue3", 
                        limits = c(min(goalie_plot_last_five$games), max(goalie_plot_last_five$games)), 
                        midpoint = median(goalie_plot_last_five$games),
                        name = "Games") +
  
  scale_x_continuous(labels = scales::percent) +
  
  scale_y_continuous(labels = scales::percent) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold", family = "Trebuchet MS"))

ggsave("goalie_plot_last_five.png", width = 21.333, height = 10.666)
