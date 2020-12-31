### load required packages

library(tidyverse)
library(ggthemes)
library(extrafont)
library(RMariaDB)
library(hrbrthemes)


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
  
  mutate(goalie = ifelse(event_team == home_team, away_goalie, home_goalie),
         team = ifelse(event_team == home_team, away_team, home_team)) %>%
  
  select(goalie, team, season, event_type, event_team, home_team, away_team, pred_goal, goal_home_weight, goal_away_weight, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  filter(!is.na(pred_goal), !is.na(goalie), !is.na(goal_home_weight)) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         goal = ifelse(event_team == home_team, goal*goal_home_weight, goal*goal_away_weight),
         pred_goal = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight)) %>%
  
  select(goalie, team, season, pred_goal, goal)


### create data frame with first year for correlation testing

goalie_seasons_1 <- goal_danger %>%
  
  mutate(season = as.numeric(substr(season, 5, 8))) %>%
  
  group_by(goalie, team, season) %>%
  
  add_tally() %>%
  
  summarize_all(sum) %>%
  
  mutate(n = sqrt(n)) %>%
  
  group_by(goalie, team, season) %>%
  
  mutate(total_shots = sum(n)) %>%
  
  ungroup() %>%
  
  filter(total_shots >= 600) %>%
  
  mutate(performance = (pred_goal - goal)/n) %>%
  
  select(goalie, team_1 = team, season_1 = season, pred_goal_1 = pred_goal, goal_1 = goal, performance_1 = performance)
  
  
### create data frame with second year for correlation testing

goalie_seasons_2 <- goalie_seasons_1 %>%
  
  mutate(season_2 = season_1 - 1) %>%
  
  select(goalie, team_2 = team_1, season_2, pred_goal_2 = pred_goal_1, goal_2 = goal_1, performance_2 = performance_1)


### join both data frames for testing and keep only complete cases

goalie_seasons <- 
  
  left_join(goalie_seasons_1, goalie_seasons_2, by = c("goalie" = "goalie", "season_1" = "season_2")) %>%
  
  drop_na() %>%
  
  mutate(team_status = ifelse(team_1 == team_2, "Same", "Change"),
         performance_change = abs(performance_1-performance_2)) %>%
  
  group_by(team_status) %>%
  
  mutate(cor = cor(performance_1, performance_2),
         facet_label = paste0(team_status, " | r = ", round(cor, 2))) %>%
  
  ungroup()


### run t test to check for significance of changing teams on change in goalie 'performance'

t.test(goalie_seasons$performance_change ~ goalie_seasons$team_status)


### scatter plot year over year performance

ggplot(goalie_seasons, aes(performance_1, performance_2)) +
  
  facet_wrap(~facet_label) +
  
  geom_point(size = 2) +
  
  geom_smooth(size = 1.5, color = "dodgerblue3", fill = "gray72") +
  
  ylab("Goals saved per unblocked shot year two\n") +
  
  xlab("\nGoals saved per unblocked shot year one") +
  
  theme_ipsum_ps() +
  
  ggtitle("How changing teams in the offseason affects year over repeatability of goalie performance", 
          subtitle = "PBP data via Evolving Hockey") +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"))

ggsave(filename = "viz/goalie_repeatability_eh.png", width = 21.333, height = 10.666)


### gsax distributions

ggplot(goalie_seasons, aes(performance_1)) +
  
  facet_wrap(~team_status) +
  
  geom_density(fill = "darkorange3", alpha = 0.33) +
  
  ylab("") +
  
  xlab("\nGoals saved per unblocked shot year one") +
  
  theme_ipsum_ps() +
  
  ggtitle("Comparing the distribution of goalie performance and team status", 
          subtitle = "PBP data via Evolving Hockey") +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"))

ggsave(filename = "viz/goalie_distros_eh.png", width = 21.333, height = 10.666)


### extra nerd stuff

## regression to confirm what scatter plot shows

goalie_model <- goalie_seasons %>%
  
  mutate(team_status_numeric = ifelse(team_status == "Change", 1, 0))

model <- lm(performance_2 ~ performance_1*team_status, goalie_seasons)

summary(model)


## check the residuals

par(mfrow = c(2, 2))

plot(model)

