### load required packages

library(tidyverse)
library(ggthemes)
library(extrafont)
library(RMariaDB)
library(hrbrthemes)


### load fonts for viz

loadfonts(device = "win")


### load moneypuck data from csv

shots_mp_1 <- read_csv("data/shots_mp_1.csv") %>%
  
  filter(shotOnEmptyNet == 0) %>%
  
  mutate(goalie_team = ifelse(team == "HOME", awayTeamCode, homeTeamCode),
         season = season + 1) %>%
  
  select(goalie = goalieNameForShot, team = goalie_team, season, goal, xGoal)


shots_mp_2 <- read_csv("data/shots_mp_2.csv") %>%
  
  filter(shotOnEmptyNet == 0) %>%
  
  mutate(goalie_team = ifelse(team == "HOME", awayTeamCode, homeTeamCode),
         season = season + 1) %>%
  
  select(goalie = goalieNameForShot, team = goalie_team, season, goal, xGoal)


shots_mp <- rbind(shots_mp_1, shots_mp_2)


### create data frame with first year for correlation testing

goalie_seasons_1 <- shots_mp %>%
  
  group_by(goalie, team, season) %>%
  
  add_tally() %>%
  
  summarize_all(sum) %>%
  
  mutate(n = sqrt(n)) %>%
  
  group_by(goalie, team, season) %>%
  
  mutate(total_shots = sum(n)) %>%
  
  ungroup() %>%
  
  filter(total_shots >= 600) %>%
  
  mutate(performance = (xGoal - goal)/n) %>%
  
  select(goalie, team_1 = team, season_1 = season, xGoal_1 = xGoal, goal_1 = goal, performance_1 = performance)
  
  
### create data frame with second year for correlation testing

goalie_seasons_2 <- goalie_seasons_1 %>%
  
  mutate(season_2 = season_1 - 1) %>%
  
  select(goalie, team_2 = team_1, season_2, xGoal_2 = xGoal_1, goal_2 = goal_1, performance_2 = performance_1)


### join both data frames for testing and keep only complete cases

goalie_seasons <- 
  
  left_join(goalie_seasons_1, goalie_seasons_2, by = c("goalie" = "goalie", "season_1" = "season_2")) %>%
  
  drop_na() %>%
  
  mutate(team_status = ifelse(team_1 == team_2, "Same", "Change"),
         performance_change = abs(performance_1-performance_2)) %>%
  
  group_by(team_status) %>%
  
  mutate(cor = cor(performance_1, performance_2),
         facet_label = paste0(team_status, " | r = ", round(cor, 2)))


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
          subtitle = "PBP data via MoneyPuck") +
  
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

ggsave(filename = "viz/goalie_repeatability_mp.png", width = 21.333, height = 10.666)

