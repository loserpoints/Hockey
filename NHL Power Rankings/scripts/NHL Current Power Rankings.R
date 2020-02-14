###### load required packages

library(tidyverse)
library(rvest)
library(BradleyTerry2)
library(ggthemes)
library(scales)
library(ggalt)
library(extrafont)


###### run function to generate power rankings and associated plots

power.rankings <- get.power.rankings()


###### define function to generate power rankings and associated plots

get.power.rankings <- function(x) {

##### Generate NHL goal and xG based power ranking plots

#### read and format required data from natural stat trick

site_5v5 <- "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=sva&loc=B&team=All&rate=n"

data_5v5 <- read_html(site_5v5) %>%
  
  html_table() %>%
  
  data.frame(.) %>%
  
  mutate(Strength = "5v5") %>%
  
  select(Game, Team, Strength, xGF, xGA, GF, GA)


site_5v4 <- "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=5v4&loc=B&team=All&rate=n"

data_5v4 <- read_html(site_5v4) %>%
  
  html_table() %>%
  
  data.frame(.) %>%
  
  mutate(Strength = "5v4") %>%
  
  select(Game, Team, Strength, xGF, xGA, GF, GA)


site_4v5 <- "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=4v5&loc=B&team=All&rate=n"

data_4v5 <- read_html(site_4v5) %>%
  
  html_table() %>%
  
  data.frame(.) %>%
  
  mutate(Strength = "4v5") %>%
  
  select(Game, Team, Strength, xGF, xGA, GF, GA)


data_all <- rbind(data_5v5, data_5v4, data_4v5) %>%
  
  rename(game = Game, team = Team, strength = Strength) %>%
  
  mutate(date = substr(game, 1, 10), 
         
         game = substr(game, 14, nchar(game))) %>%
  
  separate(game, into = c("away_team", "home_team"), sep = ",") %>%
  
  mutate_at(vars(away_team, home_team), funs(trimws(substr(., 1, nchar(.)-2)))) %>%
  
  rowwise() %>%
  
  mutate(home = ifelse(grepl(home_team, team), 1, 0),
         team = ifelse(home == 1, home_team, away_team),
         opp = ifelse(home == 0, home_team, away_team)) %>%
         
  select(team, opp, date, home, xGF, xGA, GF, GA) %>%
  
  group_by(team, opp, date, home) %>%
  
  summarize_all(sum) %>%
  
  mutate(outcome_goals = GF/(GF + GA),
         outcome_xG = xGF/(xGF + xGA))


#### get ability estimates

### fit goals model
fit_goals <- BTm(outcome = outcome_goals, formula = ~team + home, player1 = team, player2 = opp, id = "team", data = data_all) 


### get goal ability estimates
abilities_goals <- as.data.frame(BTabilities(fit_goals)) %>%
  
  mutate(team = row.names(.)) %>%
  
  select(team, ability_goals = ability) %>%
  
  mutate(ability_goals = rescale(ability_goals, to = c(1, 100)))


###fit xG model
fit_xG <- BTm(outcome = outcome_xG, formula = ~team + home, player1 = team, player2 = opp, id = "team", data = data_all) 


### get xG ability estimates
abilities_xG <- as.data.frame(BTabilities(fit_xG)) %>%
  
  mutate(team = row.names(.)) %>%
  
  select(team, ability_xG = ability) %>%
  
  mutate(ability_xG = rescale(ability_xG, to = c(1, 100)))


### merge goal and xG ability estimates
abilities <- 
  
  left_join( abilities_goals, abilities_xG, by = c("team")) %>%
  
  arrange(ability_xG) %>%
  
  mutate(team = factor(team, levels = team))


## create separate df for legend
abilities_legend <- abilities %>%
  
  pivot_longer(-team, names_to = "metric", values_to = "value")


## load extra fonts
loadfonts(device = "win")


#### plot xG and goal power rankings
ggplot(abilities, aes(y = team)) +
  
  geom_point(data = abilities_legend, aes(x = value, color = metric)) +
  
  geom_dumbbell(aes(x = ability_xG, xend = ability_goals), color = "gray81", colour_x = "dodgerblue3", colour_xend = "darkorange2", size = 1.5, size_x = 5, size_xend = 3) +
  
  theme_few() +
  
  xlab("\nTeam Quality (scaled 0-100)") +
  
  ylab("Team\n") +
  
  ggtitle("2019-2020 NHL Power Rankings", subtitle = "Team Quality based on Bradley-Terry ability estimates\n") +
  
  scale_color_manual(values = c("dodgerblue3", "darkorange2"), labels = c("xG", "Goals")) +
  
  guides(color = guide_legend(override.aes = list(size=4))) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Trebuchet MS"))

ggsave("viz/nhl_power_rankings_current.png", height = 10.666, width = 21.333)

return(abilities)


#### calculate and plot outcome and strength of schedule distributions


### format game by game data

gbg <- rbind(data_5v5, data_5v4, data_4v5) %>%
  
  rename(game = Game, team = Team, strength = Strength) %>%
  
  mutate(date = substr(game, 1, 10), 
         
         game = substr(game, 14, nchar(game))) %>%
  
  separate(game, into = c("away_team", "home_team"), sep = ",") %>%
  
  mutate_at(vars(away_team, home_team), funs(trimws(substr(., 1, nchar(.)-2)))) %>%
  
  rowwise() %>%
  
  mutate(home = ifelse(grepl(home_team, team), 1, 0),
         team = ifelse(home == 1, home_team, away_team),
         opp = ifelse(home == 0, home_team, away_team)) %>%
  
  select(team, opp, date, home, xGF, xGA, GF, GA) %>%
  
  group_by(team, opp, date, home) %>%
  
  summarize_all(sum) %>%
  
  ungroup() %>%
  
  mutate(goal_diff = GF - GA,
         xG_diff = xGF - xGA) %>%
  
  left_join(., abilities, by = c("opp" = "team"))


### reorder team factor by xG power ranking

gbg_diffs <- gbg %>%
  
  mutate(team = factor(team, levels = rev(abilities$team)))


#### plot xG and goal diff outcome distributions

ggplot(gbg_diffs) +
  
  geom_density(aes(goal_diff, fill = "Goals"),  alpha = 0.2) +
  
  geom_density(aes(xG_diff, fill = "xG"), alpha = 0.6) +
  
  facet_wrap(~team, nrow = 4) +
  
  theme_few() +
  
  xlab("\nDifferential") +
  
  ylab("") +
  
  ggtitle("2019-2020 NHL Team Goal and xG Differential Distributions", subtitle = "Data via evolving-hockey.com") +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray36") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        strip.background = element_rect(fill = "gray30"),
        strip.text = element_text(size = 16, color = "white", face = "bold", family = "Trebuchet MS"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        legend.position = "bottom",
        legend.spacing = unit(2, "cm"))

ggsave("viz/nhl_game_result_distros.png", height = 10.666, width = 21.333)


### reorder team factor by xG power ranking

gbg_sked_strength_means <- gbg %>%
  
  select(team, ability_xG) %>%
  
  group_by(team) %>%
  
  summarize_all(mean) %>%
  
  arrange(-ability_xG) %>%
  
  mutate(team = factor(team, levels = team))


gbg_sked_strength <- gbg %>%
  
  mutate(team = factor(team, levels = gbg_sked_strength_means$team))


#### plot schedule strength distributions

ggplot(gbg_sked_strength) +
  
  geom_density(aes(ability_goals, fill = "Goals"), alpha = 0.2) +
  
  geom_density(aes(ability_xG, fill = "xG"), alpha = 0.6) +
  
  geom_vline(data = gbg_sked_strength_means, aes(xintercept = ability_xG), linetype = "dashed", color = "gray36") +
  
  facet_wrap(~team, nrow = 4) +
  
  theme_few() +
  
  xlab("\nOpponent Quality") +
  
  ylab("") +
  
  ggtitle("2019-2020 NHL Team Strength of Schedule Distributions", subtitle = "Based on opponents' Bradley-Terry ability estimates") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        strip.background = element_rect(fill = "gray30"),
        strip.text = element_text(size = 16, color = "white", face = "bold", family = "Trebuchet MS"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        legend.position = "bottom",
        legend.spacing = unit(2, "cm"))


ggsave("viz/nhl_schedule_strength_distributions.png", height = 10.666, width = 21.333)


#### plot relationships between team quality and outcomes

ggplot(gbg, aes(ability_xG, xG_diff)) +
  
  geom_point(color = "dodgerblue3", size = 3) +
  
  geom_smooth(method = "lm", color = "darkorange2", size = 2) +
  
  facet_wrap(~team, nrow = 4) +
  
  theme_few() +
  
  xlab("\nOpponent Quality") +
  
  ylab("Expected Goal Differential\n") +
  
  ggtitle("2019-2020 NHL Team Performance by Competition Level", subtitle = "Based on opponents' Bradley-Terry ability estimates") +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        strip.background = element_rect(fill = "gray30"),
        strip.text = element_text(size = 16, color = "white", face = "bold", family = "Trebuchet MS"))


ggsave("viz/nhl_xg_diff_opp_quality.png", height = 10.666, width = 21.333)


}
