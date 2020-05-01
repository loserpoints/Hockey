### to do
## better comments
## fix all fonts

library(tidyverse)  
library(ggthemes) 
library(ggpubr)
library(extrafont)


### set working directory

setwd("C:/Users/Alan/Documents/R/Projects/Hockey/10 Game Report")


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


shots_query <- 
  
  "SELECT * FROM shots WHERE season = 20192020"


shots_table <- dbSendQuery(shots_db, shots_query)

### get goalie data

goalies <- dbFetch(shots_table) %>%
  
  select(game_date, event_type, game_strength_state, event_team, home_team, away_team, home_goalie, away_goalie, pred_goal, goal_home_weight, goal_away_weight, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  filter(!is.na(pred_goal), !is.na(event_team), !is.na(goal_home_weight)) %>%
  
  group_by(event_team) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         goal = ifelse(event_team == home_team, goal*goal_home_weight, goal*goal_away_weight),
         pred_goal = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight)) %>%
  
  ungroup() %>%
  
  mutate(event_goalie = ifelse(event_team == home_team, away_goalie, home_goalie),
         Team = ifelse(event_team == home_team, away_team, home_team)) %>%
  
  select(Date = game_date, Player = event_goalie, Team, Strength = game_strength_state, xGA = pred_goal, GA = goal) %>%
  
  group_by(Date, Player, Team, Strength) %>%
  
  summarize_all(sum, na.rm = T) %>%
  
  filter(Strength == "5v5" | Strength == "5v4" | Strength == "4v5")


### aaplit data by strength state

goalies.5v5 <- goalies %>%
  
  filter(Strength == "5v5")


goalies.all <- goalies %>%
  
  mutate(Strength = "all") %>%
  
  group_by(Date, Player, Team, Strength) %>%
  
  summarize_all(sum)


### re-combine goalie data

goalies <- rbind(goalies.5v5, goalies.all)


### calculate running gsax

goalies <- goalies %>%
  
  arrange(Player, Strength, Date) %>%
  
  group_by(Player, Strength) %>%
  
  mutate (Game = row_number(), Goals.Saved = xGA - GA) %>%
  
  mutate_at(vars(xGA, GA, Goals.Saved), funs(Running = cumsum(.))) %>%
  
  ungroup()


#select tampa goalies and format for game by game charts by team game

tb.goalie1 <- goalies %>%
  
  select(Team, Date) %>%
  
  distinct() %>%
  
  filter(Team == "T.B") %>%
  
  mutate(Player = "ANDREI.VASILEVSKIY") %>%
  
  arrange(Date) %>%
  
  mutate(Game = row_number())


tb.goalie2 <- goalies %>%
  
  select(Team, Date) %>%
  
  distinct() %>%
  
  filter(Team == "T.B") %>%
  
  mutate(Player = "CURTIS.MCELHINNEY") %>%
  
  arrange(Date) %>%
  
  mutate(Game = row_number())


tb.goalies <- rbind(tb.goalie1, tb.goalie2)


tb.goalie1 <- tb.goalies %>% 
  
  mutate(Strength = "5v5")

tb.goalie2 <- tb.goalies %>% 
  
  mutate(Strength = "all")

tb.goalies <- rbind(tb.goalie1, tb.goalie2)


tb.goalies <- 
  
  left_join(tb.goalies, goalies[c(-7)], by = c("Team", "Date", "Player", "Strength"))


#select 5v5 data only

gbg5v5 <- tb.goalies %>% 
  
  filter(Strength == "5v5")


#plot 5v5 chart

gbg5v5.p <- ggplot(gbg5v5, aes(x = Game, y = Goals.Saved, fill = Player)) +
  
  geom_bar(stat = "identity") +
  
  facet_wrap(~Player, ncol = 1) +
  
  theme_few() +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  ggtitle("5v5") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray27") +
  
  theme(strip.background = element_rect(fill = "gray36"), strip.text = element_text(color = "white", face = "bold", size = 18)) +
  
  theme(axis.text.x = element_text(face = "bold", size = 14)) +
  
  theme(axis.text.y = element_text(face = "bold", size = 14)) +
  
  theme(axis.title.x = element_text(face = "bold", size = 18), axis.title.y = element_text(face = "bold", size = 22)) +
  
  theme(plot.title = element_text(face = "bold", size = 28, hjust = 0.5)) +
  
  theme(legend.position = "none") +
  
  labs(y = "Goals Saved Above Expected") +
  
  theme(plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5)))


#select all situations data

gbgall <- tb.goalies %>% 
  
  filter(Strength == "all")


#plot allsits chart

gbgall.p <- ggplot(gbgall, aes(x = Game, y = Goals.Saved, fill = Player)) +
  
  geom_bar(stat = "identity") +
  
  facet_wrap(~Player, ncol = 1) +
  
  theme_few() +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  ggtitle("All Situations") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray27") +
  
  theme(strip.background = element_rect(fill = "gray36"), strip.text = element_text(color = "white", face = "bold", size = 18)) +
  
  theme(axis.text.x = element_text(face = "bold", size = 14)) +
  
  theme(axis.text.y = element_text(face = "bold", size = 14)) +
  
  theme(axis.title.x = element_text(face = "bold", size = 18), axis.title.y = element_text(face = "bold", size = 22)) +
  
  theme(plot.title = element_text(face = "bold", size = 28, hjust = 0.5)) +
  
  theme(legend.position = "none") +
  
  labs(y = "Goals Saved Above Expected") +
  
  theme(plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5)))


#format data for running 5v5 goals saved charts

running5v5 <- goalies %>% 
  
  filter(Strength == "5v5") %>%
  
  mutate(Color = ifelse(Player == "ANDREI.VASILEVSKIY", "dodgerblue3", ifelse(Player == "CURTIS.MCELHINNEY", "darkorange2", "gray81"))) %>%
  
  mutate(Order = ifelse(Player == "ANDREI.VASILEVSKIY", 1, ifelse(Player == "CURTIS.MCELHINNEY", 2, 3))) %>%
  
  arrange(-Order)


running5v5$Player <- factor(running5v5$Player, levels = unique(running5v5$Player))


color <- running5v5 %>%
  
  select(Player, Color) %>%
  
  distinct()


#plot running 5v5 goals saved chart

running5v5.p <- ggplot(running5v5, aes(x = Game, y = Goals.Saved_Running, color = Player)) +
  
  geom_line(size = 3) +
  
  theme_few() +
  
  scale_color_manual(values = color$Color) +
  
  ggtitle("5v5") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray27") +
  
  theme(axis.text.x = element_text(face = "bold", size = 14)) +
  
  theme(axis.text.y = element_text(face = "bold", size = 14)) +
  
  theme(axis.title.x = element_text(face = "bold", size = 18), axis.title.y = element_text(face = "bold", size = 18)) +
  
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  
  theme(legend.title = element_blank()) +
  
  theme(legend.position = "none") +
  
  labs(y = "Goals Saved Above Expected") +
  
  theme(plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5)))


#format data for running all situations goals saved charts

runningall <- goalies %>% 
  
  filter(Strength == "all") %>%
  
  mutate(Color = ifelse(Player == "ANDREI.VASILEVSKIY", "dodgerblue3", ifelse(Player == "CURTIS.MCELHINNEY", "darkorange2", "gray81"))) %>%
  
  mutate(Order = ifelse(Player == "ANDREI.VASILEVSKIY", 1, ifelse(Player == "CURTIS.MCELHINNEY", 2, 3))) %>%
  
  arrange(-Order)


runningall$Player <- factor(runningall$Player, levels = unique(runningall$Player))


color <- runningall %>%
  
  select(Player, Color) %>%
  
  distinct()


#plot running all situations goals saved chart

runningall.p <- ggplot(runningall, aes(x = Game, y = Goals.Saved_Running, color = Player)) +
  
  geom_line(size = 3) +
  
  theme_few() +
  
  scale_color_manual(values = color$Color) +
  
  ggtitle("All Situations") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray27") +
  
  theme(axis.text.x = element_text(face = "bold", size = 14)) +
  
  theme(axis.text.y = element_text(face = "bold", size = 14)) +
  
  theme(axis.title.x = element_text(face = "bold", size = 18), axis.title.y = element_text(face = "bold", size = 18)) +
  
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5)) +
  
  theme(legend.title = element_blank()) +
  
  theme(legend.position = "none") +
  
  labs(y = "Goals Saved Above Expected") +
  
  theme(plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5)))


#create dahsboards

gbgdash <- ggarrange(gbg5v5.p, gbgall.p, ncol = 2)

annotate_figure(
  gbgdash,
  top = text_grob(
    "\nGame By Game Goals Saved Above Expected",
    size = 28,
    face = "bold"
  ),
  bottom = text_grob(
    "data via evolving-hockey.com, chart by @loserpoints\n",
    hjust = 1.1,
    x = 1,
    vjust = 1,
    face = "italic",
    size = 18
  )
)


ggsave("Viz/goalie_gbg.png", width = 21.333, height = 10.667)

runningdash <- ggarrange(running5v5.p, runningall.p, ncol = 2)

annotate_figure(
  runningdash,
  top = text_grob(
    "\nGame By Game Goals Saved Above Expected",
    size = 28,
    face = "bold"
  ),
  bottom = text_grob(
    "data via evolving-hockey.com, chart by @loserpoints\n",
    hjust = 1.1,
    x = 1,
    vjust = 1,
    face = "italic",
    size = 18
  )
)

ggsave("Viz/goalie_running.png", width = 21.333, height = 10.667)
