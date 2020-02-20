### load required packages

library(tidyverse)
library(stringr)
library(ggalt)
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


### create table with all goal droughts

goal_droughts <- dbFetch(shots_table) %>%
  
  select(season, game_id, game_date, session, event_player_1, event_type, event_team, home_team, away_team, pred_goal, pred_goal_home_weight, pred_goal_away_weight) %>%
  
  filter(!is.na(pred_goal), !is.na(pred_goal_home_weight)) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         pred_goal = ifelse(event_team == home_team, pred_goal*pred_goal_home_weight, pred_goal*pred_goal_away_weight)) %>%
  
  arrange(event_player_1, season, game_id) %>%
  
  mutate(reset = ifelse(goal == 0 & lag(goal, default = 0) == 1, 1, goal)) %>%
  
  group_by(event_player_1) %>%
  
  mutate(idx = cumsum(reset == 1L),
         career_xG_per_goal = sum(pred_goal)/sum(goal)) %>%
  
  group_by(event_player_1, idx) %>%
  
  mutate(drought = cumsum(pred_goal)) %>%
  
  arrange(-drought) %>%
  
  ungroup() %>%
  
  select(event_player_1, event_team, season, idx, drought, career_xG_per_goal) %>%
  
  group_by(event_player_1, idx) %>%
  
  mutate(event_team = paste0(unique(event_team), collapse = ', '),
         season = paste0(unique(season), collapse = ', ')) %>%
  
  filter(drought == max(drought)) %>%
  
  mutate(drought_degree = drought/career_xG_per_goal) %>%
  
  ungroup %>%
  
  filter(drought_degree > 1) %>%
  
  mutate(duration = ifelse(nchar(season) > 4, paste0(str_sub(season, -8, -5), " - ", str_sub(season, 5, 8)), season)) %>%
  
  arrange(-drought_degree)


### clear db connection

dbClearResult(shots_table)


### specify label positions for outlier plot

goal_drought_outlier_labels <- goal_droughts %>%
  
  filter(drought_degree > 8*sd(drought_degree) + 1) %>%
  
  mutate(y_label_position = c(250, 500, 750, 1000))

  
### plot histogram of goal drought with outliers labeled

ggplot(goal_droughts, aes(drought_degree)) +
  
  geom_histogram(bins = 90, fill = "dodgerblue4") +
  
  geom_vline(xintercept = 1 + 2*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 4*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 6*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 8*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 8*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Normal hockey stuff", x = 1.85, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Ok, this is a drought", x = 3.6, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Not funny anymore", x = 5.35, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Call a priest", x = 7.1, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Just retire, wtf", x = 8.9, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_bar(data = goal_drought_outlier_labels, aes(x = drought_degree, y = y_label_position), stat = "identity", fill = "black", width = 0.005) +
  
  geom_label(data = goal_drought_outlier_labels, aes(label = paste0(event_player_1, "\n", duration), y = y_label_position), family = "Trebuchet MS", size = 4) +
  
  theme_few() +
  
  xlab("\nGoal Drought Severity") +
  
  ylab("Count\n") +
  
  ggtitle("NHL Player Goal Drought Severity Histogram Since 2007-2008", subtitle = "All data via evolving-hockey.com") +
  
  theme(axis.text = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5))

ggsave("goal_drought_histogram.png", width = 21.333, height = 10.666)



### identify location for guide line for specific player plot

gourde_drought_degree <- goal_droughts %>%
  
  filter(event_player_1 == "YANNI.GOURDE") %>%
  
  filter(drought_degree == max(drought_degree))

gourde_drought_degree <- gourde_drought_degree$drought_degree


### plot speicif player goal drought

ggplot(goal_droughts, aes(drought_degree)) +
  
  geom_histogram(bins = 90, fill = "dodgerblue4") +
  
  geom_vline(xintercept = 1 + 2*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 4*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 6*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 8*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = 1 + 8*sd(goal_droughts$drought_degree), linetype = "dashed", color = "darkorange2", size = 2) +
  
  geom_vline(xintercept = gourde_drought_degree, linetype = "dashed", color = "gray63", size = 1) +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Normal hockey stuff", x = 1.85, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Ok, this is a drought", x = 3.6, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Not funny anymore", x = 5.35, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Call a priest", x = 7.1, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Just retire, wtf", x = 8.9, y = 2250, size = 6, family = "Trebuchet MS") +
  
  geom_text(data = goal_droughts %>% filter(drought_degree == max(drought_degree)), label = "Yanni Gourde\n2019-2020", x = gourde_drought_degree + 0.31, y = 1500, size = 4, family = "Trebuchet MS") +
  
  theme_few() +
  
  xlab("\nGoal Drought Severity") +
  
  ylab("Count\n") +
  
  ggtitle("NHL Player Goal Drought Severity Histogram Since 2007-2008", subtitle = "All data via evolving-hockey.com") +
  
  theme(axis.text = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5))

ggsave("goal_drought_gourde.png", width = 21.333, height = 10.666)


### identify top 50 goal droughts in the data set

top_50_droughts <- goal_droughts %>%
  
  top_n(50, drought_degree) %>%
  
  arrange(drought_degree) %>%
  
  mutate(label = paste0(event_player_1, " (", str_sub(duration, 3, 4), "-", str_sub(duration, 10, 11), ")"),
         label = factor(label, levels = label))


### create separate data set for labelling the dumbbell plot

top_50_droughts_legend <- top_50_droughts %>%
  
  select(label, drought, drought_degree) %>%
  
  pivot_longer(-label, names_to = "metric", values_to = "value")


### generate dumbbell plot for top 50 goal droughts

ggplot(top_50_droughts, aes(y = label)) +
  
  geom_point(data = top_50_droughts_legend, aes(x = value, color = metric)) +
  
  geom_dumbbell(aes(x = drought_degree, xend = drought), color = "gray81", colour_x = "dodgerblue3", colour_xend = "gray54", size = 1.5, size_x = 5, size_xend = 3) +
  
  theme_few() +
  
  xlab("\nGoal Drought Severity") +
  
  ylab("") +
  
  ggtitle("Top 50 NHL Player Goal Droughts Since 2007-2008", 
          subtitle = "All data via evolving-hockey.com") +
  
  scale_color_manual(values = c("dodgerblue3", "gray54"), labels = c("Adj Drought\nLength", "Raw Drought\nLength")) +
  
  guides(color = guide_legend(override.aes = list(size=4))) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 18, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 22, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 14, family = "Trebuchet MS"),
        legend.key.size = unit(1.25, "cm"))

ggsave("top_goal_droughts.png", height = 21.333, width = 10.666)