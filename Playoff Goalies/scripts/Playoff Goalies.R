library(tidyverse)
library(extrafont)
library(hrbrthemes)


file_list <- list.files("data/")

file_list_df <- lapply(file_list, function(i) {
  
  read_csv(paste0("data/", i))
  
})
  
  
shots <- do.call(rbind, file_list_df) %>%
  
  mutate(goal = ifelse(event_type == "GOAL", 1, 0),
         pred_goal = replace_na(pred_goal, mean(pred_goal, na.rm = T)))


series_ids <- shots %>%
  
  select(game_id) %>%
  
  mutate(series_id = substr(game_id, 1, 9)) %>%
  
  select(series_id) %>%
  
  unique()


series_ids <- series_ids$series_id


df_list <- lapply(series_ids, function(x) {
  
  series_xg <- shots %>%
    
    filter(grepl(x, game_id)) %>%
    
    mutate(series_id = x,
           empty_net = 
             case_when(
               event_team == home_team & is.na(away_goalie)  ~ "empty net",
               event_team == away_team & is.na(home_goalie)  ~ "empty net")) %>%
    
    filter(is.na(empty_net)) %>%
    
    select(series_id, game_id, event_team, pred_goal) %>%
    
    group_by(series_id, game_id, event_team) %>%
    
    summarize_all(sum) %>%
    
    pivot_wider(names_from = event_team, values_from = pred_goal) %>%
    
    rename(team1_xg = 3, team2_xg = 4) %>%
    
    ungroup()
  
  
  series_g <- shots %>%
    
    filter(grepl(x, game_id)) %>%
    
    mutate(series_id = x) %>%
    
    select(series_id, game_id, event_team, goal) %>%
    
    group_by(series_id, game_id, event_team) %>%
    
    summarize_all(sum) %>%
    
    pivot_wider(names_from = event_team, values_from = goal) %>%
    
    rename(team1_g = 3, team2_g = 4) %>%
    
    ungroup()
  
  
  series <- 
    
    left_join(series_xg, series_g, by = c("series_id" = "series_id", "game_id" = "game_id"))
  
})


series <- do.call(rbind, df_list) %>%
  
  mutate(team1_xwin = ifelse(team1_xg > team2_xg, 1, 0),
         team1_win = ifelse(team1_g > team2_g, 1, 0),
         team1_xloss = ifelse(team1_xg < team2_xg, 1, 0),
         team1_loss = ifelse(team1_g < team2_g, 1, 0)) %>%
  
  select(series_id, team1_xwin, team1_win, team1_xloss, team1_loss) %>%
  
  group_by(series_id) %>%
  
  summarize_all(sum) %>%
  
  ungroup() %>%
  
  mutate(outcome = 
           case_when(
             team1_win == 4 & team1_xwin >= 4 ~ "Just win",
             team1_loss == 4 & team1_xloss >= 4 ~ "Just win",
             team1_win == 4 & team1_xwin < 4 & team1_xloss < 4 ~ "Early win",
             team1_loss == 4 & team1_xloss < 4 & team1_xwin < 4 ~ "Early win",
             team1_win == 4 & team1_xwin < 4 & team1_xloss >= 4 ~ "Unjust win",
             team1_loss == 4 & team1_xloss < 4 & team1_xwin >= 4 ~ "Unjust win"
             ),
         outcome = replace_na(outcome, "Early win"),
         outcome = factor(outcome, levels = c("Just win", "Early win", "Unjust win")),
         season = as.numeric(substr(series_id, 1, 4)) + 1
         ) %>%
  
  filter(season != 2008)


total_plot <- 
  
  ggplot(series, aes(outcome, fill = outcome)) +
  
  geom_bar(alpha = 0.75) +
  
  ylab("Total Count\n") +
  
  xlab("") +
  
  theme_ipsum_ps() +
  
  ggtitle("How often do NHL playoff series come down to\nshooting and/or goaltending?", 
          subtitle = "PBP data via Evolving Hockey") +
  
  scale_fill_manual(values = c("dodgerblue4", "gray54", "darkorange3")) +
  
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18, hjust = 1),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"),
        legend.position = "none")


season_plot <- 
  
  ggplot(series, aes(outcome, fill = outcome)) +
  
  facet_wrap(~season, nrow = 4) +
  
  geom_bar(alpha = 0.75) +
  
  ylab("Season Count\n") +
  
  xlab("") +
  
  theme_ipsum_ps() +
  
  #ggtitle("How often does the outcome of an NHL playoff series come down to shooting/goaltending", 
          #subtitle = "PBP data via Evolving Hockey") +
  
  scale_fill_manual(values = c("dodgerblue4", "gray54", "darkorange3")) +
  
  scale_y_continuous(breaks = c(0, 3, 6, 9)) +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"),
        legend.position = "none")


ggarrange(total_plot, season_plot, nrow = 2, heights = c(2, 4))

ggsave(filename = "viz/playoff_outcomes.jpg", width = 10.666, height = 21.333)
