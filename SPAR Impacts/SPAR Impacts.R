### load required packages

library(tidyverse)
library(rvest)
library(extrafont)
library(hrbrthemes)


### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


### load war data csvs

skater_war <- read_csv("data/skater_war.csv")
goalie_war <- read_csv("data/goalie_war.csv")
team_war <- read_csv("data/team_war.csv")


### scrape year by year standings data from espn

years <- c(2003, 2004, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

site <- paste0("http://www.espn.com/nhl/standings/_/season/", years)

season_list <- lapply(site, function(i) {
  
  page <- read_html(i)
  
  standings_table <- html_nodes(page, "table")
  
  east1 <- html_table(standings_table, fill = T)[[1]]
  
  east2 <- html_table(standings_table, fill = T)[[2]]
  
  west1 <- html_table(standings_table, fill = T)[[3]]
  
  west2 <- html_table(standings_table, fill = T)[[4]]
  
  east <- cbind(east1, east2)
  
  west <- cbind(west1, west2)
  
  colnames(east) <- east[1, ]
  
  colnames(west) <- west[1, ]
  
  colnames(east) [1] <- "Team" 
  
  colnames(west) [1] <- "Team"
  
  standings <- rbind(east, west)
  
  
standings %>%
    
    mutate(Season = substr(i, 44, 47)) %>%
    
    select(Team, Season, PTS)

})


### combine standings data for all seasons into one table

standings <- do.call(bind_rows, season_list)


### reformat standings data and fix team names to match evolving hockey data

standings <- standings %>%
  
  mutate(PTS = as.numeric(PTS)) %>%
  
  filter(!is.na(PTS)) %>%
  
  separate(Team, into = c("Delete", "Team"), sep = "--") %>%
  
  mutate(Team = ifelse(is.na(Team), Delete, Team)) %>%
  
  select(-Delete) %>%
  
  mutate(Team = substr(Team, 1, 3)) %>%
  
  mutate(Team = gsub("TBT", "T.B", Team),
         Team = gsub("NJN", "N.J", Team),
         Team = gsub("VGS", "VGK", Team),
         Team = gsub("LAL", "L.A", Team),
         Team = gsub("PHX", "ARI", Team),
         Team = gsub("SJS", "S.J", Team)) %>%
  
  filter(Season >= 2008) %>%
  
  arrange(Season, -PTS) %>%
  
  group_by(Season) %>%
  
  mutate(PTS_Rank = min_rank(desc(PTS)))


### get spar data from evolving hockey

spar <- skater_war %>%
  
  select(Player, Team, Season, SPAR) %>%
  
  mutate(Season = paste0("20", substr(Season, 4, 5))) %>%
  
  left_join(., standings, by = c("Team", "Season")) %>%
  
  arrange(Season, -PTS) %>%
  
  mutate(Team_Without_Player = PTS - SPAR,
         Player_Team_Season = paste0(Player, "_", Team, "_", Season))


### get unique list of player seasons

player_team_season <- spar$Player_Team_Season


### for each player season, replace their team points with the hypothetical points if they were swapped with a replacement level player

df_list <- lapply(player_team_season, function(x) {
  
  player <- spar %>%
    
    filter(Player_Team_Season == x)
  
  
  hypo_standings <- spar %>%
    
    mutate(Hypo_PTS = ifelse(Team == player$Team, player$Team_Without_Player, PTS)) %>%
    
    select(Team, Season, Hypo_PTS) %>%
    
    distinct() %>%
    
    arrange(Season, -Hypo_PTS) %>%
    
    group_by(Season) %>%
    
    mutate(Hypo_PTS_Rank = min_rank(desc(Hypo_PTS)))
  
  df <- 
    
    left_join(player, hypo_standings, by = c("Team", "Season"))
    
    })


### combine all the new hypothetical player seasons back into one table

hypo_pts_ranks <- do.call(rbind, df_list) %>%
  
  select(Player_Team_Season, Hypo_PTS, Hypo_PTS_Rank)


### join the original spar data with the hypothetical data

spar <- 
  
  left_join(spar, hypo_pts_ranks) %>%
  
  mutate(PTS_Rank_Diff = abs(PTS_Rank - Hypo_PTS_Rank),
         PTS_Rank_Dir = ifelse(PTS_Rank > Hypo_PTS_Rank, "Worse", "Better"),
         Impact = case_when(PTS_Rank < 16 & Hypo_PTS_Rank > 16 ~ "Make Top 16",
                            PTS_Rank > 16 & Hypo_PTS_Rank < 16 ~ "Miss Top 16")) %>%
  
  replace_na(list(Impact = "No Change"))


### format data for plotting the best seasons

spar_best_plot <- spar %>%
  
  filter(PTS_Rank_Dir == "Better") %>%
  
  arrange(-PTS_Rank_Diff, Hypo_PTS_Rank) %>%

  mutate(n = row_number()) %>%
  
  filter(n <= 40) %>%
  
  arrange(PTS_Rank_Diff, Hypo_PTS_Rank) %>%
  
  mutate(Player_Team_Season = factor(Player_Team_Season, levels = Player_Team_Season))


### plot the best seasons

ggplot(spar_best_plot, aes(x = Player_Team_Season, xend = Player_Team_Season)) +
  
  geom_segment(aes(y = Hypo_PTS_Rank, yend = PTS_Rank, color = Impact), size = 2, lineend = "round", linejoin = "round", arrow = arrow(length = unit(0.2, "cm"))) +
  
  geom_label(aes(label = spar_best_plot$Player, y = (PTS_Rank + Hypo_PTS_Rank)/2), family = "IBM Plex Sans") +
  
  coord_flip() +
  
  ylab("\nStandings Rank") +
  
  xlab("") +
  
  scale_x_discrete(labels = paste0(spar_best_plot$Team, " ", spar_best_plot$Season)) +
  
  scale_color_manual(values = c("dodgerblue3", "gray45")) +
  
  theme_ipsum_ps() +
  
  ggtitle("Best Single Season Skater Impacts on Standings Rank", 
          subtitle = "Using Standings Points Above Replacement (SPAR) from Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "bottom")


ggsave(filename = "viz/spar_playoff_impact_best.jpg", height = 21.333, width = 10.666)



### format data for plotting the worst seasons

spar_worst_plot <- spar %>%
  
  filter(PTS_Rank_Dir == "Worse") %>%
  
  arrange(-PTS_Rank_Diff, -Hypo_PTS_Rank) %>%
  
  mutate(n = row_number()) %>%
  
  filter(n <= 40) %>%
  
  arrange(PTS_Rank_Diff, -Hypo_PTS_Rank) %>%
  
  mutate(Player_Team_Season = factor(Player_Team_Season, levels = Player_Team_Season))


### plot the worst seasons

ggplot(spar_worst_plot, aes(x = Player_Team_Season, xend = Player_Team_Season)) +
  
  geom_label(aes(label = spar_worst_plot$Player, y = ifelse(spar_worst_plot$PTS_Rank < 16, PTS_Rank, Hypo_PTS_Rank)), 
             hjust = ifelse(spar_worst_plot$PTS_Rank < 16, "left", "right"),
             nudge_y = ifelse(spar_worst_plot$PTS_Rank < 16, 0.2, -0.2),
             family = "IBM Plex Sans", ) +
  
  geom_segment(aes(y = Hypo_PTS_Rank, yend = PTS_Rank, color = Impact), size = 2, lineend = "round", linejoin = "round", arrow = arrow(length = unit(0.2, "cm"))) +
  
  coord_flip() +
  
  ylab("\nStandings Rank") +
  
  xlab("") +
  
  scale_x_discrete(labels = paste0(spar_worst_plot$Team, " ", spar_worst_plot$Season)) +
  
  scale_color_manual(values = c("darkorange3", "gray45")) +
  
  theme_ipsum_ps() +
  
  ggtitle("Worst Single Season Skater Impacts on Standings Rank", 
          subtitle = "Using Standings Points Above Replacement (SPAR) from Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "bottom")


ggsave(filename = "viz/spar_playoff_impact_worst.jpg", height = 21.333, width = 10.666)


### calculate replacement level team point totals for all team seasons

skater_spar <- skater_war %>%
  
  select(Team, Season, SPAR)

goalie_spar <- goalie_war %>%
  
  select(Team, Season, SPAR)


player_spar <- rbind(skater_spar, goalie_spar) %>%
  
  filter(Season != "12-13", Season != "19-20") %>%
  
  group_by(Team, Season) %>%
  
  summarize_all(sum) %>%
  
  mutate(Season = paste0("20", substr(Season, 4, 5))) %>%
  
  left_join(., standings %>% select(Team, Season, Points = PTS), by = c("Team", "Season")) %>%
  
  mutate(Replacement_Level = Points - SPAR)


distro_mean <- mean(team_season_spar$Replacement_Level)

distro_sd <- sd(team_season_spar$Replacement_Level)


### generate distribution of single team season replacement values

ggplot(team_season_spar, aes(x = Replacement_Level)) +
  
  geom_density(aes(y = ..count..), fill = "dodgerblue3", alpha = 0.6, color = "gray36") +
  
  geom_vline(xintercept = distro_mean + distro_sd, color = "gray45", linetype = "dashed", size = 1.5) +
  
  geom_vline(xintercept = distro_mean - distro_sd, color = "gray45", linetype = "dashed", size = 1.5) +
  
  geom_label(x = 70, y = 18.75, label = "Using individual team seasons as a guide, a reasonable range\nfor a replacement level team is 49 to 62 standing points", family = "IBM Plex Sans", size = 5, label.size = NA, label.padding = unit(1, "lines")) +
  
  ylab("") +
  
  xlab("\nStandings Points") +
  
  theme_ipsum_ps() +
  
  ggtitle("Using Team Seasons to Estimate a Range of Results for a Replacement Level Team", 
          subtitle = "Using Standings Points Above Replacement (SPAR) from Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5))


ggsave(filename = "viz/replacement_level_distro.png", height = 10.666, width = 21.333)


### calculate replacement level team point totals for all seasons

season_spar <- rbind(skater_spar, goalie_spar) %>%
  
  filter(Season != "12-13", Season != "19-20") %>%
  
  group_by(Team, Season) %>%
  
  summarize_all(sum) %>%
  
  mutate(Season = paste0("20", substr(Season, 4, 5))) %>%
  
  left_join(., standings %>% select(Team, Season, Points = PTS), by = c("Team", "Season")) %>%
  
  group_by(Season) %>%

  select(-Team) %>%
  
  summarize_all(sum) %>%
  
  mutate(Team_Count = ifelse(Season < 2018, 30, 31)) %>%
  
  mutate(Replacement_Level = (Points - SPAR)/Team_Count)


bar_mean <- mean(season_spar$Replacement_Level)


### generate bar plot fot season by season replacement level teams

ggplot(season_spar, aes(Season, Replacement_Level)) +
  
  geom_bar(stat = "identity", fill = "darkorange3", alpha = 0.6, width = 0.6) +
  
  geom_hline(yintercept = bar_mean, color = "gray45", linetype = "dashed", size = 1.5) +
  
  geom_label(x = 10, y = 58, label = "On average, a replacement level team would post 55 points", family = "IBM Plex Sans", size = 5, label.size = NA, label.padding = unit(1, "lines")) +
  
  ylab("Standings Points\n") +
  
  xlab("") +
  
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50)) +
  
  theme_ipsum_ps() +
  
  ggtitle("How Good Would An Entire NHL Team of Replacement Level Players Be?", 
          subtitle = "Using Standings Points Above Replacement (SPAR) from Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5))

ggsave(filename = "viz/season_spar.jpg", width = 21.333, height = 10.666)


### calculate 16th place cut off by season, range, and average

cutoff <- standings %>%
  
  filter(Season != 2020, Season != 2013) %>%
  
  filter(PTS_Rank == 16)


cutoff_mean <- mean(cutoff$PTS)
  
cutoff_max <- max(cutoff$PTS)

cutoff_min <- min(cutoff$PTS)
