###### load required packages

library(tidyverse)
library(rvest)
library(extrafont)
library(hrbrthemes)


###### set working directory

setwd("C:/Users/Alan/Documents/R/Projects/Hockey/Team Tiers")

###### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


####### load local evolving hockey data

skater_spar <- read_csv("data/skater_spar.csv")
goalie_spar <- read_csv("data/goalie_spar.csv")
team_spar <- read_csv("data/team_spar.csv")
standings_eh <- read_csv("data/standings.csv")


###### scrape team playoff outcomes from hockey reference

### generate URLs to scrape

years <- setdiff(2008:2020, 2005)

sites <-
  paste0("https://www.hockey-reference.com/playoffs/NHL_",
         years,
         ".html")


### scrape data and output a list of data frames

dflist <- lapply(sites, function(x) {
  
  playoffs_page <- read_html(x)
  
  playoffs_table <- html_nodes(playoffs_page, "table")
  
  playoffs <- html_table(playoffs_table, fill = T) [[1]] %>%
    
    select(round = 1, teams = 3) %>%
    
    filter(round == "Final" | 
             round == "Conference Finals" | 
             round == "Second Round" | 
             round == "First Round" |
             round == "Division Finals" |
             round == "Division Semi-Finals" |
             round == "Conference Semi-Finals" |
             round == "Conference Quarter-Finals") %>%
    
    separate(teams, into = c("winner", "loser"), sep = " over ") %>%
    
    pivot_longer(-round, names_to = "outcome", values_to = "team") %>%
    
    mutate(playoffs = 1,
           playoff_exit_round = ifelse(paste0(round, outcome) == "Finalwinner", NA, round),
           cup_winner = ifelse(paste0(round, outcome) == "Finalwinner", 1, 0)) %>%
    
    filter(is.na(playoff_exit_round) | outcome == "loser") %>%
    
    select(team, playoffs, playoff_exit_round, cup_winner) %>%
    
    mutate(playoff_exit_round = gsub("Conference Finals", 3, playoff_exit_round),
           playoff_exit_round = gsub("Division Finals", 2, playoff_exit_round),
           playoff_exit_round = gsub("Division Semi-Finals", 1, playoff_exit_round),
           playoff_exit_round = gsub("Conference Semi-Finals", 2, playoff_exit_round),
           playoff_exit_round = gsub("Conference Quarter-Finals", 1, playoff_exit_round),
           playoff_exit_round = gsub("Final", 4, playoff_exit_round),
           playoff_exit_round = gsub("Second Round", 2, playoff_exit_round),
           playoff_exit_round = gsub("First Round", 1, playoff_exit_round),
           playoff_exit_round = as.numeric(playoff_exit_round))
})


### combine list of seasonal playoff data frames into one

dflist1 <- Map(cbind, dflist, years)

dflist1 <- lapply(dflist1, setNames, nm = names(dflist1[[1]]))

playoffs <- do.call(rbind, dflist1) %>%
  
  rename(season = 5) %>%
  
  mutate(team_season = paste0(team, "_", season))


###### scrape free agency list from spotrac

free_agents_site <- read_html("https://www.spotrac.com/nhl/free-agents/")

free_agents_table <- html_nodes(free_agent_site, "table")

free_agents <- html_table(free_agents_table) [[1]] %>%
  
  select(player = 1, team = 2, status = 3) %>%
  
  separate(player, into = c("last", "first"), sep = " ", extra = "drop") %>%
  
  mutate(player = paste0(first, " ", last)) %>%
  
  select(-first, -last) %>%
  
  select(player, team, status)


###### calculate replacement level team point totals for all team seasons

skater_spar <- skater_spar %>%
  
  select(Team, Season, SPAR)

goalie_spar <- goalie_spar %>%
  
  select(Team, Season, SPAR)


roster_spar <- rbind(skater_spar, goalie_spar) %>%
  
  filter(Season != "12-13", Season != "19-20") %>%
  
  group_by(Team, Season) %>%
  
  summarize_all(sum) %>%
  
  left_join(., standings_eh %>% select(Name, Team, GP, Season, Points, Points_Percent = 11), by = c("Team", "Season")) %>%
  
  mutate(Replacement_Level = Points - SPAR)


###### join roster level spar and standings data with playoff data

standings <- roster_spar %>%
  
  mutate(season = as.numeric(paste0("20", substr(Season, 4, 5)))) %>%
  
  rename(team_long = Name, team_short = Team, points = Points, points_percent = Points_Percent, replacement_level = Replacement_Level) %>%
  
  mutate(team_season = paste0(team_long, "_", season)) %>%
  
  select(-season) %>%
  
  fuzzy_join(playoffs, ., by = c("team_season" = "team_season"), match_fun = str_detect) %>%
  
  select(team = team_short, season, GP, points, points_percent, playoffs, playoff_exit_round, replacement_level)


###### calculate relevant stats for plotting

roster_spar_mean <- mean(roster_spar$Replacement_Level)

roster_spar_sd <- sd(roster_spar$Replacement_Level)


playoff_mins <- standings %>%
  
  select(season, points) %>%
  
  group_by(season) %>%
  
  summarize_all(min)

playoff_min_mean <- mean(playoff_mins$points)

playoff_min_sd <- sd(playoff_mins$points)


contenders <- standings %>%
  
  filter(playoff_exit_round > 1)

contender_mean <- mean(contenders$points)  

contender_sd <- sd(contenders$points)  


###### get this season's standings for plotting

standings_current <- standings_eh %>%
  
  filter(Season == "19-20") %>%
  
  mutate(Points_Pace = 82/GP*Points) %>%
  
  arrange(Points_Pace) %>%
  
  mutate(Team = factor(Team, levels = Team))


###### plot team tiers according to this seasons' standings

ggplot(standings_current, aes(Team, Points_Pace)) +
  
  geom_bar(stat = "identity", fill = "gray81") +
  
  coord_flip() +
  
  geom_hline(yintercept = playoff_min_mean, color = "dodgerblue3", linetype = "dashed") +
  
  geom_hline(yintercept = contender_mean, color = "darkorange2", linetype = "dashed") +
  
  geom_text(data = standings_current %>% filter(Team == "T.B"), aes(x = 3, y = 96), label = "Playoff Cut-Off\n 91 points",  size = 4, family = "Trebuchet MS") +
  
  geom_text(data = standings_current %>% filter(Team == "T.B"), aes(x = 3, y = 108), label = "Contender Cut-Off\n 103 points",  size = 4, family = "Trebuchet MS") +
  
  ylab("\nPoints Pace") +
  
  xlab("") +
  
  theme_ipsum_ps() +
  
  ggtitle("Which NHL Teams Are On Pace to be Contenders?", 
          subtitle = "Cut-offs based on seasons since 07-08, data via Evolving Hockey") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = "viz/standings.png", width = 21.333, height = 10.666)
