library(tidyverse)
library(extrafont)
library(fuzzyjoin)
library(hrbrthemes)


#### load fonts for viz

loadfonts(device = "win")


#### scrape standings data from hockey reference

### generate URLs to scrape

years <- setdiff(2008:2020, 2005)

playoffs_site <-
  paste0("https://www.hockey-reference.com/playoffs/NHL_",
         years,
         ".html")

standings_site <-
  paste0("https://www.hockey-reference.com/leagues/NHL_",
         years,
         "_standings.html")


sites <- data.frame(cbind(playoffs_site, standings_site)) %>%
  
  rename(p_site = 1, s_site = 2) %>%
  
  rowwise() %>%
  
  mutate(sites = list(c(as.character(p_site), as.character(s_site))))


sites <- sites$sites


### scrape data and output a list of data frames

dflist <- lapply(sites, function(x) {
  
  playoffs_page <- read_html(x[1])
  
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
    
    separate(teams, into = c("winner", "loser"), sep = " over | lead ")
  
})


### combine list of data frames into one

dflist1 <- Map(cbind, dflist, years)

dflist1 <- lapply(dflist1, setNames, nm = names(dflist1[[1]]))

playoff_series <- do.call(rbind, dflist1) %>%
  
  rename(season = 4)


### reformat data with each series as a single observation

playoff_series_winners <- playoff_series %>%
  
  rename(team = winner, opponent = loser) %>%
  
  mutate(outcome = "win")


playoff_series_losers <- playoff_series %>%
  
  rename(team = loser, opponent = winner) %>%
  
  mutate(outcome = "loss")


playoff_series <- rbind(playoff_series_winners, playoff_series_losers) %>%
  
  arrange(season, round, team) %>%
  
  mutate(team_season = paste0(team, "_", season))


### read local standings data from evolving hockey export and join with hockey ref playoff data

playoff_paths <- read_csv("data/EH_tm_standings_2020-09-10.csv") %>%
  
  mutate(year = as.numeric(paste0("20", substr(Season, 4, 5)))) %>%
  
  select(team = Name, year, points_percent = 11) %>%
  
  mutate(team_season = paste0(team, "_", year)) %>%
  
  fuzzy_join(playoff_series, ., by = c("team_season" = "team_season"), match_fun = str_detect) %>%
  
  select(season, round, team = 2, opponent, outcome, points_percent) %>%
  
  mutate(round = case_when(
    round == "Division Semi-Finals" ~ "First Round",
    round == "Conference Semi-Finals" ~ "Second Round",
    round == "Conference Quarter-Finals" ~ "First Round",
    round == "Final" ~ "Stanley Cup Final",
    TRUE ~ round),
    round = factor(round, levels = (c("First Round", "Second Round", "Conference Finals", "Stanley Cup Final")))
  )


### generate data frame with just losers in each round for plotting

playoff_losers <- playoff_paths %>%
  
  filter(outcome == "loss")


### generate dataframe with the means for each round for plotting

mean_lines <- playoff_paths %>%
  
  filter(outcome == "loss") %>%
  
  select(round, points_percent) %>%
  
  group_by(round) %>%
  
  summarize_all(mean)


### generate a dataframe with one team's single season path for plotting

team_path <- playoff_paths %>%
  
  filter(opponent == "Tampa Bay Lightning", season == 2020, outcome == "loss")


### plot a team's single year path

ggplot() +
  
  facet_wrap(~round, scales = "free_y", ncol = 1) +
  
  geom_density(data = playoff_losers, aes(points_percent), fill = "darkorange3", alpha = 0.25) +
  
  geom_vline(data = mean_lines, aes(xintercept = points_percent), linetype = "dashed", color = "gray36") +
  
  geom_vline(data = team_path, aes(xintercept = points_percent), linetype = "dashed", color = "dodgerblue3", size = 1.5) +
  
  theme_ipsum_ps() +
  
  ggtitle("Tampa Bay Lightning 2020 Playoff Path Difficulty", 
          subtitle = "Data via Evolving Hockey and Hockey Reference") +
  
  xlab("\nOpponent Points Percentage") +
  
  ylab("") +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 18),
        strip.background = element_rect(color = "gray36"))

ggsave(filename = "viz/lightning_2020.png", width = 10.666, height = 21.333)


#### generate a data frame with each team's single season total path difficulty

path_ranking <- playoff_paths %>%
  
  select(opponent, season, points_percent) %>%
  
  mutate(difficulty = scale(points_percent)) %>%
  
  select(-points_percent) %>%
  
  group_by(opponent, season) %>%
  
  summarize_all(sum)
  
  
