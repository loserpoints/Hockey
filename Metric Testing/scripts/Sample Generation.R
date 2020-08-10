library(tidyverse)

player_seasons_1 <- read_csv("data/player_seasons.csv") %>%
  
  mutate(Position = ifelse(Position == "D", "D", "F")) %>%
  
  arrange(Season, Position, -TOI) %>%
  
  group_by(Season, Position) %>%
  
  mutate(N = row_number()) %>%
  
  ungroup() %>%
  
  mutate(Season = as.numeric(paste0("20", substr(Season, 4, 5))),
         Team_Count = ifelse(Season > 2017, 31, 30)) %>%
  
  filter(ifelse(Position == "D", N <= Team_Count*7, N <= Team_Count*13),
         Age > 21,
         Age < 29,
         nchar(Team) < 4) %>%
  
  select(player = Player, eh_id = EH_ID, nhl_api_id = 'API ID', season_1 = Season, team_1 = Team, position = Position, age_1 = Age, gp_1 = GP, toi_1 = TOI)


player_seasons_2 <- player_seasons_1 %>%
  
  mutate(season_2 = season_1 - 1) %>%
  
  select(player, eh_id, nhl_api_id, season_2, team_2 = team_1, position, age_2 = age_1, gp_2 = gp_1, toi_2 = toi_1)


player_seasons <- 
  
  left_join(player_seasons_1, player_seasons_2, by = c("player" = "player", "eh_id" = "eh_id", "nhl_api_id" = "nhl_api_id", "position" = "position", "season_1" = "season_2")) %>%
  
  mutate(season_2 = season_1 + 1) %>%
  
  filter(team_1 != team_2,
         season_1 != 2012,
         season_2 != 2012,
         gp_2 >= gp_1 - 25,
         gp_2 <= gp_1 + 25)
