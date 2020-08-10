library(tidyverse)

player_seasons_rapm_1 <- read_csv("data/ev_rapm.csv") %>%
  
  mutate(Position = ifelse(Position == "D", "D", "F")) %>%
  
  arrange(Season, Position, -TOI) %>%
  
  group_by(Season, Position) %>%
  
  mutate(N = row_number()) %>%
  
  ungroup() %>%
  
  mutate(Season = as.numeric(paste0("20", substr(Season, 4, 5))),
         Team_Count = ifelse(Season > 2017, 31, 30)) %>%
  
  filter(ifelse(Position == "D", N <= Team_Count*7, N <= Team_Count*13)) %>%
  
  select(player = Player, eh_id = EH_ID, nhl_api_id = 'API ID', season_1 = Season, team_1 = Team, position = Position, toi_ev_1 = TOI, xgf_1 = xGF, xga_1 = xGA)


player_seasons_rapm_2 <- player_seasons_rapm_1 %>%
  
  mutate(season_2 = season_1 - 1) %>%
  
  select(player, eh_id, nhl_api_id, season_2, team_2 = team_1, position, toi_ev_2 = toi_ev_1, xgf_2 = xgf_1, xga_2 = xga_1)


player_seasons_rapm <- 
  
  left_join(player_seasons_rapm_1, player_seasons_rapm_2, by = c("player" = "player", "eh_id" = "eh_id", "nhl_api_id" = "nhl_api_id", "position" = "position", "season_1" = "season_2")) %>%
  
  mutate(season_2 = season_1 + 1) %>%
  
  filter(season_1 != 2012,
         season_2 != 2012,
         !is.na(toi_ev_2)) %>%
  
  mutate(xgf_1_rate = xgf_1/toi_ev_1,
         xgf_2_rate = xgf_2/toi_ev_2,
         xga_1_rate = xga_1/toi_ev_1,
         xga_2_rate = xga_2/toi_ev_2) %>%
  
  select(player, eh_id, nhl_api_id, position, season_1, season_2, xgf_1_rate, xgf_2_rate, xga_1_rate, xga_2_rate)



ggplot(player_seasons_rapm, aes(xgf_1_rate, xgf_2_rate)) +
  
  facet_wrap(~position) +
  
  geom_point() +
  
  geom_smooth(method = "lm")




ggplot(player_seasons_rapm, aes(xga_1_rate, xga_2_rate)) +
  
  facet_wrap(~position) +
  
  geom_point() +
  
  geom_smooth(method = "lm")




player_seasons_rapm %>%
  
  group_by(position) %>%
  
  summarize(cor(xgf_1_rate, xgf_2_rate))



player_seasons_rapm %>%
  
  group_by(position) %>%
  
  summarize(cor(xga_1_rate, xga_2_rate))
