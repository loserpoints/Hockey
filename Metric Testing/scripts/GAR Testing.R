library(tidyverse)

player_seasons_spar_1 <- read_csv("data/spar.csv") %>%
  
  mutate(Position = ifelse(Position == "D", "D", "F")) %>%
  
  arrange(Season, Position, -TOI_EV) %>%
  
  group_by(Season, Position) %>%
  
  mutate(N = row_number()) %>%
  
  ungroup() %>%
  
  mutate(Season = as.numeric(paste0("20", substr(Season, 4, 5))),
         Team_Count = ifelse(Season > 2017, 31, 30)) %>%
  
  filter(ifelse(Position == "D", N <= Team_Count*7, N <= Team_Count*13)) %>%
  
  select(player = Player, eh_id = EH_ID, nhl_api_id = 'API ID', season_1 = Season, team_1 = Team, position = Position, toi_ev_1 = TOI_EV, o_gar_1 = Off_GAR, d_gar_1 = Def_GAR)


player_seasons_spar_2 <- player_seasons_spar_1 %>%
  
  mutate(season_2 = season_1 - 1) %>%
  
  select(player, eh_id, nhl_api_id, season_2, team_2 = team_1, position, toi_ev_2 = toi_ev_1, o_gar_2 = o_gar_1, d_gar_2 = d_gar_1)


player_seasons_spar <- 
  
  left_join(player_seasons_spar_1, player_seasons_spar_2, by = c("player" = "player", "eh_id" = "eh_id", "nhl_api_id" = "nhl_api_id", "position" = "position", "season_1" = "season_2")) %>%
  
  mutate(season_2 = season_1 + 1) %>%
  
  filter(season_1 != 2012,
         season_2 != 2012,
         !is.na(toi_ev_2)) %>%
  
  mutate(o_gar_1_rate = o_gar_1/toi_ev_1,
         o_gar_2_rate = o_gar_2/toi_ev_2,
         d_gar_1_rate = d_gar_1/toi_ev_1,
         d_gar_2_rate = d_gar_2/toi_ev_2) %>%
  
  select(player, eh_id, nhl_api_id, position, season_1, season_2, o_gar_1_rate, o_gar_2_rate, d_gar_1_rate, d_gar_2_rate)





ggplot(player_seasons_spar, aes(o_gar_1_rate, o_gar_2_rate)) +
  
  facet_wrap(~position) +
  
  geom_point() +
  
  geom_smooth(method = "lm")




ggplot(player_seasons_spar, aes(d_gar_1_rate, d_gar_2_rate)) +
  
  facet_wrap(~position) +
  
  geom_point() +
  
  geom_smooth(method = "lm")




player_seasons_spar %>%
  
  group_by(position) %>%
  
  summarize(cor(o_gar_1_rate, o_gar_2_rate))



player_seasons_spar %>%
  
  group_by(position) %>%
  
  summarize(cor(d_gar_1_rate, d_gar_2_rate))
