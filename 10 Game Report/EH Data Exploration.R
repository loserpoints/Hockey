library(RMariaDB)
library(tidyverse)
library(rvest)
library(zoo)

### get toi data from evolving hockey

site_5v5 <- "https://evolving-hockey.com/?_inputs_&std_tm_adj=%22Score%20%26%20Venue%22&std_tm_span=%22Regular%22&std_tm_season=%2220192020%22&std_tm_type=%22Rates%22&dir_ttbl=%22Stats%22&dir=%22Team%20Tables%22&std_tm_table=%22On-Ice%22&std_tm_team=%22All%22&std_tm_range=%22Seasons%22&std_tm_str=%225v5%22&std_tm_group=%22Season%22"

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
  
  "SELECT * FROM shots"


shots_table <- dbSendQuery(shots_db, shots_query)


### select relevant data and add shot metrics

season_data <- dbFetch(shots_table) %>%
  
  filter(season != 20192020) %>%
  
  select(
    season,
    event_team,
    game_date,
    game_id,
    event_type,
    home_team,
    away_team,
    game_strength_state,
    pred_goal,
    pred_goal_home_weight,
    pred_goal_away_weight
  ) %>%
  
  mutate(
    xG = ifelse(
      event_team == home_team,
      pred_goal * pred_goal_home_weight,
      pred_goal * pred_goal_away_weight
    ),
    corsi = 1,
    fenwick = ifelse(event_type != "BLOCK", 1, 0),
    goal = ifelse(event_type == "GOAL", 1, 0)
  )

### split into home and away data frames to calculate for and against stats

home_data <- season_data %>%
  
  filter(
    event_team == home_team,
    game_strength_state == "5v5" | game_strength_state == "5v4" | game_strength_state == "4v5"
    ) %>%
  
  select(
    season,
    team = event_team,
    game_date,
    game_id,
    strength = game_strength_state,
    xGF = xG,
    CF = corsi,
    FF = fenwick,
    GF = goal
  ) %>%
  
  group_by(season, team, game_date, game_id, strength) %>%
  
  summarize_all(sum, na.rm = T) %>%
  
  ungroup()


away_data <- season_data %>%
  
  filter(
    event_team == away_team,
    game_strength_state == "5v5" |
      game_strength_state == "5v4" | game_strength_state == "4v5"
  ) %>%
  
  select(
    season,
    opp = event_team,
    game_date,
    game_id,
    strength = game_strength_state,
    xGA = xG,
    CA = corsi,
    FA = fenwick,
    GA = goal
  ) %>%
  
  group_by(season, opp, game_date, game_id, strength) %>%
  
  summarize_all(sum, na.rm = T) %>%
  
  ungroup()


season_data <-
  
  inner_join(home_data, away_data, by = c("season", "game_date", "game_id", "strength"))


### create duplicate table but reverse the direction of the metrics and strength states to swap teams

season_data_2 <- season_data %>%
  
  mutate(team = opp,
         strength = ifelse(
           strength == "5v4",
           "4v5",
           ifelse(strength == "4v5", "5v4", strength)),
         xGF1 = xGF,
         CF1 = CF,
         FF1 = FF,
         GF1 = GF,
         xGF = xGA,
         CF = CA,
         FF = FA,
         GF = GA,
         xGA = xGF1,
         CA = CF1,
         FA = FF1,
         GA = GF1) %>%
  
  select(-xGF1,
         -CF1,
         -FF1,
         -GF1
         )


gbg_data <- rbind(season_data, season_data_2) %>%
  
  select(-opp, -game_id) %>%
  
  arrange(team, game_date) %>%
  
  group_by(team, strength) %>%
  
  mutate(game_number = row_number()) %>%
  
  group_by(season, team, game_date) %>%
  
  mutate(game_number = max(game_number)) %>% 
  
  select(-strength) %>%
  
  select(season, team, game_date, xGF, xGA, GF, GA) %>%
  
  group_by(season, team, game_date) %>%
  
  summarize_all(sum) %>%
  
  group_by(season, team) %>%
  
  mutate(game_number = row_number()) %>%
  
  filter(max(game_number) == 82) %>%
  
  mutate_at(vars(xGF:GA), .funs = list(before = ~rollapply(., sum, align = "right", width = game_number))) %>%
  
  mutate_at(vars(xGF:GA), .funs = list(after = ~rollapply(., sum, align = "left", width = 82 - game_number))) %>%
  
  mutate(xG_share_before = xGF_before/(xGF_before + xGA_before),
         xG_share_after = xGF_after/(xGF_after + xGA_after),
         goal_share_before = GF_before/(GF_before + GA_before),
         goal_share_after = GF_after/(GF_after + GA_after)) %>%
  
  group_by(game_number) %>%
  
  mutate(xG_correlation = cor(xG_share_before, xG_share_after)^2,
         goal_correlation = cor(goal_share_before, goal_share_after)^2) %>%
  
  select(game_number, xG_correlation, goal_correlation) %>%
  
  distinct() %>%
  
  mutate(ratio = xG_correlation/goal_correlation)
  
  

























group_by(season, team) %>%
  
  mutate_at(vars(xGF:GA), .funs =list(total = ~cumsum(.))) %>%
  
  mutate(goal_share_total = GF_total/(GF_total + GA_total),
         xG_share_total = xGF_total/(xGF_total + xGA_total)) %>%
  
  select(season, team, game_date, goal_share_total, xG_share_total) %>%
  
  group_by(season, team, game_date) %>%
  
  mutate(n = row_number()) %>%
  
  filter(n == max(n)) %>%
  
  select(-n) %>%
  
  group_by(season, team) %>%
  
  mutate(game_number = row_number()) %>%
  
  filter(max(game_number) == 82) %>%
  
  mutate(final_goal_share = last(goal_share_total)) %>%
  
  group_by(game_number) %>%
  
  mutate(xG_correlation = cor(xG_share_total, final_goal_share),
         goal_correlation = cor(goal_share_total, final_goal_share)) %>%
  
  select(game_number, xG_correlation,goal_correlation) %>%
  
  distinct()
         
            