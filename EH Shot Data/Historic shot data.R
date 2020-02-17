### load required packages

library(RMariaDB)
library(tidyverse)


### read downloaded historic shot data from local

pbp0708 <- read_csv("shots_0708.csv")
pbp0809 <- read_csv("shots_0809.csv")
pbp0910 <- read_csv("shots_0910.csv")
pbp1011 <- read_csv("shots_1011.csv")
pbp1112 <- read_csv("shots_1112.csv")
pbp1213 <- read_csv("shots_1213.csv")
pbp1314 <- read_csv("shots_1314.csv")
pbp1415 <- read_csv("shots_1415.csv")
pbp1516 <- read_csv("shots_1516.csv")
pbp1617 <- read_csv("shots_1617.csv")
pbp1718 <- read_csv("shots_1718.csv")
pbp1819 <- read_csv("shots_1819.csv")


### combine historic data

pbp_historic <-
  rbind(
    pbp0708,
    pbp0809,
    pbp0910,
    pbp1011,
    pbp1112,
    pbp1213,
    pbp1314,
    pbp1415,
    pbp1516,
    pbp1617,
    pbp1718,
    pbp1819
  )


### add score and venue adjustments to historic pbp data

score_state <- pbp_historic %>%
  
  mutate(event_id = row_number()) %>%
  
  select(event_id, home_score, away_score) %>%
  
  mutate(home_leading = ifelse(home_score > away_score, "leading", NA),
         home_tied = ifelse(home_score == away_score, "tied", NA),
         home_trailing = ifelse(home_score < away_score, "trailing", NA),
         away_leading = ifelse(home_score < away_score, "leading", NA),
         away_tied = ifelse(home_score == away_score, "tied", NA),
         away_trailing = ifelse(home_score > away_score, "trailing", NA)) %>%
  
  select(-home_score, -away_score) %>%
  
  pivot_longer(-c(event_id, away_leading, away_tied, away_trailing), names_to = "measure", values_to = "home_score_state") %>%
  
  filter(!is.na(home_score_state)) %>%
  
  select(-measure) %>%
  
  pivot_longer(-c(event_id, home_score_state), names_to = "measure", values_to = "away_score_state") %>%
  
  filter(!is.na(away_score_state)) %>%
  
  select(-measure)


### load score and venue adjustments from mysql

shots_db <-
  
  dbConnect(
    MariaDB(),
    user = "root",
    password = password,
    dbname = "nhl_shots_eh",
    host = "localhost"
  )

adj_query <- 
  "SELECT * FROM score_venue_adjustment"

adj_db <- dbSendQuery(shots_db, adj_query)

adj <- dbFetch(adj_db)


### bind score states to pbp data and join with score and venue adjustments

pbp_historic <- cbind(pbp_historic, score_state) %>%
  
  mutate(score_state = ifelse(event_team == home_team, home_score_state, away_score_state)) %>%
  
  left_join(adj, by = c("game_strength_state" = "strength", "score_state" = "score_state"))


### push historic shot data to mysql

dbWriteTable(
  shots_db, 
  value = pbp_historic, 
  row.names = FALSE, 
  name = "shots", 
  append = TRUE 
  )
