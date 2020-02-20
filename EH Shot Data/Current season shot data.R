library(tidyverse)
library(RMariaDB)

### read current season data from local

pbp_1920 <- read_csv("shots_1920.csv")

### add score and venue adjustments to data set

score_state <- pbp_1920 %>%
  
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

pbp_1920 <- cbind(pbp_1920, score_state) %>%
  
  mutate(score_state = ifelse(event_team == home_team, home_score_state, away_score_state)) %>%
  
  left_join(adj, by = c("game_strength_state" = "strength", "score_state" = "score_state"))


### delete previous data from this season

filter_query <- 
  "DELETE FROM shots WHERE season = 20192020"

dbSendQuery(shots_db, filter_query)


### push current shot data to mysql

dbWriteTable(
  shots_db, 
  value = pbp_1920, 
  row.names = FALSE, 
  name = "shots", 
  append = TRUE 
)

file.remove(shots_1920)