### load required packages

library("tidyverse") 
library("rvest")


### scrape standings data from hockey reference


years <- setdiff(1993:2019, 2005)

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
  
  
  
  standings_page <- read_html(x[2])
  
  standings_table <- html_nodes(standings_page, "table")
  
  standings <- html_table(standings_table, fill = T, header = F) [[3]] %>%
    
    select(regular_season_rank = 1, team = 2, record = 3) %>%
    
    slice(-1) %>%
    
    mutate(regular_season_rank = as.numeric(regular_season_rank))
  
  
  standings <- 
    
    left_join(standings, playoffs, by = "team") %>%
    
    replace_na(list(playoffs = 0)) %>%
    
    arrange(-cup_winner, -playoff_exit_round, regular_season_rank) %>%
    
    mutate(season_final_rank = row_number(),
           season_final_rank_percent = season_final_rank/max(season_final_rank),
           regular_season_rank_percent = regular_season_rank/max(regular_season_rank))
  
  })

dflist1 <- Map(cbind, dflist, years)

dflist1 <- lapply(dflist1, setNames, nm = names(dflist1[[1]]))

standings <- do.call(rbind, dflist1) %>%
  
  rename(year = 10)



### filter standings data for one team

tbl <- standings %>%
  
  filter(team == "Tampa Bay Lightning") %>%
  
  arrange(-cup_winner, -playoff_exit_round, regular_season_rank_percent) %>%
  
  mutate(seed = row_number())


write.csv(tbl, "C:/Users/Alan/Desktop/lightning_bracket.csv", row.names = F)
