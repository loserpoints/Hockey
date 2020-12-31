###### load required packages

library(tidyverse)
library(rvest)
library(extrafont)
library(hrbrthemes)
library(scales)


#### load fonts for viz

loadfonts(device = "win")


### load data from local

reader_votes <- read_csv("data/reader_votes.csv", skip = 1) %>%
  
  select("user_id" = 1, 10:55)


writer_votes <- read_csv("data/writer_votes.csv") %>%
  
  select("player" = 1, 8:17)


#compile reader totals

reader_totals <- reader_votes %>%
  
  pivot_longer(-user_id, names_to = "player", values_to = "rank") %>%
  
  filter(rank > 0, rank < 26) %>%
  
  mutate(score = 25 - (rank - 1)) %>%
  
  select(player, score) %>%
  
  group_by(player) %>%
  
  summarize_all(sum, na.rm = T) %>%
  
  arrange(-score) %>%
  
  top_n(25) %>%
  
  mutate(reader_rank = row_number())


### compile writer totals

writer_totals <- writer_votes %>%
  
  mutate_at(c(2:11), as.numeric) %>%
  
  pivot_longer(-player, names_to = "voter", values_to = "rank") %>%
  
  filter(rank > 0, rank < 26) %>%
  
  mutate(score = 25 - (rank - 1)) %>%
  
  select(player, score) %>%
  
  group_by(player) %>%
  
  summarize_all(sum, na.rm = T) %>%
  
  arrange(-score) %>%
  
  top_n(25) %>%
  
  mutate(reader_rank = row_number())


### compile total scores

community_votes_writer <- writer_votes %>%
  
  mutate_at(c(2:11), as.numeric) %>%
  
  pivot_longer(-player, names_to = "voter", values_to = "rank") %>%
  
  pivot_wider(names_from = player, values_from = rank) %>%
  
  select(-voter) %>%
  
  mutate(voter_type = "writer")


community_votes_reader <- reader_votes %>%
  
  select(2:47) %>%
  
  mutate(voter_type = "reader") %>%
  
  rename("Peter Abbandonato" = "Peter Abbadonato")


community_votes <- bind_rows(community_votes_writer, community_votes_reader) %>%
  
  pivot_longer(-voter_type, names_to = "player", values_to = "rank") %>%
  
  filter(rank > 0, rank < 26) %>%
  
  mutate(raw_score = 25 - (rank - 1)) %>%
  
  mutate(weighted_score = ifelse(voter_type == "writer", raw_score*0.75, raw_score*9/39/4),
         voter_type = gsub("writer", "Writer", voter_type),
         voter_type = gsub("reader", "Reader", voter_type),
         voter_type = factor(voter_type, levels = c("Writer", "Reader")))


community_votes_total <- community_votes %>%
  
  select(player, weighted_score) %>%
  
  group_by(player) %>%
  
  summarize_all(sum) %>%
  
  arrange(-weighted_score) %>%
  
  top_n(25) %>%
  
  mutate(rank = row_number()) %>%
  
  ungroup()


#integer break function
integer_breaks <- function(n = 5, ...) {
  
  breaker <- pretty_breaks(n, ...)
  
  function(x) {
    
    breaks <- breaker(x)
    
    breaks[breaks == floor(breaks)]
  
    }

  }


### get list of players for viz

players <- community_votes_total %>%
  
  select(player) %>%
  
  distinct()


players <- players$player


### generate  and viz for all players

generate_viz <- lapply(players, function(x) {
  
  community_votes <- community_votes %>%
    
    filter(player == x)
  
  
  title_player = x
  
  
  title_rank <- community_votes %>%
    
    select(rank) %>%
    
    distinct()
  
  title_rank <- title_rank$rank
  
  
  ggplot(community_votes, aes(rank)) +
    
    facet_wrap(~voter_type, nrow = 2, scales = "free_y") +
    
    geom_histogram(fill = "dodgerblue3", color = "white", binwidth = 1) +
    
    ylab("Count\n") +
    
    xlab("\nRanking") +
    
    scale_x_continuous(breaks = integer_breaks(), limits = c(0, 26)) +
    
    theme_ipsum_ps() +
    
    ggtitle(
      paste0("2020 Raw Charge T25U25 Community Vote - ", title_player, " (", title_rank,")"),
      subtitle = "") +
    
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 18),
      axis.title.x = element_text(size = 18, hjust = 0.5),
      axis.title.y = element_text(size = 18, hjust = 0.5),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_line(colour = "grey90"),
      strip.text = element_text(hjust = 0.5, size = 18),
      legend.position = "none"
    )
  
  ggsave(filename = paste0("viz/", x, ".jpg"), width = 21.333, height = 10.666)
  
})