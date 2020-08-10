### load dependencies

library(tidyverse)
library(extrafont)
library(hrbrthemes)


### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


### read data from local and format

final_ranks <- read_csv("data/2020 T25 Community Vote - Tabulation.csv") %>%
  
  select(player = 1, final_rank = 30)


votes <- read_csv("data/2020 T25 Community Vote - Raw Vote Data.csv") %>%
  
  select(player = 1, 2:40) %>%
  
  pivot_longer(-player, names_to = "rank", values_to = "count") %>%
  
  mutate(rank = as.numeric(rank)) %>%
  
  left_join(final_ranks) %>%
  
  mutate(bar_alpha = ifelse(rank == final_rank, 1, 0.4),
         label = ifelse(rank == final_rank, final_rank, NA))


### get list of players for viz

players <- votes %>%
  
  filter(final_rank < 26) %>%
  
  select(player) %>%
  
  distinct()


players <- players$player



### generate  and viz for all players

generate_viz <- lapply(players, function(x) {
  
  votes <- votes %>%
    
    filter(player == x)
  
  
  title_player = x
  
  
  title_rank <- votes %>%
    
    select(final_rank) %>%
    
    distinct()
  
  title_rank <- title_rank$final_rank
  
  
  ggplot(votes, aes(rank, count)) +
    
    geom_bar(stat = "identity", fill = "#00205b", color = "white", alpha = votes$bar_alpha) +
    
    ylab("Count\n") +
    
    xlab("\nRanking") +
    
    theme_ipsum_ps() +
    
    ggtitle(
      paste0("2020 PPP T25U25 Community Vote - ", title_player, " (", title_rank,")"),
      subtitle = "") +
    
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 16),
      axis.title.x = element_text(size = 18, hjust = 0.5),
      axis.title.y = element_text(size = 18, hjust = 0.5),
      panel.grid.major = element_line(colour = "grey90"),
      panel.grid.minor = element_line(colour = "grey90"),
      legend.position = "none"
    )
  
  ggsave(filename = paste0("viz/", x, "_histo.jpg"), width = 21.333, height = 10.666)
  
})


### format data for faceted plot

facet_plot <- votes %>%
  
  filter(final_rank < 26) %>%
  
  arrange(final_rank) %>%
  
  mutate(player = factor(player, levels = unique(player)))


### generate faceted plot

ggplot(facet_plot, aes(rank, count)) +
  
  facet_wrap(~player, scales = "free_y") +
  
  geom_bar(stat = "identity", fill = "#00205b", color = "white", alpha = facet_plot$bar_alpha) +
  
  ylab("Count\n") +
  
  xlab("\nRanking") +
  
  theme_ipsum_ps() +
  
  ggtitle("2020 PPP T25U25 Community Vote",
    subtitle = "") +
  
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 18, hjust = 0.5),
    axis.title.y = element_text(size = 18, hjust = 0.5),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey90"),
    strip.text = element_text(hjust = 0.5, size = 14),
    strip.background = element_rect(color = "gray36"),
    legend.position = "none"
  )

ggsave(filename = "viz/ranking_facets.jpg", width = 21.333, height = 10.666)
