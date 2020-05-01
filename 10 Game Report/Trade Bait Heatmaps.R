### load dependencies

library(tidyverse)
library(ggthemes)
library(scales)
library(extrafont)


### load fonts for viz

loadfonts(device = "win")


### scrape trade bait list

url <- "https://www.tsn.ca/tsn-hockey-s-trade-bait-list-1.203546"

trade_bait <- read_html(url) %>%
  
  html_table() %>%
  
  data.frame(.) %>%
  
  unnest() %>%
  
  select(col = 2, pos = Pos) %>%
  
  separate(col, into = c("number", "player"), sep = "\\. ") %>%
  
  separate(player, into = c("player", "team"), sep = ", ") %>%
  
  mutate(pos = ifelse(grepl("D", pos), "D", "F"),
         player = gsub("Anthony Deangelo", "Tony Deangelo", player))


### filter skated data for only players on the trade bait chart

trade_bait_chart <- skaters.all %>%
  
  filter(Player %in% trade_bait$player) %>%
  
  mutate(Player = factor(Player, levels = trade_bait$player))


### filter for forwards only

forward_chart <- trade_bait_chart %>%
  
  filter(Position == "F")


### plot forward heatmap

ggplot(forward_chart, aes(x = Player, y = Verbose, fill = Z.Score)) +
  
  geom_tile() +
  
  facet_wrap(
    ~Group, ncol = 1, 
    strip.position = "left", 
    scales = "free_y", 
    labeller = label_wrap_gen(width = 3)) +
  
  scale_fill_gradient2(
    low = "orange", 
    mid = "gray93", 
    high = "dodgerblue4", 
    limits = c(-3,3), 
    midpoint = 0, 
    labels=c("Worst", "", "", "Avg", "", "", "Best"), 
    oob = squish, na.value = "black"
  ) +
  
  labs(caption = "data via evolving-hockey.com, chart by @loserpoints") +
  
  scale_x_discrete(position = "top") +
  
  theme_few() +
  
  theme(
    legend.title=element_blank(),
    axis.title = element_blank(),
    strip.placement = "outside", 
    strip.background = element_rect(fill = "gray36"), 
    strip.text = element_text(color = "white", face = "bold", size = 15, family = "Trebuchet MS"),
    axis.text.x = element_text(angle = 45, hjust = 0.05, face = "bold", size = 14, family = "Trebuchet MS"),
    axis.text.y = element_text(face = "bold", size = 14, family = "Trebuchet MS"),
    plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5), family = "Trebuchet MS"))


ggsave(paste0("Viz/trade_deadline_heatmap_forwards_", Sys.Date(), ".png"), width = 21.333, height = 10.667)




### filter for defense only

defense_chart <- trade_bait_chart %>%
  
  filter(Position == "D")


### plot defense heatmap

ggplot(defense_chart, aes(x = Player, y = Verbose, fill = Z.Score)) +
  
  geom_tile() +
  
  facet_wrap(
    ~Group, ncol = 1, 
    strip.position = "left", 
    scales = "free_y", 
    labeller = label_wrap_gen(width = 3)) +
  
  scale_fill_gradient2(
    low = "orange", 
    mid = "gray93", 
    high = "dodgerblue4", 
    limits = c(-3,3), 
    midpoint = 0, 
    labels=c("Worst", "", "", "Avg", "", "", "Best"), 
    oob = squish, na.value = "black"
  ) +
  
  labs(caption = "data via evolving-hockey.com, chart by @loserpoints") +
  
  scale_x_discrete(position = "top") +
  
  theme_few() +
  
  theme(
    legend.title=element_blank(),
    axis.title = element_blank(),
    strip.placement = "outside", 
    strip.background = element_rect(fill = "gray36"), 
    strip.text = element_text(color = "white", face = "bold", size = 15, family = "Trebuchet MS"),
    axis.text.x = element_text(angle = 45, hjust = 0.05, face = "bold", size = 14, family = "Trebuchet MS"),
    axis.text.y = element_text(face = "bold", size = 14, family = "Trebuchet MS"),
    plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5), family = "Trebuchet MS"))


ggsave(paste0("Viz/trade_deadline_heatmap_defense_", Sys.Date(), ".png"), width = 21.333, height = 10.667)

