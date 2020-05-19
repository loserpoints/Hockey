### load required packages

library("tidyverse") 
library("rvest")
library("mgcv")
library("scales")
library("extrafont")
library("hrbrthemes")


### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


### load local data

skater_spar <- read_csv("data/skater_spar.csv")

skater_draft <- read_csv("data/skater_draft.csv") %>%
  
  select(EH_ID, draft_year = 'Draft Yr', draft_slot = 'Draft Ov') %>%
  
  distinct()


### scrape wiki table with draft length

draft_table <- read_html("https://en.wikipedia.org/wiki/NHL_Entry_Draft") %>%
  
  html_nodes(., "table") %>%
  
  html_table(., fill = T) %>%
  
  .[[4]]


### join skater spar with draft data and filter for approximate prime years only

draft_spar_prime <- 
  
  left_join(skater_spar, skater_draft, by = c("EH_ID" = "EH_ID")) %>%
  
  filter(!is.na(draft_slot),
         Age > 22, 
         Age < 29,
         GP > 59) %>%
  
  select(player = Player, draft_year, draft_slot, season = Season, team = Team, position = Position, age = Age, spar = SPAR) %>%
  
  mutate(draft_slot = as.numeric(draft_slot))


### count number of player seasons for first overall picks

first_overall_seasons <- draft_spar_prime %>%
  
  filter(draft_slot == 1)


first_overall_seasons <- length(first_overall_seasons$player)


### set the number of draft slots that are included in the plot

draft_slots <- 1:217


### definte function to add dummy rows to account for picks that never played in the NHL

draft_slot_table_list <- lapply(draft_slots, function(x) {
  
  slot_table <- draft_spar_prime %>%
    
    filter(draft_slot == x)
  
  slot_count <- length(slot_table$player) 
  
  add_rows <- ifelse(first_overall_seasons - slot_count > 0, first_overall_seasons - slot_count, 0)
  
  slot_table <- slot_table %>%
    
    add_row(player = 0:add_rows, draft_slot = x, spar = 0)
  
})
  

### recreate draft plot data frame including dummy data and add medians and +- 2 sd values

draft_slot_spar <- do.call(rbind, draft_slot_table_list) %>%
  
  filter(player != 0) %>%
  
  group_by(draft_slot) %>%
  
  mutate(median_spar = median(spar),
         sd = sd(spar),
         upper_spar = median_spar + 2* sd(spar),
         lower_spar = median_spar - 2* sd(spar)) %>%
  
  ungroup()


### create models for smoothing

median_model <- gam(median_spar ~ s(draft_slot, bs = "cs"), data = draft_slot_spar)
upper_model <- gam(upper_spar ~ s(draft_slot, bs = "cs"), data = draft_slot_spar)
lower_model <- gam(lower_spar ~ s(draft_slot, bs = "cs"), data = draft_slot_spar)


### add smoothed median, upper, and lower bounds to data frame

draft_slot_spar <- draft_slot_spar %>%
  
  mutate(smoothed_median = predict.gam(median_model),
         smoothed_upper = predict.gam(upper_model),
         smoothed_lower = predict.gam(lower_model)) 

  
### generate data frame with player prime spar means 

player_spar_means <- draft_slot_spar %>%
  
  select(player, draft_slot, spar) %>%
  
  add_count(player, name = "season_count") %>%
  
  filter(nchar(player) > 3,
         season_count > 1) %>%
  
  group_by(player, draft_slot, season_count) %>%
  
  summarize_all(mean) %>%
  
  ungroup()


### generate data frame with spar by draft slot

draft_slot_spar_summary <- draft_slot_spar %>%
  
  select(draft_slot, smoothed_median, smoothed_upper, smoothed_lower) %>%
  
  distinct()


### define function to identify players who best represent the range of performance at each draft slot

draft_slot_example_list <- lapply(draft_slots, function(x) {
  
  draft_slot <- draft_slot_spar_summary %>%
    
    filter(draft_slot == x) %>%
    
    rename(draft_slot_comp = draft_slot) %>%
    
    slice(rep(1:n(), each = nrow(player_spar_means)))
  
  
  player_example <- cbind(player_spar_means, draft_slot) %>%
    
    mutate(smoothed_lower_diff = abs(smoothed_lower - spar),
           smoothed_median_diff = abs(smoothed_median - spar),
           smoothed_upper_diff = abs(smoothed_upper - spar),
           slot_diff = abs(draft_slot_comp - draft_slot),
           lower_match = rescale(smoothed_lower_diff, to = c(0,1)),
           median_match = rescale(smoothed_median_diff, to = c(0,1)),
           upper_match = rescale(smoothed_upper_diff, to = c(0,1)),
           slot_match = rescale(slot_diff, to = c(0,3)),
           combined_lower_match = lower_match + slot_match,
           combined_median_match = median_match + slot_match,
           combined_upper_match = upper_match + slot_match)
  
  
  lower <- player_example %>%
    
    filter(combined_lower_match == min(combined_lower_match)) %>%
    
    filter(season_count == max(season_count)) %>%
    
    slice(1) %>%
    
    select(player, spar = smoothed_lower) %>%
    
    mutate(metric = "lower_spar",
           draft_slot = x)
  
  
  median <- player_example %>%
    
    filter(combined_median_match == min(combined_median_match)) %>%
    
    filter(season_count == max(season_count)) %>%
    
    slice(1) %>%
    
    select(player, spar = smoothed_median) %>%
    
    mutate(metric = "median_spar",
           draft_slot = x)
  
  
  upper <- player_example %>%
    
    filter(combined_upper_match == min(combined_upper_match)) %>%
    
    filter(season_count == max(season_count)) %>%
    
    slice(1) %>%
    
    select(player, spar = smoothed_upper) %>%
    
    mutate(metric = "upper_spar",
           draft_slot = x)
  
  
  slot_guide <- rbind(lower, median, upper)
  
})


draft_slot_examples <- do.call(rbind, draft_slot_example_list)
  

### plot raw spar values by draft slot with gam smoothing

ggplot(draft_slot_spar, aes(x = draft_slot)) +
  
  geom_point(aes(y = spar), alpha = 0.1) +
  
  geom_line(aes(y = smoothed_upper, color = "High end"), size = 1.5) +
  
  geom_line(aes(y = smoothed_median, color = "Median"), size = 2) +
  
  geom_line(aes(y = smoothed_lower, color = "Low end"), size = 1.5) +
  
  ylab("Standings Points Above Replacement\n") +
  
  xlab("\nDraft Slot") +
  
  theme_ipsum_ps() +
  
  ggtitle("Typical NHL Skater Season Standings Points Above Replacement by Draft Slot", 
          subtitle = "Data via Evolving Hockey") +
  
  scale_color_manual(values = c("dodgerblue3", "darkorange2", "gray36")) +
  
  scale_x_continuous(breaks = seq(0, 217, 31)) +
  
  scale_y_continuous(breaks = seq(-4, 10, 2)) +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

ggsave(filename = "viz/draft_slot_spar_curves.jpg", width = 21.333, height = 10.666)


### generate violin plot to show typical performance bt draft slot

ggplot(draft_slot_spar %>% filter(draft_slot < 31), aes(x = draft_slot, y = spar)) +
  
  facet_wrap(~draft_slot, scales = "free_x", nrow = 3) +
  
  geom_violin(linetype = "blank", fill = "gray81") +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_label(data = draft_slot_examples %>% filter(metric == "lower_spar", draft_slot < 31), aes(x = draft_slot, y = spar, label = player), color = "gray36", size = 4) +
  
  geom_label(data = draft_slot_examples %>% filter(metric == "median_spar", draft_slot < 31), aes(x = draft_slot, y = spar, label = player), size = 4.5) +
  
  geom_label(data = draft_slot_examples %>% filter(metric == "upper_spar", draft_slot < 31), aes(x = draft_slot, y = spar, label = player), color = "gray36", size = 4) +
  
  ylab("Standings Points Above Replacement\n") +
  
  xlab("") +
  
  theme_ipsum_ps() +
  
  ggtitle("Historical Value of the Top 30 Picks in the NHL Draft", 
          subtitle = "Players shown are guides and don't reflect the player's actual draft slot. For example, among players selected first overall, the median prime performace has been similar to Ilya Kovalchuk,\nthe high end outcome has been similar to Nathan MacKinnon, and the low end outcome has been similar to Alex Galchenyuk.") +
  
  theme(plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 14))

ggsave(filename = "viz/draft_slot_spar.jpg", width = 21.333, height = 10.666)