library(tidyverse)
library(scales)
library(extrafont)
library(hrbrthemes)


### load fonts for viz

loadfonts(device = "win")

import_plex_sans()


### read data from local

skater_spar <- read_csv("data/skater_spar.csv") %>%
  
  select(-Team) %>%
  
  group_by(Player, EH_ID, Season, Position, Shoots, Birthday, Age) %>%
  
  summarize_all(sum)


ev_rapm <- read_csv("data/ev_rapm.csv") %>%
  
  select(1:14, 18:23) %>% 
  
  mutate(Strength = "EV")


pp_rapm <- read_csv("data/pp_rapm.csv") %>%
  
  mutate(Strength = "PP")


sh_rapm <- read_csv("data/sh_rapm.csv") %>%
  
  mutate(Strength = "SH")


calder_names <- read_csv("data/calder.csv") %>%
  
  select(Player = 1)


ev_zones <- read_csv("data/ev_zones.csv") %>%
  
  mutate(Strength = "EV")


sh_zones <- read_csv("data/sh_zones.csv") %>%
  
  mutate(Strength = "SH")


### combine all rapm to one dataframe

skater_rapm <- rbind(ev_rapm, pp_rapm, sh_rapm)


### combine usage data into one dataframe

zones <- rbind(ev_zones, sh_zones) %>%
  
  select(-GP, -TOI)


### format spar data for hart trophy

hart_spar <- skater_spar %>%
  
  filter(Season == "17-18" | Season == "18-19" | Season == "19-20") %>%
  
  group_by(Player, Position) %>%
  
  mutate(Three_Year_SPAR = sum(SPAR)) %>%
  
  ungroup() %>%
  
  filter(Season == "19-20") %>%
  
  mutate(This_Year_Diff = SPAR - max(SPAR),
         Three_Year_Diff = Three_Year_SPAR - max(Three_Year_SPAR))


hart_rapm <- skater_rapm %>%
  
  filter(Season == "17-18" | Season == "18-19" | Season == "19-20") %>%
  
  mutate(xG_Diff = xGF - xGA,
         G_Diff = GF - GA) %>%
  
  group_by(Player, Position) %>%
  
  mutate(Three_Year_xG = sum(xG_Diff),
         Three_Year_G = sum(G_Diff)) %>%
  
  ungroup() %>%
  
  filter(Season == "19-20") %>%
  
  group_by(Player, Position) %>%
  
  mutate(This_Year_xG = sum(xG_Diff),
         This_Year_G = sum(G_Diff),
         Total_TOI = sum(TOI),
         GP = max(GP)) %>%
  
  ungroup() %>%
  
  select(Player:GP, Three_Year_xG:Total_TOI) %>%
  
  distinct() %>%
  
  mutate(This_Year_xG_Gap = This_Year_xG - max(This_Year_xG),
         Three_Year_xG_Gap = Three_Year_xG - max(Three_Year_xG),
         This_Year_G_Gap = This_Year_G - max(This_Year_G),
         Three_Year_G_Gap = Three_Year_G - max(Three_Year_G))



hart <-
  
  left_join(hart_spar, hart_rapm) %>%
  
  mutate_at(
    vars(
      This_Year_Diff, 
      Three_Year_Diff, 
      This_Year_xG_Gap, 
      Three_Year_xG_Gap, 
      This_Year_G_Gap, 
      Three_Year_G_Gap), 
    ~rescale(., to = c(0, 100))) %>%
  
  mutate(
    Hart_Metric = 
      0.56*This_Year_Diff +
      0.14*Three_Year_Diff +
      (1-(1/3.75))*0.24*This_Year_xG_Gap +
      (1-(1/3.75))*0.06*Three_Year_xG_Gap +
      1/3.75*0.24*This_Year_G_Gap +
      1/3.75*0.06*This_Year_G_Gap
  )



### format trophy data for norris trophy

norris <- hart %>%
  
  filter(Position == "D")


### format trophy data for calder

calder <- 
  
  left_join(calder_names, hart)


### format data for selke

selke <-
  
  left_join(skater_rapm, zones) %>%
  
  filter(Position != "D",
         Strength != "PP",
         Season == "17-18" | Season == "18-19" | Season == "19-20") %>%
  
  mutate(xG_Diff = xGF - xGA) %>%
  
  group_by(Player, Position) %>%
  
  mutate(Three_Year_xG = sum(xG_Diff),
         Three_Year_xGA = sum(xGA),
         Three_Year_DZS = sum(DZS, na.rm = T),
         Three_Year_DZF = sum(DZF, na.rm = T)) %>%
  
  ungroup() %>%
  
  filter(Season == "19-20") %>%
  
  group_by(Player, Position) %>%
  
  mutate(This_Year_xG = sum(xG_Diff),
         This_Year_xGA = sum(xGA),
         This_Year_DZS = sum(DZS, na.rm = T),
         This_Year_DZF = sum(DZF, na.rm = T),
         Total_TOI = sum(TOI),
         GP = max(GP)) %>%
  
  ungroup() %>%
  
  select(Player:GP, Three_Year_xG:Total_TOI) %>%
  
  distinct() %>%
  
  mutate(This_Year_xG_Gap = This_Year_xG - max(This_Year_xG),
         Three_Year_xG_Gap = Three_Year_xG - max(Three_Year_xG),
         This_Year_xGA_Gap = -This_Year_xGA - min(This_Year_xGA),
         Three_Year_xGA_Gap = -Three_Year_xGA - min(Three_Year_xGA),
         This_Year_DZS_Gap = This_Year_DZS - max(This_Year_DZS),
         Three_Year_DZS_Gap = Three_Year_DZS - max(Three_Year_DZS),
         This_Year_DZF_Gap = This_Year_DZF - max(This_Year_DZF),
         Three_Year_DZF_Gap = Three_Year_DZF - max(Three_Year_DZF)) %>%
  
  mutate_at(
    vars(
      This_Year_xG_Gap, 
      Three_Year_xG_Gap, 
      This_Year_xGA_Gap, 
      Three_Year_xGA_Gap,
      This_Year_DZS_Gap,
      Three_Year_DZS_Gap,
      This_Year_DZF_Gap,
      Three_Year_DZF_Gap), 
    ~rescale(., to = c(0, 100))) %>%
  
  mutate(
    Selke_Metric = 
      0.64*This_Year_xGA_Gap +
      0.16*Three_Year_xGA_Gap +
      0.08*This_Year_DZS_Gap +
      0.02*Three_Year_DZS_Gap +
      0.08*This_Year_DZF_Gap +
      0.02*Three_Year_DZF_Gap
  )


### select top 5 players for each trophy to prepare viz

hart_top <- hart %>%
  
  select(Player, Trophy_Metric = Hart_Metric) %>%
  
  mutate(Trophy = "Hart Trophy") %>%
  
  arrange(-Trophy_Metric) %>%
  
  top_n(5, Trophy_Metric)


norris_top <- norris %>%
  
  select(Player, Trophy_Metric = Hart_Metric) %>%
  
  mutate(Trophy = "Norris Trophy") %>%
  
  arrange(-Trophy_Metric) %>%
  
  top_n(5, Trophy_Metric)


selke_top <- selke %>%
  
  select(Player, Trophy_Metric = Selke_Metric) %>%
  
  mutate(Trophy = "Selke Trophy") %>%
  
  arrange(-Trophy_Metric) %>%
  
  top_n(5, Trophy_Metric)


calder_top <- calder %>%
  
  select(Player, Trophy_Metric = Hart_Metric) %>%
  
  mutate(Trophy = "Calder Trophy") %>%
  
  arrange(-Trophy_Metric) %>%
  
  top_n(5, Trophy_Metric)



### combine the top fives for viz

trophies <- rbind(hart_top, norris_top, selke_top, calder_top) %>%
  
  arrange(Trophy, Trophy_Metric) %>%
  
  mutate(Trophy = factor(Trophy, levels = c("Hart Trophy", "Norris Trophy", "Selke Trophy", "Calder Trophy")),
         Player = factor(Player, levels = unique(Player)))


### do the viz

ggplot(trophies, aes(Player, Trophy_Metric, fill = Trophy)) +
  
  facet_wrap(~Trophy, ncol = 1, scales = "free_y") +
  
  geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
  
  coord_flip() +
  
  ylab("\nSubjective Trophy Metric Scaled 1-100") +
  
  xlab("") +
  
  theme_ipsum_ps() +
  
  ggtitle("2019-2020 Trophy Ballot", 
          subtitle = "Metric is a subjectively weighted blend of SPAR and RAPM\nstats from Evolving Hockey") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2", "darkorchid4", "firebrick")) +
  
  theme(plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        strip.text = element_text(hjust = 0.5, size = 20),
        strip.background = element_rect(color = "gray36"),
        legend.position = "none")

ggsave(filename = "viz/trophy_ballot_2020.jpg", width = 10.666, height = 21.333)