### to do
## better comments

#load dependencies
library(tidyverse) 
library(ggthemes)
library(extrafont)


#load fonts for viz
loadfonts(device = "win")


#load and format 5v5 data
site_5v5 <- "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=sva&loc=B&team=All&rate=n"

data_5v5 <- read_html(site_5v5) %>%
  
  html_table() %>%
  
  data.frame(.) %>%
  
  rename(Date = 1) %>%
  
  mutate_at(vars(CF:Attendance), funs(as.numeric(gsub("-", 0, .)))) %>%
  
  mutate(Strength = "5v5", 
         Date = as.Date(substr(Date, 1, 10)))


#format data and add cumulative stats

mm_5v5 <- data_5v5 %>%
  
  arrange(Team, Date) %>%
  
  group_by(Team) %>%
  
  mutate(N = row_number(), Running_ShotDiff = cumsum(CF) - cumsum(CA), Running_xGDiff = cumsum(xGF) - cumsum(xGA), Running_GoalDiff = cumsum(GF) - cumsum(GA))


#format shots only data

mm_5v5_shots <- mm_5v5 %>%
  
  group_by(Team) %>%
  
  filter(N == max(N)) %>%
  
  arrange(-Running_ShotDiff) %>%
  
  mutate(Order = row_number(), Total_ShotDiff = Running_ShotDiff) %>%
  
  select(Team, Order, Total_ShotDiff) %>%
  
  left_join(., mm5v5[c("Team", "N", "Running_ShotDiff")], by = c("Team" = "Team")) %>%
  
  arrange(Order) %>%
  
  mutate(Team = factor(Team, levels = unique(Team)))


#plot shot differential chart

ggplot(mm_5v5_shots, aes(x = N, y = Running_ShotDiff)) +
  
  geom_area(fill = "darkorange2") +
  
  facet_wrap(~Team, ncol = 7) +
  
  theme_few() +
  
  ggtitle("2019-2020 NHL Running 5v5 Shot Differentials",
          subtitle = "Data via Natural Stat Trick") +
  
  labs(x = "\nGames Played", 
       y = "Running 5v5 Shot Differential\n",
       caption = "chart by @loserpoints") +
  
  theme(strip.background = element_rect(fill = "dodgerblue4"), 
        strip.text = element_text(color = "white", face = "bold", size = 18),
        axis.text = element_text(face = "bold", size = 14, family = "Trebuchet MS"),
        axis.title.x = element_text(face = "bold", size = 18, family = "Trebuchet MS"), 
        axis.title.y = element_text(face = "bold", size = 22, family = "Trebuchet MS"),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5, family = "Trebuchet MS"),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5), family = "Trebuchet MS"))


#save plot

ggsave("Viz/mm_running_shotdiff.png", width = 21.333, height = 10.667)


#format xG and goal data

mm_5v5_xG <- data_5v5 %>%
  
  group_by(Team) %>%
  
  filter(N == max(N)) %>%
  
  arrange(-Running_xGDiff) %>%
  
  mutate(Order = row_number(), Total_xGDiff = Running_xGDiff) %>%
  
  select(Team, Order, Total_xGDiff) %>%
  
  left_join(., mm5v5[c("Team", "N", "Running_xGDiff", "Running_GoalDiff")], by = c("Team" = "Team")) %>%
  
  arrange(Order) %>%
  
  mutate(Team = factor(team, levels = unique(Team)))


#plot xG and goal differential chart

ggplot(mm_5v5_xG, aes(x = N, y = Running_GoalDiff, fill = "Goals")) +
  
  geom_area() +
  
  geom_line(aes(x = N, y = Running_xGDiff, color = "xG"), size = 2) +
  
  facet_wrap(~Team, ncol = 7) +
  
  theme_few() +
  
  scale_fill_manual(values = c("gray63")) +
  
  scale_color_manual(values = c("Goals" = "gray63", "xG" = "darkorange2")) +
  
  ggtitle("2019-2020 NHL Running 5v5 xG and Goal Differentials",
          subtitle = "Data via Natural Stat Trick") +
  
  labs(x = "\nGames Played", 
       y = "Running 5v5 xG and Goal Differential\n",
       caption = "chart by @loserpoints") +
  
  theme(legend.key = element_blank(), 
        legend.title = element_blank(),
        legend.box = "horizontal", 
        legend.position = "bottom", 
        legend.text = element_text(size = 14, face = "bold", family = "TrebuchetMS"),
        strip.background = element_rect(fill = "dodgerblue4"), 
        strip.text = element_text(color = "white", face = "bold", size = 18, family = "TrebuchetMS"),
        axis.text = element_text(face = "bold", size = 14, family = "TrebuchetMS"),
        axis.title.x = element_text(face = "bold", size = 18, family = "TrebuchetMS"),
        axis.title.y = element_text(face = "bold", size = 18, family = "TrebuchetMS"),
        plot.title = element_text(face = "bold", size = 22, hjust = 0.5, family = "TrebuchetMS"),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        plot.caption = element_text(size = 18, face = "italic", hjust = 1, margin = margin(t = 15, b = 5), family = "TrebuchetMS"))


#save plot

ggsave("Viz/mm_running_xgdiff.png", width = 21.333, height = 10.667)
