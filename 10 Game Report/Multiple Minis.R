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

mm5v5 <- mm5v5 %>%
  
  arrange(Team, Date) %>%
  
  group_by(Team) %>%
  
  mutate(N = row_number(), Running.ShotDiff = cumsum(CF) - cumsum(CA), Running.xGDiff = cumsum(xGF) - cumsum(xGA), Running.GoalDiff = cumsum(GF) - cumsum(GA))


#format shots only data

mm5v5.shots <- mm5v5 %>%
  
  group_by(Team) %>%
  
  filter(N == max(N)) %>%
  
  arrange(-Running.ShotDiff) %>%
  
  mutate(Order = row_number(), Total.ShotDiff = Running.ShotDiff) %>%
  
  select(Team, Order, Total.ShotDiff) %>%
  
  left_join(., mm5v5[c("Team", "N", "Running.ShotDiff")], by = c("Team" = "Team")) %>%
  
  arrange(Order)


#order teams

mm5v5.shots$Team <- factor(mm5v5.shots$Team, levels = unique(mm5v5.shots$Team))


#plot shot differential chart

ggplot(mm5v5.shots, aes(x = N, y = Running.ShotDiff)) +
  
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

mm5v5.xG <- mm5v5 %>%
  
  group_by(Team) %>%
  
  filter(N == max(N)) %>%
  
  arrange(-Running.xGDiff) %>%
  
  mutate(Order = row_number(), Total.xGDiff = Running.xGDiff) %>%
  
  select(Team, Order, Total.xGDiff) %>%
  
  left_join(., mm5v5[c("Team", "N", "Running.xGDiff", "Running.GoalDiff")], by = c("Team" = "Team")) %>%
  
  arrange(Order)


#order teams

mm5v5.xG$Team <- factor(mm5v5.xG$Team, levels = unique(mm5v5.xG$Team))


#plot xG and goal differential chart

ggplot(mm5v5.xG, aes(x = N, y = Running.GoalDiff, fill = "Goals")) +
  
  geom_area() +
  
  geom_line(aes(x = N, y = Running.xGDiff, color = "xG"), size = 2) +
  
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
