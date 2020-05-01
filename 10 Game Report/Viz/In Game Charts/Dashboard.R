library(tidyverse)
library(ggthemes)
library(extrafont)
library(ggpubr)

loadfonts(device = "win")

teams <- read_csv("team_onice_5v5.csv") %>%
  
  mutate(color = ifelse(Team == "T.B", "dodgerblue3", "gray72"),
         label = ifelse(Team == "T.B", Team, ""),
         label_fill = ifelse(Team == "T.B", "white", "dodgerblue3"),
         label_border = ifelse(Team == "T.B", 0.25, 0)) %>%
  
  group_by(Team) %>%
  
  mutate(season_fix = seq(Season))



defense <- ggplot(teams, aes(Season, (`xGA/60`))) +
  
  geom_violin(fill = "dodgerblue3", alpha = 0.8) +
  
  geom_label(data = teams %>% filter (Team == "T.B"), aes(label = label), family = "Trebuchet MS") +
  
  theme_few() +
  
  xlab("\n") +
  
  ylab("Expected goals allowed\n") +
  
  ggtitle("Defense", subtitle = "") +
  
  scale_color_manual(values = teams$color) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 18, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold", family = "Trebuchet MS"))



goaltending <- ggplot(teams, aes(Season, (`xGA/60` - `GA/60`))) +
  
  geom_violin(fill = "dodgerblue3", alpha = 0.4) +
  
  geom_label(data = teams %>% filter (Team == "T.B"), aes(label = label), family = "Trebuchet MS") +
  
  theme_few() +
  
  xlab("\n") +
  
  ylab("Goals saved above expected\n") +
  
  ggtitle("Goaltending", subtitle = "") +
  
  scale_color_manual(values = teams$color) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 18, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold", family = "Trebuchet MS"))



offense <- ggplot(teams, aes(Season, (`xGF/60`))) +
  
  geom_violin(fill = "darkorange2", alpha = 0.8) +
  
  geom_label(data = teams %>% filter (Team == "T.B"), aes(label = label), family = "Trebuchet MS") +
  
  theme_few() +
  
  xlab("\n") +
  
  ylab("Expected goals generated\n") +
  
  ggtitle("Offense", subtitle = "") +
  
  scale_color_manual(values = teams$color) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 18, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold", family = "Trebuchet MS"))



shooting <- ggplot(teams, aes(Season, (`GF/60` - `xGF/60`))) +
  
  geom_violin(fill = "darkorange2", alpha = 0.4) +
  
  geom_label(data = teams %>% filter (Team == "T.B"), aes(label = label), family = "Trebuchet MS") +
  
  theme_few() +
  
  xlab("\n") +
  
  ylab("Goals scored above expected\n") +
  
  ggtitle("Shooting", subtitle = "") +
  
  scale_color_manual(values = teams$color) +
  
  theme(axis.text = element_text(size = 14, face = "bold", family = "Trebuchet MS"),
        axis.title = element_text(size = 16, face = "bold", family = "Trebuchet MS"),
        plot.title = element_text(size = 18, face = "bold", family = "Trebuchet MS", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "italic", family = "Trebuchet MS", hjust = 0.5),
        legend.title = element_text(size = 14, face = "bold", family = "Trebuchet MS"))



dash <- ggarrange(defense, 
          goaltending, 
          offense, 
          shooting, 
          nrow = 2,
          ncol = 2)

annotate_figure(dash,
                top = text_grob("Tampa Bay Lightning 5v5 Performance per 60 Minutes\n", family = "Trebuchet MS", size = 22, face = "bold"),
                bottom = text_grob("All data via Evolving Hockey", family = "Trebuchet MS", size = 18, face = "italic", hjust = -2.25))

ggsave("dashboard.png", width = 21.333, height = 10.666)
