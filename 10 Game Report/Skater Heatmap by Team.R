### to do
## fix traded players
## clean up shot assists
## pipe everything, this code is unreadable
## better comments

#load dependencies

library(tidyverse)
library(ggthemes)
library(scales)
library(extrafont)


#load fonts for viz

loadfonts(device = "win")


#read local CSVs

skaters.5v5.summary <- read_csv("Data/5v5 summary.csv")

skaters.5v5.onice <- read_csv("Data/5v5 on ice.csv")

skaters.5v4.summary <- read_csv("Data/5v4 summary.csv")

skaters.4v5.summary <- read_csv("Data/4v5 summary.csv")

skaters.ev.rapm <- read_csv("Data/ev rapm.csv")

skaters.war <- read_csv("Data/war.csv")


#Get Team TOI

team.toi <- report_data [2] %>%
  
  data.frame(.) %>%
  
  filter(Measure == "TOI_Running") %>%
  
  pivot_wider(names_from = Strength, values_from = Season.Value) %>%
  
  select(Team, Team.4v5 = '4v5', Team.5v4 = '5v4', Team.5v5 = '5v5')


#Select relevant measures

skaters.war <- skaters.war %>%
  
  select(Player, Position, WAR, TOI_All) %>%
  
  group_by(Player, Position) %>%
  
  summarize_all(sum) %>%
  
  mutate(WAR.60 = WAR/TOI_All) %>%
  
  ungroup() %>%
  
  select(-TOI_All)


skaters.5v5.onice <- skaters.5v5.onice %>%
  
  select(Player, Position, CF) %>%
  
  group_by(Player, Position) %>%
  
  summarize_all(sum) %>%
  
  ungroup()


skaters.5v5.summary <- skaters.5v5.summary[c(1,4,6:9,13)] %>%
  
  group_by(Player, Position) %>%
  
  summarize_all(sum) %>%
  
  ungroup() %>%
  
  mutate(P1.60 = (G + A1)/TOI*60, A = A1 + A2) %>%
  
  select(-G, -A1, -A2) %>%
  
  left_join(., skaters.5v5.onice, by = c("Player", "Position"))


skaters.5v4.summary <- skaters.5v4.summary[c(1,4,6:8)] %>%
  
  group_by(Player, Position) %>%
  
  summarize_all(sum) %>%
  
  mutate(P1.60 = (G + A1)/TOI*60,
         P1.60 = as.numeric(gsub(NaN, 0, P1.60))) %>%
  
  ungroup() %>%
  
  select(-G, -A1)


skaters.4v5.summary <- skaters.4v5.summary[c(1,4,6)] %>%
  
  group_by(Player, Position) %>%
  
  summarize_all(sum) %>%
  
  ungroup()


skaters.ev.rapm <- skaters.ev.rapm %>%
  
  rename(xGPM_60 = 8, CPM_60 = 9) %>%
  
  select(Player, Position, Team, 'xGF/60', 'xGA/60', 'xGPM_60', 'CF/60', 'CA/60', 'CPM_60')


colnames(skaters.5v4.summary) <- c("Player", "Position", "TOI.5v4", "P160.5v4")

colnames(skaters.4v5.summary) <- c("Player", "Position", "TOI.4v5")

colnames(skaters.5v5.summary) [1:2] <- c("Player", "Position")


skaters.all <- 
  
  left_join(skaters.5v5.summary, skaters.ev.rapm, by = c("Player", "Position")) %>%
  
  left_join(., skaters.war, by = c("Player", "Position")) %>%
  
  left_join(., skaters.5v4.summary, by = c("Player", "Position")) %>%
  
  left_join(., skaters.4v5.summary, by = c("Player", "Position")) %>%
  
  #mutate(Team = substr(Team, 1, 3)) %>%
  
  left_join(., team.toi, by = "Team") %>%
  
  mutate(
    Total.P160 = ((P1.60*TOI/60) + (P160.5v4*TOI.5v4/60))/(TOI + TOI.5v4)*60,
    TOI.5v5 = ifelse(is.na(Team.5v5), TOI/mean(team.toi$Team.5v5), TOI/Team.5v5), 
    TOI.5v4 = ifelse(is.na(Team.5v4), TOI.5v4/mean(team.toi$Team.5v4), TOI.5v4/Team.5v4),
    TOI.4v5 = ifelse(is.na(Team.4v5), TOI.4v5/mean(team.toi$Team.4v5), TOI.4v5/Team.4v5)) %>%
  
  select(-Team.5v5, -Team.5v4, - Team.4v5)


#Add shot assist estimations

skaters.all$Position <- ifelse(skaters.all$Position == "D", "D", "F")

skaters.all$eSA <-
  
  ifelse(
    skaters.all$Position == "F",
    -0.3307 +
      (skaters.all$A / skaters.all$iCF * 2.793) +
      (skaters.all$CF / skaters.all$iCF * 0.3342),
    (
      -0.2889 +
        (skaters.all$A / skaters.all$iCF * 0.5905) +
        (skaters.all$CF / skaters.all$iCF * 0.2511)
    )
  ) * skaters.all$iCF


skaters.all$eSC.60 <-(skaters.all$iCF + skaters.all$eSA) / skaters.all$TOI * 60

skaters.all$eSA.60 <- skaters.all$eSA/skaters.all$TOI*60

skaters.all$icF.60 <- skaters.all$iCF/skaters.all$TOI*60

skaters.all$TOI <- skaters.all$TOI.5v5

skaters.all <- skaters.all[c(-3, -4, -6, -7, -15, -22)]


#Filter by TOI

skaters.all <- skaters.all %>%
  
  group_by(Position) %>%
  
  arrange(-TOI.5v5) %>%
  
  mutate(N = row_number()) %>%
  
  filter(Position == "F" & N < 404 | Position == "D" & N < 218 | Player == "ADAM.ERNE" | Player == "RYAN.CALLAHAN") %>%
  
  select(-N)


#Reshape data and add groupings

skaters.all <- melt(skaters.all, id = c(1,2,4))

colnames(skaters.all) [4:5] <- c("Measure", "Value")

skaters.all.measures <- data.frame(unique(skaters.all$Measure))

colnames(skaters.all.measures) <- c("Measure")

skaters.all.measures$Verbose <-
  c(
    "5v5 Primary Scoring",
    "Expected Goal Generation",
    "Expected Goal Suppression",
    "Expected Goal Diff",
    "Shot Generation",
    "Shot Suppression",
    "Shot Diff",
    "Wins Above Replacement",
    "5v4 TOI",
    "5v4 Primary Scoring",
    "4v5 TOI",
    "Total Primary Scoring",
    "5v5 TOI",
    "Estimated Shot Contributions",
    "Estimated Shot Assists",
    "Shots"
  )


skaters.all.measures$Measure.Order <-
  
  c(3, 1, 1, 1, 2, 2, 2, 1, 2, 2, 1, 1, 3, 1, 2, 3)

skaters.all.measures$Group <-
  c(
    "Scoring",
    "EV Offense",
    "EV Defense",
    "EV Total",
    "EV Offense",
    "EV Defense",
    "EV Total",
    "WAR",
    "Icetime",
    "Scoring",
    "Icetime",
    "Scoring",
    "Icetime",
    "Shot Contribs",
    "Shot Contribs",
    "Shot Contribs"
  )


skaters.all.measures$Group.Order <-
  
  c(2, 4, 5, 6, 4, 5, 6, 7, 1, 2, 1, 2, 1, 3, 3, 3)


skaters.all <- 
  
  left_join(skaters.all, skaters.all.measures, by = "Measure") %>%
  
  mutate(Value = ifelse(Group == "EV Defense", Value*-1, Value))


#add zcores for relevant measures

skaters.all <- skaters.all %>%
  
  group_by(Measure, Position) %>%
  
  mutate_at(vars(Value), funs(Z.Score = scale)) %>%
  
  ungroup()


#reorder players by 5v5 TOI and reorder measures and groups

skaters.order <- skaters.all %>%
  
  filter(Measure == "TOI.5v5") %>%
  
  arrange(desc(Position),-Value) %>%
  
  mutate(Player.Order = row_number()) %>%
  
  select(Player, Player.Order)


skaters.all <- 
  
  left_join(skaters.all, skaters.order, by = "Player") %>%
  
  arrange(Player.Order, Group.Order, Measure.Order)


#Reorder factor levels for all ordering variables

skaters.all$Player <- factor(skaters.all$Player, levels = unique(skaters.all$Player))

skaters.all$Group <- factor(skaters.all$Group, levels = unique(skaters.all$Group))

skaters.all$Measure <- factor(skaters.all$Measure, levels = unique(skaters.all$Measure))

skaters.all$Verbose <- factor(skaters.all$Verbose, levels = unique(skaters.all$Verbose))


#Team Selection

skaters.chart <- skaters.all %>%
  
  filter(grepl("T.B", Team) | Player == "Blake Coleman")


#Generate team skater KPI heat map

ggplot(skaters.chart, aes(x = Player, y = Verbose, fill = Z.Score)) +
  
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
  
  scale_x_discrete(position = "top")
  
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


#save plot

ggsave(paste0("Viz/lightning_skater_heatmap_", Sys.Date(), ".png"), width = 21.333, height = 10.667)

