### to do
## better title spacing on final dashboard
## allow for selection of any two teams


######### generate team comparison chart with descriptive variabile names

###### load dependencies

require(tidyverse) 
require(ggthemes)
require(scales)
require(ggpubr)
library(cowplot)


###### generate team comparison chart using function defined below

generate_team_comparison_dashboard()


###### define function for generating team comparison chart

generate_team_comparison_dashboard <- function(x) {

### format data for comparison charts

comp_chart <- report_data [3] %>% 
  
  data.frame(.) %>%
  
  filter(Team == "T.B" | Team == "ARI") %>%
  
  mutate(Team_Order = ifelse(Team == "T.B", 1, 2)) %>%
  
  arrange(Team_Order, Group_Order, Measure_Order) %>%
  
  mutate(Team = factor(Team, levels = unique(Team)),
         Group = factor(Group, levels = unique(Group)),
         Verbose = factor(Verbose, levels = unique(Verbose)))


### define function for ordering within a variable in ggplot

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) 
  
  {
  
  new_x <- paste(x, within, sep = sep)
  
  stats::reorder(new_x, by, FUN = fun)

  }


### create 5v5 offense plot

off_5v5 <- comp_chart %>% 
  
  filter(Group == "5v5 Offense") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


off_5v5_plot <- 
  
  ggplot(off_5v5,
         aes(
           x = reorder_within(Team,-Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Generation"),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Generation"),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("5v5 Offense") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )


### create 5v5 defense plot

def_5v5 <- comp_chart %>%
  
  filter(Group == "5v5 Defense") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


def_5v5_plot <- 
  
  ggplot(def_5v5,
         aes(
           x = reorder_within(Team, -Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Suppression"),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Suppression"),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("5v5 Defense") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )


### create 5v5 shares plot

total_5v5 <- comp_chart %>% 
  
  filter(Group == "5v5 Total") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


total_5v5_plot <- 
  
  ggplot(total_5v5,
         aes(
           x = reorder_within(Team, -Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Share"),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Share"),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("5v5 Total") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )


### create 5v5 results plot

result_5v5 <- comp_chart %>% 
  
  filter(Group == "5v5 Results") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


result_5v5_plot <- 
  
  ggplot(result_5v5,
         aes(
           x = reorder_within(Team,-Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Goals Scored Above Exp."),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Goals Scored Above Exp."),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("5v5 Results") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )


### create 5v4 offense plot

off_5v4 <- comp_chart %>% 
  
  filter(Group == "5v4 Offense") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


off_5v4_plot <-
  
  ggplot(off_5v4,
         aes(
           x = reorder_within(Team, -Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Generation"),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Generation"),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("5v4 Offense") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )


### create 4v5 defense plot

def_4v5 <- comp_chart %>% 
  
  filter(Group == "4v5 Defense") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


def_4v5_plot <- 
  
  ggplot(def_4v5,
         aes(
           x = reorder_within(Team,-Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Suppression"),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Shot Suppression"),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("4v5 Defense") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )


### create special teams results plot

result_st <- comp_chart %>% 
  
  filter(Group == "ST Results") %>%
  
  arrange(Team_Order, Group_Order, Measure_Order)


result_st_plot <- 
  
  ggplot(result_st,
         aes(
           x = reorder_within(Team, -Team_Order, Verbose),
           y = Season_Value_Z_Score,
           fill = Team
         )) +
  
  geom_bar(stat = "identity") +
  
  geom_point(
    aes(
      x = reorder_within(Team,-Team_Order, Verbose),
      y = U10_Value_Z_Score,
      fill = Team
    ),
    size = 5,
    shape = 21,
    stroke = 1,
    color = "black"
  ) +
  
  geom_rect(
    ymin = -2.5,
    ymax = -1,
    xmin = -Inf,
    xmax = Inf,
    fill = "tomato",
    alpha = 0.05
  ) +
  
  geom_rect(
    ymin = 1,
    ymax = 2.5,
    xmin = -Inf,
    xmax = Inf,
    fill = "forestgreen",
    alpha = 0.05
  ) +
  
  geom_hline(yintercept = 1,
             linetype = 2,
             size = 1) +
  
  geom_hline(yintercept = -1,
             linetype = 2,
             size = 1) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Goals Scored Above Exp."),
    aes(
      x = 2.15,
      y = 1.25,
      label = "Good",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  geom_label(
    data = filter(comp_chart, Verbose == "Goals Scored Above Exp."),
    aes(
      x = 2.15,
      y = -1.25,
      label = "Bad",
      size = 6
    ),
    fill = "white",
    family = "Trebuchet MS"
  ) +
  
  facet_wrap(
    ~ Verbose,
    ncol = 1,
    strip.position = "left",
    scales = "free_y",
    labeller = label_wrap_gen(15)
  ) +
  
  coord_flip(ylim = c(-2.5, 2.5)) +
  
  theme_few() +
  
  ggtitle("Special Teams Results") +
  
  scale_fill_manual(values = c("dodgerblue3", "darkorange2")) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      family = "Trebuchet MS"
    ),
    legend.position = "none",
    axis.title = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      face = "bold",
      size = 15,
      angle = 180,
      family = "Trebuchet MS"
    ),
    axis.text = element_blank(),
    plot.caption = element_text(
      size = 18,
      face = "italic",
      hjust = 1,
      margin = margin(t = 15, b = 5),
      family = "Trebuchet MS"
    )
  )

### get legend for dashboard

legend <- get_legend(result_st_plot)


### arrange plots on dashboard

set_null_device("png")

comp_dash <-
  
  ggarrange(
    off_5v5_plot,
    off_5v4_plot,
    def_5v5_plot,
    def_4v5_plot,
    total_5v5_plot,
    result_st_plot + theme(legend.position = "none"),
    result_5v5_plot,
    legend,
    nrow = 4,
    ncol = 2,
    heights = c(3, 3, 2, 2)
  )


### get title info
comp_chart_teams <- comp_chart %>%
  
  select(Team) %>%
  
  distinct()

team1 <- "T.B"

team2 <- comp_chart_teams %>%
  
  filter(Team != "T.B")

team2 <- team2[1, 1]

date <- Sys.Date()


### add titles and annotations to dashboard

annotate_figure(
  comp_dash,
  top = text_grob(
    paste0(team1, " vs. ", team2, " (", date, ")\n"),
    face = "bold",
    size = 28
  ),
  bottom = text_grob(
    "All data via naturalstattrick.com, chart by @loserpoints",
    hjust = 1.1,
    x = 1,
    face = "italic",
    size = 18
  )
)

### save plot

ggsave(paste0("Viz/team_comp_dash_", date, ".jpg"), width = 21.333, height = 10.667)

}
