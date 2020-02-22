### to do
## better title spacing on final dashboard


######### generate single team chart with descriptive variabile names

###### load dependencies

library(tidyverse)
library(ggthemes) 
library(scales) 
library(ggpubr)
library(extrafont)
library(cowplot)

###### load fonts for viz

loadfonts(device = "win", quiet = T)


###### select team for chart

select_team <- "T.B"

verbose_team <- "Tampa Bay Lightning"


###### generate team chart using function defined below

generate_team_dashboard()


###### define function for generating team chart

generate_team_dashboard <- function(x) {
  
  team_chart <- report_data [3] %>%
    
    data.frame(.) %>%
    
    filter(Team == select_team) %>%
    
    arrange(Group_Order, Measure_Order) %>%
    
    mutate(
      Group = factor(Group, levels = unique(Group)),
      Verbose = factor(Verbose, levels = unique(Verbose))
    )
  
  
  ####### create individual component plots to be arranged on dashboard
  
  ### create 5v5 offense plot
  
  off_5v5 <- team_chart %>%
    
    filter(Group == "5v5 Offense") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  off_5v5_plot <-
    
    ggplot(off_5v5, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Generation"),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Generation"),
      aes(
        x = 1.35,
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
    
    ggtitle("5v5 Offense") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  
  ### create 5v5 defense plot
  
  def_5v5 <- team_chart %>%
    
    filter(Group == "5v5 Defense") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  def_5v5_plot <-
    
    ggplot(def_5v5, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Suppression"),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Suppression"),
      aes(
        x = 1.35,
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
    
    ggtitle("5v5 Defense") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  ### create 5v5 share plot
  
  total_5v5 <- team_chart %>%
    
    filter(Group == "5v5 Total") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  total_5v5_plot <-
    
    ggplot(total_5v5, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Share"),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Share"),
      aes(
        x = 1.35,
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
    
    ggtitle("5v5 Total") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  ### create 5v5 results plot
  
  result_5v5 <- team_chart %>%
    
    filter(Group == "5v5 Results") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  result_5v5_plot <-
    
    ggplot(result_5v5, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Goals Scored Above Exp."),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Goals Scored Above Exp."),
      aes(
        x = 1.35,
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
    
    ggtitle("5v5 Results") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  ### create 5v4 offense plot
  
  off_5v4 <- team_chart %>%
    
    filter(Group == "5v4 Offense") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  off_5v4_plot <-
    
    ggplot(off_5v4, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Generation"),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Generation"),
      aes(
        x = 1.35,
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
    
    ggtitle("5v4 Offense") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  ### create 4v5 defense plot
  
  def_4v5 <- team_chart %>%
    
    filter(Group == "4v5 Defense") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  def_4v5_plot <-
    
    ggplot(def_4v5, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Suppression"),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Shot Suppression"),
      aes(
        x = 1.35,
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
    
    ggtitle("4v5 Defense") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  ### create special teams results plot
  
  result_st <- team_chart %>%
    
    filter(Group == "ST Results") %>%
    
    arrange(Group_Order, Measure_Order)
  
  
  result_st_plot <-
    
    ggplot(result_st, aes(x = Verbose, y = Season_Value_Z_Score)) +
    
    geom_bar(stat = "identity", fill = "dodgerblue3") +
    
    geom_point(
      aes(x = Verbose, y = PU10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "gray"
    ) +
    
    geom_point(
      aes(x = Verbose, y = U10_Value_Z_Score),
      size = 5,
      shape = 21,
      stroke = 1,
      color = "black",
      fill = "darkorange2"
    ) +
    
    geom_hline(yintercept = 1,
               linetype = 2,
               size = 1) +
    
    geom_hline(yintercept = -1,
               linetype = 2,
               size = 1) +
    
    geom_rect(
      ymin = -2.5,
      ymax = -1,
      xmin = -Inf,
      xmax = Inf,
      fill = "tomato",
      alpha = 0.1
    ) +
    
    geom_rect(
      ymin = 1,
      ymax = 2.5,
      xmin = -Inf,
      xmax = Inf,
      fill = "forestgreen",
      alpha = 0.1
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Goals Scored Above Exp."),
      aes(
        x = 1.35,
        y = 1.25,
        label = "Good",
        size = 6
      ),
      fill = "white",
      family = "Trebuchet MS"
    ) +
    
    geom_label(
      data = filter(team_chart, Verbose == "Goals Scored Above Exp."),
      aes(
        x = 1.35,
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
    
    ggtitle("Special Teams Results") +
    
    theme_few() +
    
    theme(
      plot.title = element_text(
        face = "bold",
        size = 22,
        family = "Trebuchet MS"
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(
        face = "bold",
        size = 15,
        angle = 180,
        family = "Trebuchet MS"
      ),
      legend.position = "none",
      plot.caption = element_text(
        size = 18,
        face = "italic",
        hjust = 1,
        margin = margin(t = 15, b = 5),
        family = "Trebuchet MS"
      )
    )
  
  
  ###### arrange plots on dashboard
  
  set_null_device("png")
  
  
  teamdash <-
    
    plot_grid(
      off_5v5_plot,
      off_5v4_plot,
      def_5v5_plot,
      def_4v5_plot,
      total_5v5_plot,
      result_st_plot +
        theme(legend.position = "none"),
      result_5v5_plot,
      text_grob(
        "The good and bad reference lines are at 1 and -1 standard deviations respectively for each measure.\nThe bar length is the z-score relative to all 31 NHL teams so far this season.\nThe orange dot represents the measure over the previous ten games.\nThe gray dot represents the measure over the ten games before that.",
        size = 14,
        family = "Trebuchet MS"
      ),
      nrow = 4,
      ncol = 2,
      heights = c(3, 3, 2, 2)
    )
  
  
  annotate_figure(
    teamdash,
    top = text_grob(
      paste0(verbose_team, " Team Performance\n"),
      face = "bold",
      size = 28,
      family = "Trebuchet MS"
    ),
    bottom = text_grob(
      "All data via naturalstattrick.com, chart by @loserpoints",
      hjust = 1.1,
      x = 1,
      face = "italic",
      size = 18,
      family = "Trebuchet MS"
    )
  )
  
  
  ###### save plot
  
  ggsave(paste0("Viz/team_dash_", Sys.Date(), ".jpg"),
         width = 21.333,
         height = 10.667)
  
}
