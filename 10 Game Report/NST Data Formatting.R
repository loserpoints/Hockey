### to do
## better comments


#load dependencies

library(tidyverse)
library(ggthemes)
library(scales)
library(rvest)

report_data <- get_report_data()

get_report_data <- function(x) {
  
  #load and format 5v5 data
  site_5v5 <-
    "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=sva&loc=B&team=All&rate=n"
  
  data_5v5 <- read_html(site_5v5) %>%
    
    html_table() %>%
    
    data.frame(.) %>%
    
    rename(Date = 1) %>%
    
    mutate_at(vars(CF:Attendance), funs(as.numeric(gsub("-", 0, .)))) %>%
    
    mutate(Strength = "5v5",
           Date = as.Date(substr(Date, 1, 10)))
  
  
  #load and format 5v4 data
  site_5v4 <-
    "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=5v4&loc=B&team=All&rate=n"
  
  data_5v4 <- read_html(site_5v5) %>%
    
    html_table() %>%
    
    data.frame(.) %>%
    
    rename(Date = 1) %>%
    
    mutate_at(vars(CF:Attendance), funs(as.numeric(gsub("-", 0, .)))) %>%
    
    mutate(Strength = "5v4",
           Date = as.Date(substr(Date, 1, 10)))
  
  
  #load and format 4v5 data
  site_4v5 <-
    "http://naturalstattrick.com/games.php?fromseason=20192020&thruseason=20192020&stype=2&sit=4v5&loc=B&team=All&rate=n"
  
  data_4v5 <- read_html(site_4v5) %>%
    
    html_table() %>%
    
    data.frame(.) %>%
    
    rename(Date = 1) %>%
    
    mutate_at(vars(CF:Attendance), funs(as.numeric(gsub("-", 0, .)))) %>%
    
    mutate(Strength = "4v5",
           Date = as.Date(substr(Date, 1, 10)))
  
  
  #combine all situations data
  
  all_situations <- rbind(data_5v5, data_5v4, data_4v5)
  
  
  #convert long form team names to short
  
  teams <- all_situations %>%
    
    select(Team) %>%
    
    distinct() %>%
    
    arrange(Team) %>%
    
    mutate(
      Team_Short =
        c(
          "ANA",
          "ARI",
          "BOS",
          "BUF",
          "CGY",
          "CAR",
          "CHI",
          "COL",
          "CBJ",
          "DAL",
          "DET",
          "EDM",
          "FLA",
          "L.A",
          "MIN",
          "MON",
          "NSH",
          "N.J",
          "NYI",
          "NYR",
          "OTT",
          "PHI",
          "PIT",
          "S.J",
          "STL",
          "T.B",
          "TOR",
          "VAN",
          "VGK",
          "WSH",
          "WPG"
        )
    )
  
  
  all_situations <-
    
    inner_join(all_situations, teams, by = c("Team"))
  
  
  #select measures and calc running sums for full season
  
  season <- all_situations %>%
    
    select(Team = Team_Short, Date, Strength, TOI, CF, CA, FF, FA, xGF, xGA, GF, GA) %>%
    
    arrange(Team, Date, Strength) %>%
    
    group_by(Team, Strength) %>%
    
    mutate(N = row_number()) %>%
    
    group_by(Team, Date) %>%
    
    mutate(N = max(N)) %>%
    
    ungroup() %>%
    
    select(-Date) %>%
    
    group_by(Team, Strength) %>%
    
    mutate_at(vars(CF:GA), funs(gsub("-", 0, .))) %>%
    
    mutate_at(vars(-group_cols(),-N), funs(Running = cumsum(.))) %>%
    
    select(Team, Strength, N, TOI_Running:GA_Running)
  
  
  #additional full season calcs
  
  season <- season %>%
    
    group_by(Team, Strength) %>%
    
    mutate_at(
      vars(
        CF_Running,
        CA_Running,
        xGF_Running,
        xGA_Running,
        GF_Running,
        GA_Running
      ),
      funs(Per_60 = . / TOI_Running * 60)
    ) %>%
    
    mutate(
      CFS_Running = CF_Running / (CF_Running + CA_Running),
      xGS_Running = xGF_Running / (xGF_Running + xGA_Running),
      GS_Running = GF_Running / (GF_Running + GA_Running),
      xFSh_Running = xGF_Running / FF_Running,
      xFSv_Running = 1 - (xGA_Running / FA_Running),
      FSh_Running = GF_Running / FF_Running,
      FSv_Running = 1 - (GA_Running / FA_Running),
      GFAE_Running = GF_Running - xGF_Running,
      GAAE_Running = xGA_Running - GA_Running,
      GDiffAE_Running = (GF_Running - xGF_Running) + (xGA_Running - GA_Running),
      CA_Running_Per_60 = CA_Running_Per_60 * -1,
      xGA_Running_Per_60 = xGA_Running_Per_60 * -1
    )
  
  
  #select max N for each team
  
  season <- season %>%
    
    group_by(Team, Strength) %>%
    
    filter(N == max(N)) %>%
    
    pivot_longer(-c("Team", "Strength", "N"),
                 names_to = "Measure",
                 values_to = "Season_Value")
  
  
  #select ultimate ten games
  
  ultimate_ten <- all_situations %>%
    
    select(Team = Team_Short, Date, Strength, TOI, CF, CA, FF, FA, xGF, xGA, GF, GA) %>%
    
    arrange(Team, Date, Strength) %>%
    
    group_by(Team, Strength) %>%
    
    mutate(N = row_number()) %>%
    
    group_by(Team, Date) %>%
    
    mutate(N = max(N)) %>%
    
    ungroup() %>%
    
    select(-Date) %>%
    
    group_by(Team, Strength) %>%
    
    filter(N > max(N) - 10)
  
  
  #subset measures and calc running sums for ultimate ten games
  
  ultimate_ten <- ultimate_ten %>%
    
    select(Team, Strength, N, TOI, CF, CA, FF, FA, xGF, xGA, GF, GA) %>%
    
    arrange(Team, N, Strength) %>%
    
    group_by(Team, Strength) %>%
    
    mutate_at(vars(-group_cols()), funs(Running = cumsum)) %>%
    
    select(Team:N, TOI_Running:GA_Running)
  
  
  #additional ultimate 10 game calcs
  
  ultimate_ten <- ultimate_ten %>%
    
    group_by(Team, Strength) %>%
    
    mutate_at(
      vars(
        CF_Running,
        CA_Running,
        xGF_Running,
        xGA_Running,
        GF_Running,
        GA_Running
      ),
      funs(Per_60 = . / TOI_Running * 60)
    ) %>%
    
    mutate(
      CFS_Running = CF_Running / (CF_Running + CA_Running),
      xGS_Running = xGF_Running / (xGF_Running + xGA_Running),
      GS_Running = GF_Running / (GF_Running + GA_Running),
      xFSh_Running = xGF_Running / FF_Running,
      xFSv_Running = 1 - (xGA_Running / FA_Running),
      FSh_Running = GF_Running / FF_Running,
      FSv_Running = 1 - (GA_Running / FA_Running),
      GFAE_Running = GF_Running - xGF_Running,
      GAAE_Running = xGA_Running - GA_Running,
      GDiffAE_Running = (GF_Running - xGF_Running) + (xGA_Running - GA_Running),
      CA_Running_Per_60 = CA_Running_Per_60 * -1,
      xGA_Running_Per_60 = xGA_Running_Per_60 * -1
    )
  
  
  #select max N for each team
  
  ultimate_ten <- ultimate_ten %>%
    
    group_by(Team, Strength) %>%
    
    filter(N == max(N)) %>%
    
    pivot_longer(-c("Team", "Strength", "N"),
                 names_to = "Measure",
                 values_to = "U10_Value")
  
  
  #select penultimate ten games
  
  penultimate_ten <- all_situations %>%
    
    select(Team = Team_Short, Date, Strength, TOI, CF, CA, FF, FA, xGF, xGA, GF, GA) %>%
    
    arrange(Team, Date, Strength) %>%
    
    group_by(Team, Strength) %>%
    
    mutate(N = row_number()) %>%
    
    group_by(Team, Date) %>%
    
    mutate(N = max(N)) %>%
    
    ungroup() %>%
    
    select(-Date) %>%
    
    group_by(Team, Strength) %>%
    
    filter(N > max(N) - 20 & N < max(N) - 9)
  
  
  #subset measures and calc running sums for penultimate ten games
  
  penultimate_ten <- penultimate_ten %>%
    
    select(Team, Strength, N, TOI, CF, CA, FF, FA, xGF, xGA, GF, GA) %>%
    
    arrange(Team, N, Strength) %>%
    
    group_by(Team, Strength) %>%
    
    mutate_at(vars(-group_cols()), funs(Running = cumsum)) %>%
    
    select(Team:N, TOI_Running:GA_Running)
  
  
  #additional penultimate 10 game calcs
  
  penultimate_ten <- penultimate_ten %>%
    
    group_by(Team, Strength) %>%
    
    mutate_at(
      vars(
        CF_Running,
        CA_Running,
        xGF_Running,
        xGA_Running,
        GF_Running,
        GA_Running
      ),
      funs(Per_60 = . / TOI_Running * 60)
    ) %>%
    
    mutate(
      CFS_Running = CF_Running / (CF_Running + CA_Running),
      xGS_Running = xGF_Running / (xGF_Running + xGA_Running),
      GS_Running = GF_Running / (GF_Running + GA_Running),
      xFSh_Running = xGF_Running / FF_Running,
      xFSv_Running = 1 - (xGA_Running / FA_Running),
      FSh_Running = GF_Running / FF_Running,
      FSv_Running = 1 - (GA_Running / FA_Running),
      GFAE_Running = GF_Running - xGF_Running,
      GAAE_Running = xGA_Running - GA_Running,
      GDiffAE_Running = (GF_Running - xGF_Running) + (xGA_Running - GA_Running),
      CA_Running_Per_60 = CA_Running_Per_60 * -1,
      xGA_Running_Per_60 = xGA_Running_Per_60 * -1
    )
  
  
  #select max N for each team
  
  penultimate_ten <- penultimate_ten %>%
    
    group_by(Team, Strength) %>%
    
    filter(N == max(N)) %>%
    
    pivot_longer(-c("Team", "Strength", "N"),
                 names_to = "Measure",
                 values_to = "PU10_Value")
  
  
  #combine data frames into master
  
  all_situations <- cbind(season, ultimate_ten, penultimate_ten)
  
  all_situations <- all_situations[c(1:5, 10, 15)]
  
  
  #add zcores for relevant measures
  
  all_situations <- all_situations %>%
    
    group_by(Strength, Measure) %>%
    
    mutate_at(vars(Season_Value, U10_Value, PU10_Value),
              funs(Z_Score = scale))
  
  #prepare all_situations data for use in charts
  
  #select relevant measures
  
  team_charts_5v5_off <- all_situations %>%
    
    filter(Strength == "5v5") %>%
    
    filter(
      Measure == "CF_Running_Per_60" |
        Measure == "xFSh_Running" | Measure == "xGF_Running_Per_60"
    ) %>%
    
    mutate(Group = "5v5 Offense")
  
  
  team_charts_5v5_def <- all_situations %>%
    
    filter(Strength == "5v5") %>%
    
    filter(
      Measure == "CA_Running_Per_60" |
        Measure == "xFSv_Running" | Measure == "xGA_Running_Per_60"
    ) %>%
    
    mutate(Group = "5v5 Defense")
  
  
  team_charts_5v5_total <- all_situations %>%
    
    filter(Strength == "5v5") %>%
    
    filter(Measure == "CFS_Running" | Measure == "xGS_Running") %>%
    
    mutate(Group = "5v5 Total")
  
  
  team_charts_5v5_results <- all_situations %>%
    
    filter(Strength == "5v5") %>%
    
    filter(Measure == "GFAE_Running" | Measure == "GAAE_Running") %>%
    
    mutate(Group = "5v5 Results")
  
  
  team_charts_5v4_off <- all_situations %>%
    
    filter(Strength == "5v4") %>%
    
    filter(
      Measure == "CF_Running_Per_60" |
        Measure == "xFSh_Running" | Measure == "xGF_Running_Per_60"
    ) %>%
    
    mutate(Group = "5v4 Offense")
  
  
  team_charts_4v5_def <- all_situations %>%
    
    filter(Strength == "4v5") %>%
    
    filter(
      Measure == "CA_Running_Per_60" |
        Measure == "xFSv_Running" | Measure == "xGA_Running_Per_60"
    ) %>%
    
    mutate(Group = "4v5 Defense")
  
  
  team_charts_st_results_off <- all_situations %>%
    
    filter(Strength == "5v4") %>%
    
    filter(Measure == "GFAE_Running") %>%
    
    mutate(Group = "ST Results")
  
  
  team_charts_st_results_def <- all_situations %>%
    
    filter(Strength == "4v5") %>%
    
    filter(Measure == "GAAE_Running") %>%
    
    mutate(Group = "ST Results")
  
  
  team_charts <-
    rbind(
      team_charts_5v5_off,
      team_charts_5v5_def,
      team_charts_5v5_total,
      team_charts_5v5_results,
      team_charts_5v4_off,
      team_charts_4v5_def,
      team_charts_st_results_off,
      team_charts_st_results_def
    )
  
  
  #add verbose measure names
  
  measures <- data.frame(unique(team_charts$Measure))
  
  colnames(measures) <- "Measure"
  
  measures$Verbose <-
    c(
      "Shot Generation",
      "Expected Goal Generation",
      "Shot Danger Generation",
      "Shot Suppression",
      "Expected Goal Suppression",
      "Shot Danger Suppression",
      "Shot Share",
      "Expected Goal Share",
      "Goals Scored Above Exp.",
      "Goals Saved Above Exp."
    )
  
  measures$Measure_Order <- c(1, 3, 2, 5, 6, 5, 7, 8, 9, 10)
  
  team_charts <-
    
    left_join(team_charts, measures, by = c("Measure"))
  
  
  #add group ordering
  
  groups <- data.frame(unique(team_charts$Group))
  
  colnames(groups) <- "Group"
  
  groups$Group_Order <- c(1, 2, 3, 4, 5, 6, 7)
  
  team_charts <-
    
    left_join(team_charts, groups, by = c("Group"))
  
  
  #reorder teams by 5v5 xG%
  
  teams <- team_charts %>%
    
    filter(Strength == "5v5", Measure == "xGS_Running") %>%
    
    arrange(-Season_Value) %>%
    
    mutate(Team_Order = row_number()) %>%
    
    ungroup() %>%
    
    select(Team, Team_Order)
  
  
  team_charts <-
    
    left_join(team_charts, teams, by = c("Team")) %>%
    
    arrange(Team_Order, Group_Order, Measure_Order) %>%
    
    ungroup() %>%
    
    mutate(
      Team = factor(Team, levels = unique(Team)),
      Group = factor(Group, levels = unique(Group)),
      Measure = factor(Measure, levels = unique(Measure))
    )
  
  
  return(list(all_situations, season, team_charts))
  
}
