### load required packages

library("tidyverse") 
library("rvest")


### scrape all single season skater data from hockey reference


years <- setdiff(1918:2007, 2005)


site <-
  paste0("https://hockey-reference.com/leagues/NHL_",
         years,
         "_skaters.html")

dflist <- lapply(site, function(i) {
  
  page <- read_html(i)
  
  ps.table <- html_nodes(page, "table")
  
  ps <- html_table(ps.table) [[1]]
  
})

dflist1 <- Map(cbind, dflist, years)

dflist1 <- lapply(dflist1, setNames, nm = dflist1[[1]] [1,])

player.stats1 <- do.call(rbind, dflist1)


years <- seq(2008, 2020)

site <- paste0("https://hockey-reference.com/leagues/NHL_", years, "_skaters.html")

dflist <- lapply(site, function(i) {
  
  page <- read_html(i)
  
  ps.table <- html_nodes(page, "table")
  
  ps <- html_table(ps.table) [[1]]

  })

dflist2 <- Map(cbind, dflist, years)

dflist2 <- lapply(dflist2, setNames, nm = dflist2[[1]] [1, ])


player.stats2 <- do.call(rbind, dflist2)

player.stats2 <- player.stats2[c(1:23, 29)]


colnames(player.stats2) [17:19] <- c("EV", "PP", "SH")

colnames(player.stats1) [24] <- "Season"

colnames(player.stats2) [24] <- "Season"

player.stats <- rbind(player.stats1, player.stats2)

colnames(player.stats) [17:19] <- c("EV.A", "PP.A", "SH.A")

player.stats <- player.stats %>% 
  
  filter(Rk != "Rk", Tm != "TOT")
