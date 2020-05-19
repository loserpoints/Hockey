### load required packages

library("tidyverse") 
library("rvest")


### scrape standings data from hockey reference


years <- (1980:2019)

draft_site <-
  paste0("https://www.hockey-reference.com/draft/NHL_",
         years,
         "_entry.html")


dflist <- lapply(draft_site, function(x) {
  
  playoffs_page <- read_html(x)
  
  playoffs_table <- html_nodes(playoffs_page, "table")
  
  playoffs <- html_table(playoffs_table, fill = T) [[1]]
  
})


dflist1 <- Map(cbind, dflist, years)

dflist1 <- lapply(dflist1, setNames, nm = names(dflist1[[1]]))

draft <- do.call(rbind, dflist1)

colnames(draft) <- draft[1, ]

colnames(draft) [15] <- "GGP"

draft <- draft %>%
  
  filter(Team != "Team", !grepl("Round", Team)) %>%
  
  rename(Year = '1980')
  
