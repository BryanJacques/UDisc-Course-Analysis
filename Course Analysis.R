# load libraries
library(tidyverse)
library(here)
library(readxl)

rm(list = ls())

# hard coded variables
scores_lag <- 3                                                                 # amount of lag between time name appears in results data and when scores first appear, this is fixed
hole_indicators <- c("MAP","VIEW MAP")                                          # Udisc uses these values to view hole maps, used to determine how many holes are on course
results_file <- here("data/Results/WACO3.xlsx")

# load top players
top_players <- read_csv(here("data/Top Players.csv"))

# load las vegas challenge
LVCR <- read_excel(
  results_file
  ,col_names = "value") %>% 
  filter(!is.na(`value`)) 

# add fields, including has_space to detect if player name
LVCR <- LVCR %>% 
  mutate(
    row_num = 1:n()
    ,has_space = str_detect(`value`, " ")
    )

# get hole count for course
hole_count <- LVCR %>% 
  select(`value`) %>%
  filter(str_to_upper(`value`) %in% hole_indicators) %>%
  NROW()


# get name rank
name_rank <- LVCR %>% 
  filter(has_space) %>% 
  mutate(name_rank = 1:n()) %>% 
  select(row_num, name_rank)

# add name rank and dummy cols to LVCR
LVCR <- LVCR %>% 
  left_join(name_rank, by = "row_num") %>% 
  mutate(hole = NA
         ,score = NA
         ,player = NA)


# get scores
for (item in 1:nrow(LVCR)) {
  if (LVCR[item, "has_space"] == T) {
    
    row_num <- LVCR[item, "row_num"]
    player <- LVCR[item, "value"]

    for(hole in 1:hole_count) {
      select_row <- as.integer(row_num + scores_lag + hole)
      
      LVCR[select_row, "hole"] <- hole
      LVCR[select_row,"score"] <- LVCR[select_row,"value"]
      LVCR[select_row,"player"] <- player
    }
    
  }
  
}


# view(top_players)
 view(LVCR)