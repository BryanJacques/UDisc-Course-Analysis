# load libraries
  library(tidyverse)
  library(here)
  library(readxl)
  
  rm(list = ls())

  
# hard coded variables
  scores_lag <- 3                                                               # amount of lag between time name appears in results data and when scores for holes appear
  hole_indicators <- c("MAP","VIEW MAP")                                        # Udisc uses these values to view hole maps, used to determine how many holes are on course
  results_files <- list.files(path = here("data/Results/"))


# hard coded all_rounds table, will append individual results table to this

  all_rounds <- tibble(
    hole = integer()
    ,score = character()
    ,player = character()
    ,file = character()
  )


# process results files, convert to scores

  for (file_ in results_files) {
    
    results_file <- str_c(here("data/Results//"),file_)
    
    # load round results
    round_results <- read_excel(
      results_file
      ,col_names = "value") %>% 
      filter(!is.na(`value`)) 
    
    # add fields, including has_space to detect if player name
    round_results <- round_results %>% 
      mutate(
        row_num = 1:n()
        ,has_space = str_detect(`value`, " ")
        ,file = file_
      )
    
    # get hole count for course
    hole_count <- round_results %>% 
      select(`value`) %>%
      filter(str_to_upper(`value`) %in% hole_indicators) %>%
      NROW()
    
    
    # add name rank and dummy cols to round_results
    round_results <- round_results %>% 
      mutate(hole = NA
             ,score = NA
             ,player = NA)
    
    
    # get scores
    for (item in 1:nrow(round_results)) {
      if (round_results[item, "has_space"] == T) {
        
        row_num <- round_results[item, "row_num"]
        player <- round_results[item, "value"]
        
        for(hole in 1:hole_count) {
          select_row <- as.integer(row_num + scores_lag + hole)
          
          round_results[select_row, "hole"] <- hole
          round_results[select_row,"score"] <- round_results[select_row,"value"]
          round_results[select_row,"player"] <- player
        }
        
      }
      
    }
    
    round_results_filtered <- round_results %>% 
      filter(!is.na(score)) %>% 
      select(hole, score, player, file)
    
    all_rounds <- all_rounds %>% 
      bind_rows(round_results_filtered)
  }


# add fields to all rounds

  # add round
  all_rounds <- all_rounds %>% 
    mutate(
      round = case_when(
        str_detect(file,"1") ~ 1
        ,str_detect(file,"2") ~ 2
        ,str_detect(file,"3") ~ 3
        ,str_detect(file,"4") ~ 4
      )
    )

  # add full tournament name
  all_rounds <- all_rounds %>% 
    mutate(
      tournament_full = case_when(
        str_detect(file,"LEDGESTONE") ~ "Discraft Ledgestone Insurance Open"
        ,str_detect(file,"MVP") ~ "MVP Open at Maple Hill"
        ,str_detect(file,"TEXAS") ~ "Texas State Disc Golf Championship"
        ,str_detect(file,"LVCR") ~ "Las Vegas Challenge"
        ,str_detect(file,"DESMOINES") ~ "Des Moines Challenge"
        ,str_detect(file,"WACO") ~ "Waco Annual Charity Open"
        ,str_detect(file,"GMC") ~ "Green Mountain Championship"
        ,str_detect(file,"DGLO") ~ "Discraft Great Lakes Open"
        ,str_detect(file,"PRESERVE") ~ "The Preserve Championship"
        ,str_detect(file,"JONESBORO") ~ "Jonesboro Open"
        ,str_detect(file,"OTB") ~ "OTB Open"
        ,str_detect(file,"IDLEWILD") ~ "Idlewild Open"
        ,str_detect(file,"PORTLAND") ~ "Portland Open"
      )
    )
  
  # add short tournament name
  all_rounds <- all_rounds %>% 
    mutate(
      tournament_short = case_when(
        str_detect(file,"LEDGESTONE") ~ "Ledgestone"
        ,str_detect(file,"MVP") ~ "MVP Open"
        ,str_detect(file,"TEXAS") ~ "Texas State"
        ,str_detect(file,"LVCR") ~ "LV Challenge"
        ,str_detect(file,"DESMOINES") ~ "Des Moines"
        ,str_detect(file,"WACO") ~ "Waco"
        ,str_detect(file,"GMC") ~ "GMC"
        ,str_detect(file,"DGLO") ~ "DGLO"
        ,str_detect(file,"PRESERVE") ~ "Preserve"
        ,str_detect(file,"JONESBORO") ~ "Jonesboro"
        ,str_detect(file,"OTB") ~ "OTB Open"
        ,str_detect(file,"IDLEWILD") ~ "Idlewild"
        ,str_detect(file,"PORTLAND") ~ "Portland"
      )
    )

  
# output file
  
  write_csv(all_rounds, file = here("data/all_rounds.csv"))


# diagnose
  
  view(all_rounds)
  
  all_rounds %>% 
    group_by(file) %>% 
    summarize(records = n()) %>% 
    arrange(desc(records)) %>% 
    view(title = "by file")
  
  all_rounds %>% 
    group_by(player) %>% 
    summarize(records = n()) %>% 
    arrange(records) %>% 
    view(title = "by player")
