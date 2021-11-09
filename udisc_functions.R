# hard coded variables
# ------------------------------------------------------------------------------
scores_lag <- 3                                                                 # amount of lag between time name appears in results data and when scores for holes appear
hole_indicators <- c("MAP","VIEW MAP")                                          # Udisc uses these values to view hole maps, used to determine how many holes are on course
results_files <- list.files(path = here("data/Results/"))


# udisc_scores function, process raw excel file into usable score file
# ------------------------------------------------------------------------------
udisc_scores <- function(results_file) {
  
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
  
  round_results_filtered
}
