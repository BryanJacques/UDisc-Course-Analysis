# load libraries, clear environment, import data
# ------------------------------------------------------------------------------
  
library(tidyverse)
library(here)
library(readxl)

rm(list = ls())

hole_scores <- read_csv(here("Data/Hole_Scores.csv"))
round_course_map <- read_excel(here("Data/Round_Course_Map.xlsx"))
course_hole_map <- read_excel(here("Data/Course_Hole_Map.xlsx"))

hole_scores <- hole_scores %>% 
  left_join(round_course_map, by = c("tournament_short","round")) %>% 
  left_join(course_hole_map, by = c("course","hole")) %>% 
  mutate(par_score = score_val - par)


# create player summary
# ------------------------------------------------------------------------------

player_summary <- hole_scores %>% 
  group_by(player) %>% 
  summarize(
    events_played = n_distinct(tournament_short)
    ,holes_played = n()
    ,total_score = sum(score_val)
    ,avg_score = mean(score_val)
    ,median_score = median(score_val)
    ,total_par_score = sum(par_score)
    ,avg_par_score = mean(par_score)
    ,median_par_score = median(par_score)
  )


# diagnose
# ------------------------------------------------------------------------------

# view(hole_scores)
# view(round_course_map)
# view(course_hole_map)
# view(player_summary)
