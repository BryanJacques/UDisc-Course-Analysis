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


# add bounceback scores (score after current hole)
# ------------------------------------------------------------------------------

hole_scores_w_prior <- hole_scores %>% 
  mutate(prior_hole = hole - 1) %>% 
  select(p_player = player
         ,p_tournament_short = tournament_short
         ,p_round = round
         ,bounceback_par_score = par_score
         ,prior_hole)

hole_scores <- hole_scores %>% 
  left_join(
    hole_scores_w_prior
    ,by = c("player" = "p_player"
            ,"tournament_short" = "p_tournament_short"
            ,"round" = "p_round"
            ,"hole" = "prior_hole")
            )

rm(hole_scores_w_prior)


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
    ,bird_or_better = sum(if_else(par_score < 0,1,0))
    ,bird_rate = bird_or_better / holes_played
    ,bogey_or_worse = sum(if_else(par_score > 0,1,0))
    ,bogey_rate = bogey_or_worse / holes_played
    ,bounceback_birds = sum(bounceback_par_score < 0 & par_score > 0, na.rm = T)
    ,bounceback_opps = sum(!is.na(bounceback_par_score) & par_score > 0)
    ,bounceback_rate = bounceback_birds / bounceback_opps
    ,bounceback_gap = bounceback_rate - bird_rate
  )



# diagnose
# ------------------------------------------------------------------------------

# view(hole_scores)
# view(round_course_map)
# view(course_hole_map)
# view(player_summary)
