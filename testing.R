library(tidyverse)
library(here)
library(readxl)

rm(list = ls())

source(here("udisc_functions.R"))

all_rounds <- tibble(
  hole = integer()
  ,score = character()
  ,player = character()
  ,file = character()
)

results_files <- list.files(path = here("data/Results/"))

for (file_ in results_files) {
  all_rounds <- all_rounds %>% 
    bind_rows(udisc_scores(file_))
}


# add fields to all rounds, make other modifications
# ------------------------------------------------------------------------------

# convert score to number, only keep valid scores
all_rounds <- all_rounds %>% 
  mutate(score_val = as.integer(score)) %>% 
  filter(score_val > 0, !is.na(score_val)) %>% 
  select(-score)

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

# find skipped rounds by players, eliminate from scores
skipped_rounds <- all_rounds %>% 
  group_by(player, tournament_short, round) %>% 
  summarize(holes_played = n()) %>% 
  filter(holes_played == 1)

all_rounds <- all_rounds %>% 
  anti_join(skipped_rounds, by = c("player" , "tournament_short", "round"))

# eliminate "Top X Players" and similar values from Player Scores
all_rounds <- all_rounds %>% 
  filter(
    !str_detect(str_to_upper(player), "TOP \\d")
    & !str_detect(str_to_upper(player), "VIEW MAP")
  )


# output file
# ------------------------------------------------------------------------------
write_csv(all_rounds, file = here("data/Hole_Scores.csv"))


# diagnose
# ------------------------------------------------------------------------------

# view(all_rounds)
# 
# all_rounds %>% 
#   group_by(file) %>% 
#   summarize(
#     records = n()
#     ,max_score = max(score_val,na.rm = T)
#     ) %>% 
#   arrange(desc(records)) %>% 
#   view(title = "by file")
# 
# all_rounds %>% 
#   group_by(player) %>% 
#   summarize(records = n()) %>% 
#   arrange(records) %>% 
#   view(title = "by player")

