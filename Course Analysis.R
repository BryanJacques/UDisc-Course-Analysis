# load libraries
library(tidyverse)
library(here)
library(readxl)

# load top players
top_players <- read_csv(here("data/Top Players.csv"))

# load las vegas challenge
Las_Vegas_Challenge <- read_excel(
  here("data/Results/Las Vegas Challenge.xlsx")
  ,col_names = "Value") %>% 
  mutate(row_num = 1:length(Las_Vegas_Challenge$Value)) %>% 
  left_join(top_players, by = c("Value" = "Player_Name"))


# view(top_players)
# view(Las_Vegas_Challenge)