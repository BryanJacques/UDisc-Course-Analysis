UDisc Analysis
================
Bryan Jacques

# UDisc Player and Course Analysis

<img src="images\McBeth%20Throwing.jpg" style="width:70.0%" alt="Paul McBeth Throwing" />
<!-- allthingsdiscgolf.com -->

This analysis uses data from Udisc’s website, which records scores and
other data associated with professional disc golf tournaments played
throughout the year. It will assume the reader understands the basics of
disc golf, which is basically the same as traditional golf, but instead
of hitting balls into a cup the players throw discs into a basket.
Individual statistics will be examined, as well as statistics pertaining
to specific courses and holes.

This analysis will be limited to a subset of tournaments from the
professional schedule. The data for the tournaments was taken from
[here](https://udisclive.com/schedule). Results were copied and pasted
into excel, saved individually, and a script was made to compile all the
scores into one dataset (Compile\_Scores.R). Two other datasets were
manually created in excel to map the courses to the specific tournaments
and rounds they were played, and to track par and length for each hole
played (Data/Round\_Course\_Map.xlsx and Data/Course\_Hole\_Map.xlsx
respectively)

## Setup

The first thing we need to do is set up our environment to run the
analysis

``` r
library(tidyverse)
library(here)
library(readxl)

hole_scores <- read_csv(here("Data/Hole_Scores.csv"))
round_course_map <- read_excel(here("Data/Round_Course_Map.xlsx"))
course_hole_map <- read_excel(here("Data/Course_Hole_Map.xlsx"))

hole_scores <- hole_scores %>% 
  left_join(round_course_map, by = c("tournament_short","round")) %>% 
  left_join(course_hole_map, by = c("course","hole")) %>% 
  mutate(par_score = score_val - par)
```

Tournament round results were stored in dataset hole\_scores, the round
and course mapping was stored in round\_course\_map, and the length and
par information for each hole was stored in course\_hole\_map.
round\_course\_map and course\_hole\_map were joined to the hole\_scores
dataset to get course and length information for every hole played.
Let’s preview the hole\_scores dataset to make sure the data was
imported and joined properly:

``` r
glimpse(hole_scores)
```

Rows: 92,233 Columns: 11 $ hole <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
12, 13, 14, 15, 16\~ $ player <chr> “Paul McBeth”, “Paul McBeth”, “Paul
McBeth”, “Paul Mc\~ $ file
<chr>”DESMOINES1.xlsx“,”DESMOINES1.xlsx“,”DESMOINES1.xls\~ $ score\_val
<dbl> 4, 3, 2, 2, 3, 4, 2, 3, 3, 4, 4, 2, 3, 4, 3, 4, 2, 5,\~ $ round
<dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,\~ $
tournament\_full <chr> “Des Moines Challenge”, “Des Moines Challenge”,
“Des \~ $ tournament\_short <chr>”Des Moines“,”Des Moines“,”Des
Moines“,”Des Moines\~ $ course <chr> “Pickard Park”, “Pickard Park”,
“Pickard Park”, "Pick\~ $ distance <dbl> 795, 753, 320, 490, 330, 633,
345, 540, 290, 844, 810\~ $ par <dbl> 4, 4, 3, 3, 3, 4, 3, 3, 3, 5, 4,
3, 3, 4, 4, 3, 3, 5,\~ $ par\_score <dbl> 0, -1, -1, -1, 0, 0, -1, 0, 0,
-1, 0, -1, 0, 0, -1, 1\~
