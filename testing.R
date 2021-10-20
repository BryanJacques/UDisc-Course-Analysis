hole_scores %>% 
  filter(
    course %in% c("Glendoveer"
                  ,"Infinite Discs Course"
                  ,"Fox Run Meadows"
    )
  ) %>% 
  group_by(player, course, round) %>% 
  summarize(
    records = n()
    ,distinct_holes = n_distinct(hole)
    ,sum_par_score = sum(par_score)
    ,sum_score_val = sum(score_val)
  ) %>% 
  filter(distinct_holes == 18) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = sum_par_score, fill = course))

hole_scores %>% 
  filter(
    course %in% c("Glendoveer"
                  ,"Infinite Discs Course"
                  ,"Fox Run Meadows"
    )
  ) %>% 
  group_by(player, course, round) %>% 
  summarize(
    records = n()
    ,distinct_holes = n_distinct(hole)
    ,sum_par_score = sum(par_score)
    ,sum_score_val = sum(score_val)
  ) %>% 
  filter(distinct_holes == 18) %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = sum_par_score, fill = course)
                 ,color = "black"
                 #,binwidth = 2
                 ,bins = 15
                 #,boundary = 2      # not sure what this does
                 ,alpha = .5
                 #,linetype
                 ,position = "stack" # dodge, stack, fill, jitter, identity
                 )



#?geom_histogram
