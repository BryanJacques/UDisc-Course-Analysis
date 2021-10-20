
# geom_histogram and similar
# ------------------------------------------------------------------------------

# geom_histogram
bjj_geom_histogram <- 
  hole_scores %>% 
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
                   ,bins = 15
                   ,alpha = .99
                   ,position = "identity" # dodge, stack, fill, jitter, identity
                   #, linetype
                   #, boundary   = 2 # not sure what this does
                   #, binwidth   = 2
                   ) +
    facet_wrap(~ course, nrow = 7)

# geom_bar
bjj_geom_bar <- 
  hole_scores %>% 
  ggplot() +
  geom_bar(
    mapping = aes(x = par_score)
    #,fill = "purple"
    ,fill = rgb(92,112,124,max=255)
    ,width = 1
    ) +
  facet_grid(round ~ tournament_short) 



# show bjj_plots
# ------------------------------------------------------------------------------
bjj_geom_histogram
bjj_geom_bar

# geom_freqpoly()
# geom_density()
# geom_dotplot()
# view(hole_scores)