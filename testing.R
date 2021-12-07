install.packages("ggridges")
library(ggridges)

hole_scores %>% 
  group_by(player, tournament_short, course, round) %>% 
  mutate(player_holes_in_round = n()) %>% 
  group_by(tournament_short, course, round) %>% 
  mutate(median_holes_in_round = median(player_holes_in_round)) %>% 
  filter(
    player_holes_in_round >= median_holes_in_round
    ,hole <= median_holes_in_round
  ) %>% 
  group_by(player, tournament_short, course, round) %>% 
  summarize(total_par_score = sum(par_score)) %>% 
  group_by(tournament_short, course, round) %>% 
  mutate(median_course_round = median(total_par_score)) %>% 
  ggplot(
    aes(x = total_par_score
        ,y = reorder(course,desc(median_course_round))
        ,fill = reorder(course,desc(median_course_round))
    )
  ) +
  #geom_density_ridges(show.legend = FALSE)
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, show.legend = F) +
  ggtitle("Round Score Distribution by Course") +
  labs(x = NULL, y = NULL) +
  theme_light()

hole_scores %>%
  group_by(course, hole) %>% 
  summarise(
    avg_par_score = round(mean(par_score),2)
    ,sd_par_score = round(sd(par_score),2)
    ) %>%
  as_tibble() %>% 
  arrange(desc(avg_par_score)) %>%
  slice_head(n = 10) %>%
  flextable() %>% 
  theme_zebra()

hole_scores %>%
  group_by(course, hole) %>% 
  summarise(avg_par_score = mean(par_score)) %>% 
  group_by(course) %>% 
  mutate(course_avg_par_score = mean(avg_par_score)) %>% 
  ggplot(aes(x = hole, y = reorder(course,desc(course_avg_par_score)), fill= avg_par_score)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdBu") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks=c(1:18), labels=c(1:18),limits=c(1,18))
