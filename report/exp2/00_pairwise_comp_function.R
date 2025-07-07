### a script to make a function for pairwise comps 

library(modelr)
library(tidybayes)

create_pairwise_plot = function(answer1, category1, answer2, category2, rope)
{

big_df = senses_tidy %>% 
  data_grid(CategoryType) %>%
  add_fitted_draws(exp2_mod, dpar = TRUE, category = "Selection",
                   re_formula = NA) %>% 
  mutate(log_odds = qlogis(.value))

# is not artifact vs both natural kinds 

comp_df_1 = big_df %>% 
  filter(CategoryType == category1 & Selection == answer1)

comp_df_2 = big_df %>% 
  filter(CategoryType == category2 & Selection == answer2)

effect = comp_df_2$log_odds - comp_df_1$log_odds 

df = data.frame(effect, comp = "Nk_both-art_isnot") %>% 
  mutate(in_rope = ifelse(abs(effect) < rope, 1,0))


plot = df %>% 
  ggplot(aes(y = comp, x = effect, fill = after_stat(abs(x) < rope))) +
  stat_halfeye() + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.y=element_blank(),legend.position="none") +
  geom_text(aes(x = mean(effect), label = paste("Most Plausible Effect:", 
                                                round(median(effect), digits = 2))),
            vjust = 2) +
  geom_text(aes(x = mean(effect), label = paste("% in ROPE: ", (sum(in_rope)/4000)*100)
  ), vjust = 3.5) + theme(text=element_text(size=12)) + 
  ggtitle(paste0("Pairwise comparison of ", answer1, "-", category1, " to ", 
                 answer2, "-", category2), subtitle = paste0("ROPE range: +/- ", rope, " log-odds"))

return(plot)
}
  

create_pairwise_df = function(answer1, category1, answer2, category2, rope)
{
  
  big_df = senses_tidy %>% 
    data_grid(CategoryType) %>%
    add_fitted_draws(exp2_mod, dpar = TRUE, category = "Selection",
                     re_formula = NA) %>% 
    mutate(log_odds = qlogis(.value))
  
  # is not artifact vs both natural kinds 
  
  comp_df_1 = big_df %>% 
    filter(CategoryType == category1 & Selection == answer1)
  
  comp_df_2 = big_df %>% 
    filter(CategoryType == category2 & Selection == answer2)
  
  effect = comp_df_2$log_odds - comp_df_1$log_odds 
  
  df = data.frame(effect, comp = "Nk_both-art_isnot") %>% 
    mutate(in_rope = ifelse(abs(effect) < rope, 1,0))
  
  return(df)
}

create_pairwise_table = function(comb1, comb2, comb3)
  
{
  table = all_data %>% 
    mutate(Comparison = paste0(combo1,"_", combo2)) %>% 
    filter(Comparison == comb1 | Comparison == comb2 |
             Comparison == comb3) %>% 
    group_by(Comparison) %>% 
    summarize(HDI = hdi(effect), mean_eff = mean(effect), 
              Percentage_in_rope = sum(in_rope)/4000) %>% 
    mutate(Compelling_difference = ifelse(Percentage_in_rope < .05, "Yes", "No")) %>% 
    mutate(Effect = paste0(round(mean_eff, digits = 2), 
                           " [",
                           round(HDI[,1], digits = 2),
                           " - ",
                           round(HDI[,2], digits = 2),
                           "]")) %>% 
    select(Comparison, Effect, Compelling_difference) %>% 
    arrange(desc(Comparison))
  return(table)
}



create_pairwise_df_exp3 = function(answer1, category1, answer2, category2, rope)
{
  
  big_df_3 = exp3_tidy %>% 
    data_grid(CategoryType) %>%
    add_fitted_draws(b3, dpar = TRUE, category = "Selection",
                     re_formula = NA) %>% 
    mutate(log_odds = qlogis(.value))
  
  # is not artifact vs both natural kinds 
  
  comp_df_1 = big_df_3 %>% 
    filter(CategoryType == category1 & Selection == answer1)
  
  comp_df_2 = big_df_3 %>% 
    filter(CategoryType == category2 & Selection == answer2)
  
  effect = comp_df_2$log_odds - comp_df_1$log_odds 
  
  df = data.frame(effect, comp = "Nk_both-art_isnot") %>% 
    mutate(in_rope = ifelse(abs(effect) < rope, 1,0))
  
  return(df)
}
