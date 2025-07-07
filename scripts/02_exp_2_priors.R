
data_for_priors %>% 
  filter(CategoryType == "Abstract" | CategoryType == "Definite" | CategoryType == "Natural") %>% 
  select(CategoryType, predicted_answer, predicted_grp_prob) %>% 
  pivot_wider(names_from = predicted_answer, values_from = predicted_grp_prob)
  