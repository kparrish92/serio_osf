### Sim the truth and let's see what happens. 



sim_trial = function(probs)
{  
trial = rmultinom(n=1, size=1, prob=probs) %>% as.data.frame() %>% 
  mutate(answer = c("is", "is_not", "both")) %>% 
  filter(V1 == 1)

return(trial)
}

sim_ppt = function(ppt_no)
{
trials_list = list()
for (i in 1:6) {
df_s = sim_trial(c(.1,.35,.55))
df_s$rep = i
df_s$category = "natural_kind"
trials_list[[i]] = df_s
}
nkdf = do.call(rbind,trials_list)

trials_list_abs = list()
for (i in 1:6) {
  df_s2 = sim_trial(c(.2,.2,.6))
  df_s2$rep = i
  df_s2$category = "abstract"
  trials_list_abs[[i]] = df_s2
}
abdf = do.call(rbind,trials_list_abs)

trials_list_art = list()
for (i in 1:6) {
  df_s3 = sim_trial(c(.05,.35,.55))
  df_s3$rep = i
  df_s3$category = "artifact"
  trials_list_art[[i]] = df_s3
}
artdf = do.call(rbind,trials_list_art)
fdf = rbind(artdf, nkdf, abdf)
fdf$participant = ppt_no

return(fdf)
}


ppt_no = c(1:48)

big_list = list()
for (it in 1:48) {
tdf = sim_ppt(ppt_no[it])  
big_list[[it]] = tdf
}
sim_df = do.call(rbind, big_list)


sim_df %>% 
  group_by(category, answer) %>% 
  summarize(n = n()/288)


ppt_no


b2_sim <- brm(answer ~ category + (category | participant), 
          prior = prior_check,   
          data=sim_df,
          family="categorical")


d= get_prior(b2_sim)


conditional_effects(b2_sim, categorical = TRUE)

prior_check = c(set_prior("normal(-1, 1)", class = "Intercept", dpar = "muis"),
                set_prior("normal(0, 1)", class = "Intercept", dpar = "muisnot"))

(b2_sim)

p_data = sim_df %>% 
  group_by(answer, participant, category) %>% 
  summarize(n = n())

poisson_model_three <- brm(n ~ answer*category + (1 | participant), 
                           data=p_data,
                           family="poisson")



conditional_effects(b2_sim, categorical = TRUE)

conditional_effects(poisson_model_three)
