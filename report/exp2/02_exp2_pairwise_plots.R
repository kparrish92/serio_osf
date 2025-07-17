# This script runs the pairwise comparisons using a custom function and generates 
# the plots

source(here::here("report", "exp2", "00_pairwise_comp_function.R"))

b2 = readRDS(here("data", "models", "exp2_model.rds"))
senses_tidy = read.csv(here("data", "tidy", "senses_tidy.csv")) 


### Pairwise comparisons 
answer1 = c("Is", "Is not", "Both")
category1 = c("Value", "Artifact", "Natural")

df_source = crossing(answer1,category1) %>% 
  mutate(cat_c = paste0(answer1,"_",category1))


comp_list = list()  

for (i in 1:9) {
  this_df = df_source %>% 
    mutate(answer2 = df_source$answer1[i]) %>% 
    mutate(category2 = df_source$category1[i])
  comp_list[[i]] = this_df
}


combos = do.call(rbind, comp_list) 

exp2_mod = b2

rope_for_data = .2

list_pdf = list()
for (it in 1:nrow(combos)) {
  pdf = create_pairwise_df(combos$answer1[it], combos$category1[it],
                           combos$answer2[it], combos$category2[it], rope = rope_for_data) %>% 
    mutate(combo1 = paste0(combos$answer1[it], "_", combos$category1[it])) %>% 
    mutate(combo2 = paste0(combos$answer2[it], "_", combos$category2[it]))
  list_pdf[[it]] = pdf
}

all_data = do.call(rbind,list_pdf)


d = all_data %>% group_by(combo1, combo2) %>% 
  summarize(Effect = mean(effect), 
            Pct_rope = sum(in_rope)/4000) 

rope = rope_for_data

## Both across categories 
all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Both_Artifact_Both_Natural" | comps == "Both_Artifact_Both_Value" |
         comps == "Both_Natural_Both_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "red", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Both_Artifact_Both_Natural"="Artifact - Natural Kind",
                            "Both_Artifact_Both_Value"="Artifact - Value", 
                            "Both_Natural_Both_Value"="Natural Kind - Value"))

create_pairwise_table("Both_Artifact_Both_Natural", "Both_Artifact_Both_Value", "Both_Natural_Both_Value") %>% 
  write.csv(here("data", "tidy", "exp2_posthoc_table_both.csv"))
  


ggsave("both_cond_plots.png", path = here("report", "exp2", "figs"))


## Is across categories 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Artifact_Is_Natural" | comps == "Is_Artifact_Is_Value" |
           comps == "Is_Natural_Is_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "green", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Is_Artifact_Is_Natural"="Artifact - Natural Kind",
                            "Is_Artifact_Is_Value"="Artifact - Value", 
                            "Is_Natural_Is_Value"="Natural Kind - Value"))

create_pairwise_table("Is_Artifact_Is_Natural", "Is_Artifact_Is_Value", "Is_Natural_Is_Value") %>% 
  write.csv(here("data", "tidy", "exp2_posthoc_table_is.csv"))

ggsave("is_cond_plots.png", path = here("report", "exp2", "figs"))



lists = all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) 

unique(lists$comps)

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is not_Artifact_Is not_Natural" | comps == "Is not_Artifact_Is not_Value" |
           comps == "Is not_Natural_Is not_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "blue", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Is not_Artifact_Is not_Natural"="Artifact - Natural Kind",
                            "Is not_Artifact_Is not_Value"="Artifact - Value", 
                            "Is not_Natural_Is not_Value"="Natural Kind - Value"))

create_pairwise_table("Is not_Artifact_Is not_Natural", "Is not_Artifact_Is not_Value", 
                      "Is not_Natural_Is not_Value") %>% 
  write.csv(here("data", "tidy", "exp2_posthoc_table_isnot.csv"))

ggsave("isnot_cond_plots.png", path = here("report", "exp2", "figs"))

# Both, is, is not within natural kinds 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Natural_Both_Natural" | comps == "Is_Natural_Is not_Natural" |
           comps == "Both_Natural_Is not_Natural") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "purple", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Is_Natural_Both_Natural"="Is - Both",
                            "Is_Natural_Is not_Natural"="Is - Is not", 
                            "Both_Natural_Is not_Natural"="Both - Is not"))


all_data %>% 
  mutate(Comparison = paste0(combo1,"_", combo2)) %>% 
  filter(Comparison == "Is_Natural_Both_Natural" | Comparison == "Is_Natural_Is not_Natural" |
           Comparison == "Both_Natural_Is not_Natural") %>% 
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
  arrange(desc(Comparison)) %>% 
  write.csv(here("data", "tidy", "exp2_posthoc_table_natural.csv"))

ggsave("within_nk.png", path = here("report", "exp2", "figs"))


# Both, is, is not within artifacts 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Artifact_Both_Artifact" | comps == "Is_Artifact_Is not_Artifact" |
           comps == "Both_Artifact_Is not_Artifact") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "yellow", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Is_Artifact_Both_Artifact"="Is - Both",
                            "Is_Artifact_Is not_Artifact"="Is - Is not", 
                            "Both_Artifact_Is not_Artifact"="Both - Is not"))


create_pairwise_table("Is_Artifact_Both_Artifact", "Is_Artifact_Is not_Artifact", 
                      "Both_Artifact_Is not_Artifact") %>% 
  write.csv(here("data", "tidy", "exp2_posthoc_table_artifact.csv"))


ggsave("within_art.png", path = here("report", "exp2", "figs"))


all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Value_Both_Value" | comps == "Is_Value_Is not_Value" |
           comps == "Both_Value_Is not_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "orange", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlim(-6,6) +
  xlab("Difference in Log-odds") +
  scale_y_discrete(labels=c("Is_Value_Both_Value"="Is - Both",
                            "Is_Value_Is not_Value"="Is - Is not", 
                            "Both_Value_Is not_Value"="Both - Is not"))

create_pairwise_table("Is_Value_Both_Value", "Is_Value_Is not_Value", 
                      "Both_Value_Is not_Value") %>% 
  write.csv(here("data", "tidy", "exp2_posthoc_table_value.csv"))

ggsave("within_value.png", path = here("report", "exp2", "figs"))



# Both, is, is not within value



## Collapsed plots 
all_data %>% 
  filter(combo1 == "Both_Artifact") %>% 
  ggplot(aes(y = combo2, x = effect, color = combo2)) +
  stat_pointinterval(alpha = .5, position = position_dodge(width = .5)) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position ="none") +
  ggtitle("Effects relative to answers of both in the artifact condition") +
  scale_color_manual(values=c("#56B4E9", "#56B4E9", "#56B4E9",
                              "#E69F00", "#E69F00", "#E69F00",
                              "#999999", "#999999", "#999999"))


ggsave("ba_coll.png", path = here("report", "exp2", "figs"))



posterior_pred = conditional_effects(exp2_mod, categorical = TRUE)[["CategoryType:cats__"]] %>% 
  select(effect1__, effect2__, estimate__, lower__, upper__) %>% 
  mutate(estimate__ = round(estimate__, digits = 2)) %>% 
  mutate(lower__ = round(lower__, digits = 2)) %>% 
  mutate(upper__ = round(upper__, digits = 2)) %>% 
  mutate(model_effect = paste0(estimate__, " [", lower__, " - ", upper__, "]")) %>% 
  rename("CategoryType" = "effect1__") %>% 
  select(CategoryType, effect2__, model_effect) %>% 
  pivot_wider(names_from = effect2__, values_from = model_effect)

