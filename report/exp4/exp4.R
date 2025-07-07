library(here)
library(tidyverse)
library(brms)
exp4 = read.csv(here("data", "exp4_data.csv"))
exp4 <- tibble::rownames_to_column(exp4, "Participant")

PivotData <- pivot_longer(exp4, Water:Shovel, names_to="Item", values_to="Selection")
FinalData <- PivotData %>% mutate(CategoryType = case_when(
  Item == "Water" ~ "Natural",
  Item == "Tiger" ~ "Natural",
  Item == "Gold" ~ "Natural",
  Item == "Raccoon" ~ "Natural",
  Item == "Helium" ~ "Natural",
  Item == "Zebra" ~ "Natural",
  Item == "Fork" ~ "Artifact",
  Item == "Kettle" ~ "Artifact",
  Item == "Chair" ~ "Artifact",
  Item == "Razor" ~ "Artifact",
  Item == "Harpoon" ~ "Artifact",
  Item == "Shovel" ~ "Artifact",
  Item == "Heavy.Metal" ~ "Value",
  Item == "Scientist" ~ "Value",
  Item == "Mentor" ~ "Value",
  Item == "Argument" ~ "Value",
  Item == "Artist" ~ "Value",
  Item == "Minister" ~ "Value",
  Item == "Spicy" ~ "Vague",
  Item == "Tall" ~ "Vague",
  Item == "Square" ~ "Definite",
  Item == "Gallon" ~ "Definite",
  Item == "Comedian" ~ "Comedian",
  Item == "Honeybees" ~ "Honeybees",
  Item == "Clock" ~ "Clock"))

FinalData$CategoryType = as.factor(FinalData$CategoryType)
FinalData$CategoryType = relevel(FinalData$CategoryType, ref = "Value")

FinalData %>% 
  write.csv(here("data", "tidy", "exp4_tidy.csv"))

unique(FinalData$CategoryType)

exp2mod_for_priors = readRDS(here("data", "models", "exp2_model.rds"))


#b2 <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | Item), 
#          prior = prior_check,
#          data=FinalData,
#          family="categorical",
#          file = here("data", "models", "exp4_model.rds"))


prior_check = c(set_prior("normal(-1, 0.5)", class = "Intercept", dpar = "muIs"),
                set_prior("normal(.5, 0.5)", class = "Intercept", dpar = "muIsnot"),
                set_prior("normal(-1.77, 0.5)", class = "b", dpar = "muIs", coef = "CategoryTypeArtifact"),
                set_prior("normal(-1.25, 0.5)", class = "b", dpar = "muIs", coef = "CategoryTypeNatural"),
                set_prior("normal(.6, 0.5)", class = "b", dpar = "muIsnot", coef = "CategoryTypeArtifact"),
                set_prior("normal(-6.98, 0.5)", class = "b", dpar = "muIsnot", coef = "CategoryTypeNatural"))


b4_s <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | Item), 
            data= FinalData %>% filter(CategoryType == "Natural" | CategoryType == "Value" | CategoryType == "Artifact"),
            family="categorical",
            file = here("data", "models", "exp4_model_s.rds"))

b4 <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | Item), 
            data= FinalData %>% filter(CategoryType == "Natural" | CategoryType == "Value" | CategoryType == "Artifact"),
            family="categorical",
            prior = prior_check,
            file = here("data", "models", "exp4_model.rds"))


conditional_effects(b4_s, categorical = TRUE)

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

rope_for_data = .2

FinalData %>% 
  filter(CategoryType == "Natural" | 
           CategoryType == "Value" | CategoryType == "Artifact") %>% 
  write.csv(here("data", "tidy", "exp4_filtered.csv"))


senses_tidy = read.csv(here("data", "tidy", "exp4_filtered.csv"))


unique(senses_tidy$CategoryType)

exp2_mod = b4

list_pdf = list()
for (it in 1:nrow(combos)) {
  pdf = create_pairwise_df(combos$answer1[it], combos$category1[it],
                           combos$answer2[it], combos$category2[it], rope = rope_for_data) %>% 
    mutate(combo1 = paste0(combos$answer1[it], "_", combos$category1[it])) %>% 
    mutate(combo2 = paste0(combos$answer2[it], "_", combos$category2[it]))
  list_pdf[[it]] = pdf
}

all_data = do.call(rbind,list_pdf)

rope = rope_for_data

## Both across categories 
all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Both_Artifact_Both_Natural" | comps == "Both_Artifact_Both_Value" |
           comps == "Both_Natural_Both_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "darkviolet", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-8,8) +
  scale_y_discrete(labels=c("Both_Artifact_Both_Natural"="Artifact - Natural Kind",
                            "Both_Artifact_Both_Value"="Artifact - Value", 
                            "Both_Natural_Both_Value"="Natural Kind - Value"))

create_pairwise_table("Both_Artifact_Both_Natural", "Both_Artifact_Both_Value", "Both_Natural_Both_Value") %>% 
  write.csv(here("data", "tidy", "exp4_posthoc_table_both.csv"))

ggsave("both_cond_plots.png", path = here("report", "exp4", "figs"))

## Is across categories 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Artifact_Is_Natural" | comps == "Is_Artifact_Is_Value" |
           comps == "Is_Natural_Is_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "darkseagreen", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-8,8) +
  scale_y_discrete(labels=c("Is_Artifact_Is_Natural"="Artifact - Natural Kind",
                            "Is_Artifact_Is_Value"="Artifact - Value", 
                            "Is_Natural_Is_Value"="Natural Kind - Value"))

create_pairwise_table("Is_Artifact_Is_Natural", "Is_Artifact_Is_Value", "Is_Natural_Is_Value") %>% 
  write.csv(here("data", "tidy", "exp4_posthoc_table_is.csv"))

ggsave("is_cond_plots.png", path = here("report", "exp4", "figs"))


# In not post hoc

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is not_Artifact_Is not_Natural" | comps == "Is not_Artifact_Is not_Value" |
           comps == "Is not_Natural_Is not_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "cyan2", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-8,8) +
  scale_y_discrete(labels=c("Is not_Artifact_Is not_Natural"="Artifact - Natural Kind",
                            "Is not_Artifact_Is not_Value"="Artifact - Value", 
                            "Is not_Natural_Is not_Value"="Natural Kind - Value"))

create_pairwise_table("Is not_Artifact_Is not_Natural", "Is not_Artifact_Is not_Value", 
                      "Is not_Natural_Is not_Value") %>% 
  write.csv(here("data", "tidy", "exp4_posthoc_table_isnot.csv"))

ggsave("isnot_cond_plots.png", path = here("report", "exp4", "figs"))

# natural kinds 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Natural_Both_Natural" | comps == "Is_Natural_Is not_Natural" |
           comps == "Both_Natural_Is not_Natural") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "darkmagenta", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-8,8) +
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
  write.csv(here("data", "tidy", "exp4_posthoc_table_natural.csv"))

ggsave("within_nk.png", path = here("report", "exp4", "figs"))


#  artifacts 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Artifact_Both_Artifact" | comps == "Is_Artifact_Is not_Artifact" |
           comps == "Both_Artifact_Is not_Artifact") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "cornsilk4", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-8,8) +
  scale_y_discrete(labels=c("Is_Artifact_Both_Artifact"="Is - Both",
                            "Is_Artifact_Is not_Artifact"="Is - Is not", 
                            "Both_Artifact_Is not_Artifact"="Both - Is not"))


create_pairwise_table("Is_Artifact_Both_Artifact", "Is_Artifact_Is not_Artifact", 
                      "Both_Artifact_Is not_Artifact") %>% 
  write.csv(here("data", "tidy", "exp4_posthoc_table_artifact.csv"))

ggsave("within_art.png", path = here("report", "exp4", "figs"))

# Value 

all_data %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Is_Value_Both_Value" | comps == "Is_Value_Is not_Value" |
           comps == "Both_Value_Is not_Value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "darksalmon", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlim(-8,8) +
  xlab("Difference in Log-odds") +
  scale_y_discrete(labels=c("Is_Value_Both_Value"="Is - Both",
                            "Is_Value_Is not_Value"="Is - Is not", 
                            "Both_Value_Is not_Value"="Both - Is not"))

create_pairwise_table("Is_Value_Both_Value", "Is_Value_Is not_Value", 
                      "Both_Value_Is not_Value") %>% 
  write.csv(here("data", "tidy", "exp4_posthoc_table_value.csv"))

ggsave("within_value.png", path = here("report", "exp4", "figs"))

length(unique(FinalData$Participant))
unique(FinalData$CategoryType)

FinalData %>% 
  group_by(Participant) %>% 
  summarise(n = n())
