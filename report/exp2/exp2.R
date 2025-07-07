# This script uses the raw data from exp 1 to generate priors for experiment 2.
library(here)
library(tidyverse)
library(brms)
library(bayestestR)


# Note: the files need to be inside a folder called "tidy", which is inside a folder called "data". If it's somewhere else, you can adjust these lines to read the data.
nonmember_tidy = read.csv(here("data", "tidy", "nonmember_tidy.csv"))
member_tidy = read.csv(here("data", "tidy", "member_tidy.csv"))

# make a grouped dataframe to identify the total number of trials per main condition (denominator of probabiliy)
total_possible_nm_df = nonmember_tidy %>% 
  group_by(CategoryType) %>% 
  summarise(n = n())

total_possible_m_df = member_tidy %>% 
  group_by(CategoryType) %>% 
  summarise(n = n())

# assign them to an object 
nm_total = total_possible_nm_df$n[1] 
m_total = total_possible_m_df$n[1] 

# Mutate the original data conditionally 
## For nonmember, 1 and 2 are ratings are predicted to be "is" answers.
## 3,4,and 5 are predicted to be "both" answers.
## 6 and 7 are predicted to be "is not" answers.
nonmember_tidy_mutated = nonmember_tidy %>% 
  mutate(predicted_answer = case_when(
    Rating == 1 | Rating == 2 ~ "is",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is_not",
  ))
## For member, it is revered from nonmember:
## 1 and 2 are ratings are predicted to be "is not" answers.
## 3,4,and 5 are predicted to be "both" answers.
## 6 and 7 are predicted to be "is" answers.  
member_tidy_mutated = member_tidy %>% 
  mutate(predicted_answer = case_when(
    Rating == 1 | Rating == 2 ~ "is_not",
    Rating == 3 | Rating == 4 | Rating == 5 ~ "both",
    Rating == 6 | Rating == 7 ~ "is",
  ))

# calculate the proportion of predicted is_not, both, and is answers for both member and non member data.
nonmember_tidy_mutated_grouped = nonmember_tidy_mutated %>% 
  group_by(CategoryType, predicted_answer) %>% 
  summarise(n_a = n())

member_tidy_mutated_grouped = member_tidy_mutated %>% 
  group_by(CategoryType, predicted_answer) %>% 
  summarise(n_b = n())

# Join the member and nonmember data and calculate the proprotion of predicted responses relative to the total number of trials - our priors in probability.
data_for_priors = left_join(nonmember_tidy_mutated_grouped, member_tidy_mutated_grouped) %>% 
  mutate(predicted_grp_prob = (n_a + n_b)/sum(m_total, nm_total))


# Join the member and nonmember data and calculate the proprotion of predicted responses relative to the total number of trials - our priors in probability.
data_for_priors = left_join(nonmember_tidy_mutated_grouped, member_tidy_mutated_grouped) %>% 
  mutate(predicted_grp = (n_a + n_b)/sum(m_total, nm_total))


data_for_priors %>%
  filter(CategoryType == "Value" | CategoryType == "Artifact" | CategoryType == "Natural") %>% 
  mutate(log_odds = qlogis(predicted_grp)) %>% 
  mutate(upper = log_odds + .5) %>% 
  mutate(lower = log_odds - .5) %>% 
  mutate(upper_p = plogis(upper)) %>% 
  mutate(lower_p = plogis(lower)) %>% 
  mutate(predicted_grp_prob = round(predicted_grp, digits = 2)) %>% 
  mutate(predicted_upper_p = round(upper_p, digits = 2)) %>% 
  mutate(predicted_lower_p = round(lower_p, digits = 2)) %>% 
  mutate(Prior_range = paste0(predicted_grp_prob, " [", predicted_lower_p, "-", predicted_upper_p, "]" )) %>% 
  select(Prior_range, CategoryType, predicted_answer) %>% 
  pivot_wider(names_from = predicted_answer, values_from = Prior_range)

  

data_for_priors %>% 
  filter(CategoryType == "Value" | CategoryType == "Artifact" | CategoryType == "Natural") %>% 
  select(CategoryType, predicted_answer, predicted_grp) %>% 
  pivot_wider(names_from = predicted_answer, values_from = predicted_grp)

data_for_priors %>% 
  write.csv(here("data", "tidy", "prior_data.csv"))
            ### save it and load it in the exp 2 rmd 

sd = .5
is_int = paste0("normal(", qlogis(.29), ", ",sd,")")
intnot_int = paste0("normal(", qlogis(.36), ", ",sd,")")
is_art = paste0("normal(", 1.3, ", ",sd,")")
is_nat = paste0("normal(", 1.7, ", ",sd,")")
is_not_art = paste0("normal(", .81, ", ",sd,")")
is_not_nat = paste0("normal(", 1.38, ", ",sd,")")

prior_check = c(set_prior(is_int, class = "Intercept", dpar = "muIs"),
                set_prior(intnot_int, class = "Intercept", dpar = "muIsnot"),
                set_prior(is_art, class = "b", dpar = "muIs", coef = "CategoryTypeArtifact"),
                set_prior(is_nat, class = "b", dpar = "muIs", coef = "CategoryTypeNatural"),
                set_prior(is_not_art, class = "b", dpar = "muIsnot", coef = "CategoryTypeArtifact"),
                set_prior(is_not_nat, class = "b", dpar = "muIsnot", coef = "CategoryTypeNatural"))

## Loading the tidy data and fitting the model  
senses_tidy = read.csv(here("data", "tidy", "senses_tidy.csv")) 

senses_tidy$CategoryType = as.factor(senses_tidy$CategoryType)
senses_tidy$CategoryType = relevel(senses_tidy$CategoryType, ref = "Value")

b2 <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | Item), 
          prior = prior_check,
          data=senses_tidy,
          family="categorical",
          file = here("data", "models", "exp2_model.rds"))

b2_s <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | Item), 
          data= senses_tidy,
          family="categorical",
          file = here("data", "models", "exp2_model_s.rds"))


