library(here)
library(tidyverse)
library(brms)
library(bayestestR)

# Load helper functions (including pairwise comp fuction)
source(here::here("report", "exp2", "00_pairwise_comp_function.R"))

# Tidy the data 
exp3_tidy = read.csv(here("data", "tidy", "four_choice_data.csv")) %>% 
  rename("CategoryType" = "kind") %>% 
  rename("Selection" = "response") %>% 
  rename("Participant" = "prolific_id") %>% 
  filter(CategoryType == "natural kind" | 
           CategoryType == "abstract concept" | 
           CategoryType == "artifact") %>% 
  mutate(CategoryType = case_when(
    CategoryType == "natural kind" ~ "natural_kind",
    CategoryType == "abstract concept" ~ "value",
    CategoryType == "artifact" ~ "artifact"
  ))

#### Generate priors from desc data from exp 2 
senses_tidy = read.csv(here("data", "tidy", "senses_tidy.csv")) # Load exp 2 data 

# Check the total data per category to calculate a percentage 
senses_tidy %>% 
  group_by(CategoryType) %>% 
  summarise(n = n())

### Prior chart to report prior to converting to model terms 
senses_tidy %>% 
  group_by(CategoryType, Selection) %>% 
  summarise(n = n()/288)

#Is=Akira_is_right,
#Is not=Elaine_is_right,
#Both=Both_are_right.

#### Artifact ####
# Both are right - .587
# Akira is right - .0625
# Elaine is right - .351

#### Natural ####
# Both are right - .549
both_nat = qlogis(.549)
# Akira is right - .0833
akira_nat = qlogis(.0833)
# Elaine is right - .368
elaine_nat = qlogis(.368)

#### Value ####
# Both are right - .562
both_val = qlogis(.562)
# Akira is right - .212
akira_val = qlogis(.212)
# Elaine is right - .226
elaine_val = qlogis(.226)

# set sd and intercepts 
sd = .5
both_int = paste0("normal(", qlogis(.587), ", ",sd,")")
akira_int = paste0("normal(", qlogis(.0625), ", ",sd,")")
elaine_int = paste0("normal(", qlogis(.351), ", ",sd,")")
neither_int = paste0("normal(", qlogis(.001), ", ",sd,")")

mod4 = qlogis(.549) - qlogis(.587) # both artifact - natural kind effect (model term 4)
mod5 = both_val - qlogis(.587) # both artifact - value effect (model term 5)
mod6 = elaine_nat - qlogis(.351)
mod7 = elaine_val - qlogis(.351)
mod8 = qlogis(.002) - qlogis(.001)
mod9 = qlogis(.002) - qlogis(.001)

# set priors
prior_check = c(set_prior(both_int, class = "Intercept", dpar = "muBoth"),
                set_prior(elaine_int, class = "Intercept", dpar = "muElaineright"),
                set_prior(neither_int, class = "Intercept", dpar = "muNeither"),
                set_prior(paste0("normal(", mod4, ", ",sd,")"), class = "b", dpar = "muBoth", coef = "CategoryTypenatural_kind"),
                set_prior(paste0("normal(", mod5, ", ",sd,")"), class = "b", dpar = "muBoth", coef = "CategoryTypevalue"),
                set_prior(paste0("normal(", mod6, ", ",sd,")"), class = "b", dpar = "muElaineright", coef = "CategoryTypenatural_kind"),
                set_prior(paste0("normal(", mod7, ", ",sd,")"), class = "b", dpar = "muElaineright", coef = "CategoryTypevalue"),
                set_prior(paste0("normal(", mod8, ", ",sd,")"), class = "b", dpar = "muNeither", coef = "CategoryTypenatural_kind"),
                set_prior(paste0("normal(", mod9, ", ",sd,")"), class = "b", dpar = "muNeither", coef = "CategoryTypevalue"))
                
         
# run model with priors 
b3 <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | spec), 
            data=exp3_tidy,
            prior = prior_check,
            family="categorical",
            file = here("data", "models", "exp3_model.rds"))

# run model with default priors
b3_s <- brm(Selection ~ CategoryType + (CategoryType | Participant) + (1 | spec), 
          data=exp3_tidy,
          family="categorical",
          file = here("data", "models", "exp3_model_s.rds"))


### Pairwise comparisons 
answer1 = c("Akira right", "Both", "Elaine right", "Neither")
category1 = c("value", "artifact", "natural_kind")

# make a dataframe crossing all possible combos to loop through
df_source = crossing(answer1,category1) %>% 
  mutate(cat_c = paste0(answer1,"_",category1))

# run the loop
## list for storage
comp_list = list()  

for (i in 1:12) {
  this_df = df_source %>% 
    mutate(answer2 = df_source$answer1[i]) %>% 
    mutate(category2 = df_source$category1[i])
  comp_list[[i]] = this_df
}

# combine the list into a df
combos = do.call(rbind, comp_list) 

rope_for_data = .2

# use combos dataframe to generate bigger post-hoc comparisons dataframe 
list_pdf = list()
for (it in 1:nrow(combos)) {
  pdf = create_pairwise_df_exp3(combos$answer1[it], combos$category1[it],
                           combos$answer2[it], combos$category2[it], rope = rope_for_data) %>% 
    mutate(combo1 = paste0(combos$answer1[it], "_", combos$category1[it])) %>% 
    mutate(combo2 = paste0(combos$answer2[it], "_", combos$category2[it]))
  list_pdf[[it]] = pdf
}

# combine 
all_data = do.call(rbind,list_pdf)

# save output
all_data %>% 
  write.csv(here("data", "tidy", "exp3_pairwise_data.csv"))





