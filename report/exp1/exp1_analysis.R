# Load libraries
library(here)
library(tidyverse)
library(brms)
library(bayestestR)

# load tidy data 
nonmember_tidy = read.csv(here("data", "tidy", "nonmember_tidy.csv")) %>% 
  mutate(membership = "nonmember")
member_tidy = read.csv(here("data", "tidy", "member_tidy.csv")) %>% 
  mutate(membership = "member")


## Set priors 

#### member model 
sd = .5
prior_artifact_m = qlogis(.25) # the value in qlogis is the expected probabiliy of 1
prior_def_m = prior_artifact_m - qlogis(.95) 
prior_nk_m = prior_artifact_m - qlogis(.4)
prior_vague_m = prior_artifact_m - qlogis(.05)
prior_value_m = prior_artifact_m - qlogis(.15)

member_priors = c(set_prior(paste0("normal(", prior_def_m, ", ",sd,")"), coef = "CategoryTypeDefinite"),
                  set_prior(paste0("normal(", prior_nk_m, ", ",sd,")"), coef = "CategoryTypeNatural"),
                  set_prior(paste0("normal(", prior_vague_m, ", ",sd,")"), coef = "CategoryTypeVague"),
                  set_prior(paste0("normal(", prior_value_m, ", ",sd,")"), coef = "CategoryTypeValue"))


#### non-member model 
prior_artifact_nm = qlogis(.06) # the value in qlogis is the expected prob of 1
prior_def_nm = prior_artifact_nm - qlogis(.01) 
prior_nk_nm = prior_artifact_nm - qlogis(.01)
prior_vague_nm = prior_artifact_nm - qlogis(.05)
prior_value_nm = prior_artifact_nm - qlogis(.15)


nonmember_priors = c(set_prior(paste0("normal(", prior_def_nm, ", ",sd,")"), coef = "CategoryTypeDefinite"),
                     set_prior(paste0("normal(", prior_nk_nm, ", ",sd,")"), coef = "CategoryTypeNatural"),
                     set_prior(paste0("normal(", prior_vague_nm, ", ",sd,")"), coef = "CategoryTypeVague"),
                     set_prior(paste0("normal(", prior_value_nm, ", ",sd,")"), coef = "CategoryTypeValue"))


## Combine the two dataframes 
ord_data = rbind(member_tidy, nonmember_tidy) 
## Run member model
ord_mod_member_priors <- brm(as.integer(Rating) ~ CategoryType + 
                 (CategoryType | Participant) + (1 | Item),
               data = ord_data %>% filter(membership == "member"),
               family = cumulative(),
               prior = member_priors,
               cores = 4,
               file = here("data", "models","ord_member.rds"))

ord_mod_member <- brm(as.integer(Rating) ~ CategoryType + 
                 (CategoryType | Participant) + (1 | Item),
               data = ord_data %>% filter(membership == "member"),
               family = cumulative(),
               cores = 4,
               file = here("data", "models","ord_member_s.rds"))


## Run nonmember model
ord_mod_nonmember_priors <- brm(as.integer(Rating) ~ CategoryType + 
                        (CategoryType | Participant) + (1 | Item),
                      data = ord_data %>% filter(membership == "nonmember"),
                      family = cumulative(),
                      prior = nonmember_priors,
                      cores = 4,
                      file = here("data", "models","ord_nonmember.rds"))

ord_mod_nonmember <- brm(as.integer(Rating) ~ CategoryType + 
                                  (CategoryType | Participant) + (1 | Item),
                                data = ord_data %>% filter(membership == "nonmember"),
                                family = cumulative(),
                                cores = 4,
                                file = here("data", "models","ord_nonmember_s.rds"))
