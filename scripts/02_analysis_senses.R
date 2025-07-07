source(here::here("scripts", "00_libs.R"))


senses_tidy %>% 
  group_by(CategoryType, Selection) %>% 
  summarise(n = n())




prior_check = c(set_prior("normal(-.89, 0.5)", class = "Intercept", dpar = "muIs"),
                set_prior("normal(-.5, 0.5)", class = "Intercept", dpar = "muIsnot"),
                set_prior("normal(-.75, 0.5)", class = "b", dpar = "muIs", coef = "CategoryTypeArtifact"), # increase in log odds for answering is for artifacts relative to the is in percent for value  
                set_prior("normal(-.5, 0.5)", class = "b", dpar = "muIs", coef = "CategoryTypeNatural"),
                set_prior("normal(-.5, 0.5)", class = "b", dpar = "muIsnot", coef = "CategoryTypeArtifact"),
                set_prior("normal(-1.5, 0.5)", class = "b", dpar = "muIsnot", coef = "CategoryTypeNatural"))


qlogis(.29) - qlogis(.35)


plogis(-1.1)
# prior no 3 - goes from .33 to around .1 - check the math - (effect should be .23 prob to log odds)
# prior no 4- foes from .33 to .05 or so, effect should be .28
# prior no 5 - .2
# prior no 6 - .4 


senses_tidy = read.csv(here("data", "tidy", "senses_tidy.csv")) 

senses_tidy$CategoryType = as.factor(senses_tidy$CategoryType)
senses_tidy$CategoryType = relevel(senses_tidy$CategoryType, ref = "Value")




b2 <- brm(Selection ~ CategoryType + (CategoryType | Participant), 
          prior = prior_check,
          data=senses_tidy,
          family="categorical")

summary(b2)

conditional_effects(b2, categorical = TRUE)

plogis(-2)