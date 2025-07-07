source(here::here("scripts", "00_libs.R"))


senses = read.csv(here("data", "senses.csv")) 

PivotData <- pivot_longer(senses, Water:Shovel, names_to="Item", values_to="Selection")
Data <- PivotData %>% mutate(CategoryType = case_when(
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
  Item == "Gallon" ~ "Definite"))

Data <- Data %>% filter(!CategoryType %in% c("Vague", "Definite"))

Data$Item <- as.factor(Data$Item)
Data$Selection <- factor(Data$Selection, levels = c("Both", "Is not", "Is"))
Data$CategoryType <- factor(Data$CategoryType, levels =c("Natural", "Artifact", "Value"))

Data %>% 
  write.csv(here("data", "tidy", "senses_tidy.csv"))