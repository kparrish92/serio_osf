library(here)
library(tidyverse)

member = read.csv(here("data", "baseline_member_r.csv"))
nonmember = read.csv(here("data", "baseline_nonmember.csv"))

PivotData <- pivot_longer(nonmember, Water:Shovel, names_to="Item", values_to="Rating")
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
  Item == "Gallon" ~ "Definite"))


FinalData$Item <- as.factor(FinalData$Item)
FinalData$CategoryType <- as.factor(FinalData$CategoryType)

FinalData %>% 
  write.csv(here("data", "tidy", "nonmember_tidy.csv"))

Data <- pivot_longer(member, Water:Shovel, names_to="Item", values_to="Rating")
MutatedData <- Data %>% mutate(CategoryType = case_when(
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

MutatedData$Item <- as.factor(MutatedData$Item)
MutatedData$CategoryType <- as.factor(MutatedData$CategoryType)

MutatedData %>% 
  write.csv(here("data", "tidy", "member_tidy.csv"))
