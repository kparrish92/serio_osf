
exp3_pw = read.csv(here("data", "tidy", "exp3_pairwise_data.csv"))
source(here::here("report", "exp2", "00_pairwise_comp_function.R"))
rope = .2

## Elaine right (w/o neither)

exp3_pw %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Elaine right_artifact_Elaine right_natural_kind" | 
           comps == "Elaine right_artifact_Elaine right_value" |
           comps == "Elaine right_natural_kind_Elaine right_value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "seagreen", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Elaine right_artifact_Elaine right_natural_kind"="Artifact - Natural Kind",
                            "Elaine right_artifact_Elaine right_value"="Artifact - Value", 
                            "Elaine right_natural_kind_Elaine right_value"="Natural Kind - Value"))


create_pairwise_table("Elaine right_artifact_Elaine right_natural_kind", 
                      "Elaine right_artifact_Elaine right_value", 
                      "Elaine right_natural_kind_Elaine right_value") %>% 
  write.csv(here("data", "tidy", "exp3_posthoc_table_elaine.csv"))

ggsave("elaine_pairw.png", path = here("report", "exp3", "figs"))
## Akira right (w/o neither )

exp3_pw %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Akira right_artifact_Akira right_natural_kind" | 
           comps == "Akira right_artifact_Akira right_value" |
           comps == "Akira right_natural_kind_Akira right_value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "grey", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Akira right_artifact_Akira right_natural_kind"="Artifact - Natural Kind",
                            "Akira right_artifact_Akira right_value"="Artifact - Value", 
                            "Akira right_natural_kind_Akira right_value"="Natural Kind - Value"))


create_pairwise_table("Akira right_artifact_Akira right_natural_kind", 
                      "Akira right_artifact_Akira right_value", 
                      "Akira right_natural_kind_Akira right_value") %>% 
  write.csv(here("data", "tidy", "exp3_posthoc_table_akira.csv"))

ggsave("akira_pairw.png", path = here("report", "exp3", "figs"))

## Both right (w/o neither )

exp3_pw %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Both_artifact_Both_natural_kind" | 
           comps == "Both_artifact_Both_value" |
           comps == "Both_natural_kind_Both_value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "pink", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Both_artifact_Both_natural_kind"="Artifact - Natural Kind",
                            "Both_artifact_Both_value"="Artifact - Value", 
                            "Both_natural_kind_Both_value"="Natural Kind - Value"))


create_pairwise_table("Both_artifact_Both_natural_kind", 
                      "Both_artifact_Both_value", 
                      "Both_natural_kind_Both_value") %>% 
  write.csv(here("data", "tidy", "exp3_posthoc_table_both.csv"))

ggsave("both_pairw.png", path = here("report", "exp3", "figs"))



## Within Value

exp3_pw %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Both_value_Elaine right_value" |
           comps == "Both_value_Akira right_value" |
           comps == "Elaine right_value_Akira right_value") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "bisque3", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Both_value_Elaine right_value"="Both right - Elaine right", 
                            "Both_value_Akira right_value"="Both right - Akira right",
                            "Elaine right_value_Akira right_value"="Elaine right - Akira right"))


create_pairwise_table("Both_value_Elaine right_value", 
                      "Both_value_Akira right_value", 
                      "Elaine right_value_Akira right_value") %>% 
  write.csv(here("data", "tidy", "exp3_posthoc_table_value.csv"))

ggsave("value_pairw.png", path = here("report", "exp3", "figs"))



## Within natural kinds 

exp3_pw %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Both_natural_kind_Elaine right_natural_kind" |
           comps == "Both_natural_kind_Akira right_natural_kind" |
           comps == "Elaine right_natural_kind_Akira right_natural_kind") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "deepskyblue3", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Both_natural_kind_Elaine right_natural_kind"="Both right - Elaine right", 
                            "Both_natural_kind_Akira right_natural_kind"="Both right - Akira right",
                            "Elaine right_natural_kind_Akira right_natural_kind"="Elaine right - Akira right"))

create_pairwise_table("Both_natural_kind_Elaine right_natural_kind", 
                      "Both_natural_kind_Akira right_natural_kind", 
                      "Elaine right_natural_kind_Akira right_natural_kind") %>% 
  write.csv(here("data", "tidy", "exp3_posthoc_table_nk.csv"))

ggsave("nk_pairw.png", path = here("report", "exp3", "figs"))

## Within artifact 

exp3_pw %>% 
  mutate(comps = paste0(combo1,"_", combo2)) %>% 
  filter(comps == "Both_artifact_Elaine right_artifact" |
           comps == "Both_artifact_Akira right_artifact" |
           comps == "Elaine right_artifact_Akira right_artifact") %>% 
  ggplot(aes(y = comps, x = effect)) +
  stat_halfeye(fill = "coral", alpha = .5) + geom_vline(xintercept = rope*-1, linetype = "dashed") +
  geom_vline(xintercept = rope, linetype = "dashed") + theme_minimal() + 
  ylab("") + theme(axis.ticks=element_blank(),
                   axis.title.y=element_blank(), legend.position = "none") +
  xlab("Difference in Log-odds") +
  xlim(-6,6) +
  scale_y_discrete(labels=c("Both_artifact_Elaine right_artifact"="Both right - Elaine right", 
                            "Both_artifact_Akira right_artifact"="Both right - Akira right",
                            "Elaine right_artifact_Akira right_artifact"="Elaine right - Akira right"))

create_pairwise_table("Both_artifact_Elaine right_artifact", 
                      "Both_artifact_Akira right_artifact", 
                      "Elaine right_artifact_Akira right_artifact") %>% 
  write.csv(here("data", "tidy", "exp3_posthoc_table_art.csv"))

ggsave("art_pairw.png", path = here("report", "exp3", "figs"))

