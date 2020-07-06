library(tidyverse)


colors <- c(
  "FALSE"="#7E8283",
  "TRUE"="#F6931D",
  "ESHOCK"="#FF0100",
  "HAMILTON"="#00B34E",
  "CITWAY"="#806A9D"
)


df <- read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(group_name %in% c("acute_citalopram", 
                           "acute_saline", 
                           "shock", 
                           "sham", 
                           "acute_cit", 
                           "acute_sal"),
         cluster != "no_baseline")


df %>% 
  mutate(has_one_hour = experiment_name != "HAMILTON") %>%
  group_by(has_one_hour) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=has_one_hour)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=paste0(round(pct, 1), "%")), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=colors,
                    labels=c("20 Minutes", "60 Minutes"),
                    name="Baseline Duration") +  
  ggtitle("Proportion of Neurons") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )

df %>%
  select(session_name, experiment_name) %>%
  distinct() %>%
  mutate(has_one_hour = experiment_name != "HAMILTON") %>%
  group_by(has_one_hour) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=has_one_hour)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=count), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=colors,
                    labels=c("20 Minutes", "60 Minutes"),
                    name="Baseline Duration") +  
  ggtitle("Number of Recordings") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )

## Proportion of neurons and recordings receiving citalopram or saline versus nothing
df %>%
  select(session_name, group_name, experiment_name) %>%
  distinct() %>%
  mutate(treatment = if_else(group_name %in% c("shock","sham"),
                                "None",
                                if_else(group_name %in% c("acute_citalopram", 
                                                          "acute_cit"),
                                        "Citalopram", "Saline")
                                ),
         treatment = factor(treatment, levels=c("None", "Saline", "Citalopram"))) %>%
  group_by(treatment) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=treatment)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=count), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=c("Citalopram"="#FF0100", "Saline"="#00B34E", "None"="#7E8283"),
                    name="Drug Challenge") +  
  ggtitle("Number of Recordings") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )

df %>%
  select(session_name, experiment_name) %>%
  distinct() %>%
  mutate(has_one_hour = experiment_name != "HAMILTON") %>%
  group_by(has_one_hour) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=has_one_hour)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=count), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=colors,
                    labels=c("No Footshock", "Footshock"),
                    name="") +  
  ggtitle("Number of Recordings") +
  theme_void() +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
  )
