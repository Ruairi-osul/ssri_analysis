library(tidyverse)

data_dir <- file.path("data")

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

##################################

library(ggthemes)

df %>%
  count(session_name) %>%
  summarise(m=mean(n))

df %>%
  count(session_name) %>%
  mutate(id=factor(rownames(.)),
         id=fct_reorder(id, n)) %>%
  ggplot(aes(y=id, x=n)) +
  geom_col(fill="#7E8283", lw=10) +
  geom_vline(xintercept=24.6, linetype="dashed", size=1.3, color="black") +
  theme_clean() + 
  labs(x="Number of Neurons Recorded", y="Individual Recording Sessions") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))
