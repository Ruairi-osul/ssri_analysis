library(tidyverse)
library(ggthemes)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_gdocs())
data_dir <- file.path("data")


df = read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(group_name %in% c("acute_citalopram", 
                          "acute_saline", 
                          "shock", 
                          "sham", 
                          "acute_cit", 
                          "acute_sal"))

df <- df %>%
  mutate(has_baseline = factor(cluster != "no_baseline", 
  labels=c("no_baseline", "baseline")))


df %>%
  group_by(has_baseline) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=has_baseline)) +
    geom_bar(position=position_stack(), stat="identity", width=2) + 
    geom_text(aes(label=paste0(round(pct, 1), "%")), 
              position=position_stack(vjust=0.5), 
              size=6, 
              color="white") +
    scale_fill_manual(values=colors, labels=c("Not Baseline-Active", "Baseline-Active")) +  
    theme_void() +
    theme(legend.title=element_blank(), 
          legend.text=element_text(size=15),
          )


df %>%
  filter(cluster != "no_baseline") %>%
  group_by(cluster) %>%
  summarise(count=n()) %>%
  mutate(pct=count/sum(count) * 100) %>%
  ggplot(aes(x=1, y=pct, fill=cluster)) +
  geom_bar(position=position_stack(), stat="identity", width=2) + 
  geom_text(aes(label=paste0(round(pct, 1), "%")), 
            position=position_stack(vjust=0.5), 
            size=6, 
            color="white") +
  scale_fill_manual(values=colors, labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) +  
  theme_void() +
  theme(legend.title=element_blank(), 
        legend.text=element_text(size=15),
  )

