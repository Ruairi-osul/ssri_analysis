library(tidyverse)
library(scales)

data_dir <- file.path("data")

df <- read_csv(file.path(data_dir, "cit_zscores.csv")) %>%
  filter(cluster != "no_baseline") %>%
  mutate(treatment= factor(treatment),
         time = bin/ 60 )


colors <- c(
  SAL="#7E8283",
  CIT="#31A6D9",
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
)

df 

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=25),
                  axis.title.y = element_text(colour = "black", size=25),
                  strip.text = element_text(color="black", size=20),
                  axis.text.y = element_text(color="black", size=15),
                  axis.text.x = element_text(color="black", size=15),
                  legend.title =element_text(color="black", size=18),
                  legend.text = element_text(color="black", size=15)))


df %>%
  filter(cluster=="fast_firing") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram="#00B34E")) +
  scale_fill_manual(values=c(Saline="#7E8283", Citalopram="#00B34E"), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time", y="Z Score", color="Fast Firing", fill="")

df %>%
  filter(cluster=="slow_regular") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram="#FF0100")) +
  scale_fill_manual(values=c(Saline="#7E8283", Citalopram="#FF0100"), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time", y="Z Score", color="Slow Regular", fill="")

df %>%
  filter(cluster=="slow_irregular") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram="#806A9D")) +
  scale_fill_manual(values=c(Saline="#7E8283", Citalopram="#806A9D"), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time", y="Z Score", color="Slow Irregular", fill="")
