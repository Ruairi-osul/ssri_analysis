library(tidyverse)
library(ggthemes)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_gdocs() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  axis.text = element_text(color="black", size=14),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=18)))
data_dir <- file.path("data")


df <- read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(group_name %in% c("acute_citalopram", 
                          "acute_saline", 
                          "shock", 
                          "sham", 
                          "acute_cit", 
                          "acute_sal"),
         cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster))

df %>%
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point(alpha=0.4) +
  scale_color_manual(values=colors, 
                     labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) +
  labs(y="Mean Firing  [Hz]", x="Spike Regularity [CV(ISI)]") +
  theme(legend.position = c(0.7, 0.8), legend.background= element_rect(fill="NA"))

