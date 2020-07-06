library(tidyverse)
library(ggthemes)
library(scales)

colors <- c(
  "Slow Regular"="#FF0100",
  "Fast Firing"="#00B34E",
  "Slow Irregular"="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_gdocs() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5),
                  axis.text = element_text(color="black", size=13),
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
  mutate(cluster = factor(cluster, labels=c("Fast Firing", "Slow Irregular", "Slow Regular")),
         waveform_width=waveform_width / 30)

df_melt <- df %>%
  select(neuron_id, mean_firing_rate, cv2_isi, waveform_width, peak_asymmetry, cluster, channel) %>%
  rename("Mean Firing Rate\n[Hz]"=mean_firing_rate,
         "Spike Regularity\n[CV(ISI)]"=cv2_isi,
         "Channel Number"=channel,
         "Waveform Asymmetry\n[AU]"= peak_asymmetry,
         "Waveform Width\n[ms]"= waveform_width) %>%
  gather(key="metric", value="value", -neuron_id, -cluster)

df_melt %>%
  ggplot(aes(x=cluster, y=value, color=cluster)) +
  geom_boxplot(width=0.4) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  scale_color_manual(values=colors) +
  labs(x="", y="") +
  coord_flip() +
  facet_grid(cols=vars(metric), scales="free") +
  theme(legend.position = "None")


