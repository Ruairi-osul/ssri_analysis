# 03: Different Neuron Types
#
# Purpose:
#   Describe properties of the three clusters identified in the dataset
# 
# Requires:
#   ../data/baseline.csv
#     created by: notebooks/'01: Estimation of Neurotransmitter Expression.ipynb'
#
# Produces:
#   scatter plot of spiking characteristics used for clustering
#   boxplots of other sp
#   proportion plot of the relative frequencies of the different neuron types

library(tidyverse)
library(ggthemes)

colors <- c(
  `Slow Regular`="#332288",
  `Fast Firing`="#117733",
  `Slow Irregular`="#CC6677",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=10),
                  axis.title.y = element_text(colour = "black", size=10),
                  axis.text = element_text(color="black", size=10),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=10)))
data_dir <- file.path("data")
fig_dir <- file.path("figs")


df <- read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(group_name %in% c("acute_citalopram", 
                          "acute_saline", 
                          "shock", 
                          "sham", 
                          "acute_cit", 
                          "acute_sal")) %>%
  mutate(cluster = factor(cluster, 
                          labels=c("Fast Firing","no_baseline", "Slow Irregular", "Slow Regular")))

# Scatter plot

df %>%
  filter(cluster != "no_baseline") %>%
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point(alpha=0.4, size=2) +
  scale_color_manual(values=colors, 
                     labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) +
  labs(y="Mean Spike Rate [Hz]", x="Spike Regularity [CV(ISI)]") +
  theme(
    legend.position = c(0.7, 0.8),
    legend.background= element_rect(color="NA", fill="NA"),
      ) +
  ggsave(file.path(fig_dir, "neuronTypeScatter.svg"))

# Bar Prop Neuron Types

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
  ) +
  ggsave(file.path(fig_dir, "FreqNeuronTypes.svg"))

# boxplots 

df_melt <- df %>%
  filter(cluster != "no_baseline") %>%
  select(neuron_id, mean_firing_rate, cv2_isi, waveform_width, peak_asymmetry, cluster, channel) %>%
  rename("Mean Spike Rate\n[Hz]"=mean_firing_rate,
         "Spike Regularity\n[CV(ISI)]"=cv2_isi,
         "Channel Number"=channel,
         "Waveform Asymmetry\n[AU]"= peak_asymmetry,
         "Waveform Width\n[ms]"= waveform_width) %>%
  gather(key="metric", value="value", -neuron_id, -cluster)

df_melt %>%
  ggplot(aes(x=cluster, y=value, color=cluster)) +
  geom_boxplot(width=0.3, lwd=1) +
  scale_y_continuous(limits = c(0,NA), breaks=scales::pretty_breaks(3)) +
  scale_color_manual(values=colors) +
  labs(x="", y="") +
  coord_flip() +
  facet_grid(cols=vars(metric), scales="free") +
  theme(legend.position = "None") +
  ggsave(file.path(fig_dir, "NeuronTypeBox.svg"))

# No Baseline Plot

df %>%
  mutate(has_baseline = factor(cluster != "no_baseline", 
  labels=c("no_baseline", "baseline"))) %>%
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

