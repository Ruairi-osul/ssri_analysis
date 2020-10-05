# 03: Compare Probe to Single Unit
#
# Purpose:
#   Compare and contrast the neurons we get in our single unit data set and probe data set
# 
# Requires:
#   ../data/baseline.csv
#     created by: notebooks/'01: Estimation of Neurotransmitter Expression.ipynb'
#   ../data/single_unit_labelled.csv
#      created by:
#
# Produces:
#   plots:
#     scatter plot of single unit spiking characteristics
#     bar plot comparing frequancy of recorded neurons of different types
#   models:
#     test hypothesis of association between recording method and neuron type observed

library(tidyverse)
library(ggthemes)


# plotting setup
colors <- c(
  `Slow Regular`="#332288",
  `Fast Firing`="#117733",
  `Slow Irregular`="#CC6677",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  axis.text = element_text(color="black", size=14),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=15)))

# dir setup
data_dir <- file.path("data")
fig_dir <- file.path("figs")

# load datasets
dfb <- read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(group_name %in% c("acute_citalopram", 
                          "acute_saline", 
                          "shock", 
                          "sham", 
                          "acute_cit", 
                          "acute_sal"),
         cluster != "no_baseline") %>%
  mutate(recording_method="Silicon\nProbe",
         waveform_width=waveform_width / 30) %>%
  select(neuron_id, recording_method, cluster, mean_firing_rate, cv2_isi, waveform_width)

dfs <- read_csv(file.path(data_dir, "single_unit_labelled.csv")) %>%
  mutate(id=as.numeric(rownames(.)),
         recording_method="Glass\nPipette") %>%
  left_join(read_csv(file.path(data_dir, "single_unit_dataset_tidied.csv"))) %>%
  select(id, recording_method, cluster, "Mean Firing Rate", "CV(ISI)", "to_baseline") %>%
  rename(neuron_id="id", mean_firing_rate="Mean Firing Rate", cv2_isi="CV(ISI)", waveform_width="to_baseline")
  
dfs_sub <- dfs %>%
  select(neuron_id, recording_method, cluster)

# concatenate datasets

df <- bind_rows(dfb, dfs) %>%
  mutate(
    cluster = factor(
      cluster, 
      labels=c("Fast Firing", "Slow Irregular", "Slow Regular")
      )
  )

# Model

tab <- table(df$cluster, df$recording_method)
mod <- chisq.test(tab)
mod

# Bar plot

df %>% 
  ggplot(aes(x=recording_method, fill=cluster)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=colors) + 
  labs(x="", y="Neuron Count") +
  theme(axis.text.x = element_text(color="black", size=15, angle = 0, hjust=0.5)) +
  ggsave(file.path(fig_dir, "RecordingMethodComparison.svg"))
  

# Scatter

df %>%
  filter(recording_method == "Glass\nPipette") %>%
  rename(mean_firing_rate="Mean Firing Rate", cv2_isi="CV(ISI)") %>%
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point(alpha=0.7, size=2) +
  scale_color_manual(values=colors) +
  xlim(0, 1.5) +
  labs(y="Mean Firing Rate [Hz]", x="Spike Regularity [CV(ISI)]") +
  theme(legend.position = c(0.7, 0.8), 
        legend.background= element_rect(fill="NA", color="NA"),
        axis.title.y=element_text(size=14),
        axis.title.x=element_text(size=14),
        ) +
  ggsave(file.path(fig_dir, "SingleUnitScatter.svg"))

dfs

# Boxplots

df %>%
  mutate(cluster=factor(cluster, levels=c("Fast Firing", "Slow Irregular", "Slow Regular")),
         cluster = fct_rev(cluster)) %>%
  rename(`Mean Firing Rate\n[Hz]`=mean_firing_rate, `Waveform Width\n[ms]`=waveform_width,
         `Spike Regularity\n[CV(ISI)]`= cv2_isi) %>%
  gather("metric", "value", -neuron_id, -recording_method, -cluster) %>%
  ggplot(aes(x=recording_method, y=value, color=cluster)) +
  geom_boxplot(width=0.6, lwd=1) +
  scale_color_manual(values=colors) +
  scale_y_continuous(limits = c(0,NA), breaks=scales::pretty_breaks(3)) +
  coord_flip() +
  facet_grid(cols=vars(metric), scales="free") +
  labs(x="", y="") +
  theme(legend.position = "NA",
        strip.text.x = element_text(size = 13)) +
  ggsave(file.path(fig_dir, "SingleUnitbox.svg"))
