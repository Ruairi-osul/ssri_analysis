# SR drug effects

library(tidyverse)
library(scales)

data_dir <- file.path("data")
fig_dir <- file.path("figs")


# plot setup
colors <- c(
  `Slow Regular`="#332288",
  `Fast Firing`="#117733",
  `Slow Irregular`="#CC6677",
  `Non Responder`="#7E8283",
  `Stimulated`="#F6931D",
  `Inhibited`="#31A6D9"
)


theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=18),
                  axis.title.y = element_text(colour = "black", size=18),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5),
                  axis.text.y = element_text(color="black", size=15),
                  axis.text.x = element_text(color="black", size=15, angle = 0, hjust=0.9),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=15)))

# load datasets
df <- read_csv(file.path("data", "cit_effects.csv")) %>%
  filter(cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster, 
                          labels=c("Fast Firing", "Slow Irregular", "Slow Regular")))

dfz <- read_csv(file.path("data", "cit_timeseries.csv")) %>%
  filter(cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster, 
                          labels=c("Fast Firing", "Slow Irregular", "Slow Regular")),
         time = bin/60
         ) %>%
  filter(time <= 20)

df_way <- read_csv(file.path("data", "way_effects.csv")) %>%
  filter(cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster, 
                          labels=c("Fast Firing", "Slow Irregular", "Slow Regular")))

# time series plot


dfz %>%
  filter(cluster=="Slow Regular") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.27) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram=colors[["Slow Regular"]])) +
  scale_fill_manual(values=c(Saline="#7E8283",Citalopram=colors[["Slow Regular"]]), guide="none") +
  coord_cartesian(ylim=c(-15,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-15,  -10, -5, 0)) +
  labs(x="Time [min]", y="Z", color="Slow Regular", fill="") +
  ggsave(file.path(fig_dir, "DrugEffects_LineSR.svg"))



####

colors <- c(
  `Citalopram`="#332288",
  `Saline`="#7E8283"
)

df %>%
  filter(cluster=="Slow Regular",
         response_dm %in% c("Inhibited"),
  ) %>%
  select(neuron_id, treatment, cluster, response_dm, diff_of_means_pct, diff_of_means) %>%
  rename(
    `% Pre-Drug\nSpike Rate`=diff_of_means_pct,
    `Spike Rate Change\n[Hz]`=diff_of_means
  ) %>%
  gather("metric", "value", -cluster, -neuron_id, -treatment, -response_dm) %>%
  ggplot(aes(y=value, x=treatment, color=treatment)) +
  geom_boxplot(lwd=1, width=0.21) +
  scale_color_manual(values=colors) +
  scale_y_continuous(breaks=scales::pretty_breaks(3), limits=c(NA, 0)) +
  coord_flip() +
  facet_grid(cols=vars(metric), scales="free") +
  labs(x="", y="") +
  theme(strip.text.x = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.spacing=unit(2, "lines")
  ) +
  ggsave(file.path(fig_dir, "SR_cit_box.svg"))

#


df %>%
  filter(cluster=="Slow Regular",
         response_dm %in% c("Inhibited"),
  ) %>%
  select(neuron_id, treatment, cluster, response_dm, diff_of_means_pct, diff_of_means) %>%
  rename(
    `% Pre-Drug\nSpike Rate`=diff_of_means_pct,
    `Spike Rate Change\n[Hz]`=diff_of_means
  ) %>%
  gather("metric", "value", -cluster, -neuron_id, -treatment, -response_dm) %>%
  ggplot(aes(y=value, x=treatment, color=treatment)) +
  geom_boxplot(lwd=1, width=0.21) +
  scale_color_manual(values=colors) +
  scale_y_continuous(breaks=scales::pretty_breaks(3), limits=c(NA, 0)) +
  # coord_flip() +
  facet_grid(rows=vars(metric), scales="free") +
  labs(x="", y="") +
  theme(strip.text.y = element_text(size=14),
        legend.position = "none",
        axis.text.x = element_text(size=14, angle=45),
        axis.text.y = element_text(size=14),
        panel.spacing=unit(2.1, "lines")
  ) +
  ggsave(file.path(fig_dir, "SR_cit_box.svg"))





#####

colors <- c(
  `Citalopram`="#332288",
  `Saline`="#7E8283"
)

df %>%
  group_by(cluster, treatment) %>%
  summarise(was_inhibited=mean(response_dm=="Inhibited") * 100) %>%
  ungroup() %>%
  filter(cluster=="Slow Regular") %>%
  ggplot(aes(x=treatment, y=was_inhibited, fill=treatment)) +
  geom_col(width=0.6) +
  labs(x="", y="% Units Inhibited") +
  scale_fill_manual(values=colors) +
  theme(axis.text.x = element_text(color="black", size=14, hjust=0.5, angle=0),
        legend.position = "NA") +
  ggsave(file.path(fig_dir, "SR_prop_inhibited.svg"))


df_way %>%
  left_join(df)%>%
  drop_na() %>%
  filter(
    cluster == "Slow Regular",
    citalopram_response == "Inhibited",
    treatment == "Citalopram"
  ) %>%
  summarise(way_stimulated = mean(way_effect== "Stimulated") * 100) %>%
  ggplot(aes(y=way_stimulated, x=factor(1)) ) +
  geom_col(width=0.4, fill="#332288") +
  lims(y=c(0, 80)) +
  labs(x="Citalopram-Inhibited\nUnits", y="% WAY100635-\nActiviated") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(color="black", size=14, hjust=0.5, angle=0)) +
  ggsave(file.path(fig_dir, "SR_WAY.svg"))
