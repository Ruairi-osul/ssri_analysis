library(tidyverse)
library(lsmeans)
source(file.path("rscripts", "theme_pub.r"))
source(file.path("rscripts", "diff_mean_plot.r"))


data_dir <- file.path("data")
fig_dir <- file.path("figs")

df <- read_csv(file.path(data_dir, "cross_corr_gw.csv"))
df_groups <- read_csv((file.path(data_dir, "chronic_baseline.csv")))

axes_labels <- c("SAL", "CIT", "WITH")

# merge datasets, create pair and bin ids
dfa <- df_groups %>% 
  select(neuron_id, group_name, type) %>%
  rename(neuron_1=neuron_id) %>%
  right_join(df, by="neuron_1") %>%
  mutate(significantly_correlated = p<0.05,
         bin_id=rownames(.),
         pair_id = group_indices(., neuron_1, neuron_2))

### create grouped dataset
df_grouped <- dfa %>% 
  group_by(pair_id) %>%
  summarise(pair_correlated = sum(p<0.05) > 1) %>%
  left_join(select(dfa, session_name, group_name, group_1, group_2, pair_id))

### calculate latencies
df_latency <- dfa %>%
  group_by(pair_id) %>%
  summarise(mean_cc = mean(crosscorrelation))%>%
  left_join(dfa) %>%
  mutate(dist_from_mean = abs(crosscorrelation - mean_cc)) %>%
  group_by(pair_id) %>%
  filter(p < 0.05) %>%
  summarise(max_dist=max(dist_from_mean), latency=time_sec[which.max(dist_from_mean)]) %>%
  left_join(df_grouped) %>% 
  distinct()

###############################

df_ff_sr_grouped <-  df_grouped %>%
                        filter(group_1 == "fast_firing",
                               group_2 == "slow_regular")
df_ff_sr_grouped %>%
  group_by(group_name) %>%
  summarise(proportion=mean(pair_correlated)) %>%
  ggplot(aes(x=group_name, y=proportion)) +
  geom_bar(stat="identity", width=0.7, fill="black") +
  theme_Publication() +
  labs(title="Fast Firing - p5HT",
       y="Proprtion\nSignificantly Crosscorrelated",
       x="") +
  theme(text=element_text(size=18)) +
  scale_x_discrete(labels=axes_labels) +
  theme(text=element_text(size=20))

df_ff_sr_latency <- df_latency %>% filter(group_1=="fast_firing",
                                           group_2 == "slow_regular")

diff_mean_plot(df_ff_sr_latency, 
               x="group_name", y="latency")

dfa %>%
  filter(pair_id==89
         ) %>%
  ggplot(aes(x=time_sec, y=crosscorrelation)) +
  geom_line()
