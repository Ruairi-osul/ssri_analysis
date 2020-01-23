library(tidyverse)
library(lsmeans)
source(file.path("rscripts", "theme_pub.r"))
source(file.path("rscripts", "diff_mean_plot.r"))


data_dir <- file.path("data")
fig_dir <- file.path("figs")

df <- read_csv(file.path(data_dir, "cross_corr_sr.csv"))
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

# create grouped dataset by pair
df_grouped <- dfa %>% 
  group_by(pair_id) %>%
  summarise(pair_correlated = sum(p<0.05) > 1) %>%
  left_join(select(dfa, session_name, group_name, pair_id))

# create latency dataset
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

# plot grouped data
df_grouped %>%
  group_by(group_name) %>%
  summarise(proportion=mean(pair_correlated)) %>%
  ggplot(aes(x=group_name, y=proportion)) +
  geom_bar(stat="identity", width=0.7, fill="black") +
  theme_Publication() +
  labs(y="p5HT - p5HT\nProprtion Correlated",
       x="") +
  theme(text=element_text(size=18)) +
  scale_x_discrete(labels=axes_labels) +
  theme(text=element_text(size=20))

# plot latency
df_latency
diff_mean_plot(df_latency, x="group_name", y="latency")
