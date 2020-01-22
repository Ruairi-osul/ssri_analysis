library(car)
library(tidyverse)
library(lsmeans)
source(file.path("rscripts", "theme_pub.r"))
source(file.path("rscripts", "diff_mean_plot.r"))


data_dir <- file.path("data")
fig_dir <- file.path("figs")

df <- read_csv(file.path(data_dir, "spikecount_corr_1s_gw.csv"))
df_groups <- read_csv((file.path(data_dir, "chronic_baseline.csv")))

axes_labels <- c("SAL", "CIT", "WITH")

dfa <- df_groups %>% 
  select(neuron_id, group_name) %>%
  rename(neuron_1=neuron_id) %>%
  right_join(df, by="neuron_1") %>%
  mutate(pair_id=rownames(.),
         pearson_mag=abs(pearson_r),
         significantly_correlated = p<0.05)

df_sr_ff <- dfa %>%
  filter(group_1 == "slow_regular",
         group_2 == "fast_firing")

df_sr_ff %>%
  group_by(group_name) %>%
  summarise(proportion=mean(significantly_correlated)) %>%
  ggplot(aes(x=group_name, y=proportion)) +
    geom_bar(stat="identity", width=0.7, fill="black") +
    theme_Publication() +
    labs(y="Fast Firing - p5-HT\nProprtion p5-HT Pairs Correlated",
         x="") +
    theme(text=element_text(size=20)) +
    scale_x_discrete(labels=axes_labels) +
    ggsave(file.path(fig_dir, "ff-p5ht_Rsc_prop.png"))

diff_mean_plot(df_sr_ff, x="group_name", y='pearson_mag') +
  theme_Publication() +
  theme(legend.position="none") +
  labs(y="Fast Firing - p5-HT\nMagnitude of Correlation") +
  theme(text=element_text(size=20)) +
  scale_x_discrete(labels=axes_labels) 

model <- lm(pearson_mag ~ group_name, data=df_sr_ff)
anova(model)
pairwise.t.test(df_sr_ff$pearson_mag, df_sr_ff$group_name, 
                paired=F, p.adjust.method = "bonferroni")



#############################################################################
# sr_si
#############################################################################

df_sr_si <- dfa %>%
  filter(group_1 == "slow_irregular",
         group_2 == "slow_regular")

df_sr_si %>%
  group_by(group_name) %>%
  summarise(proportion=mean(significantly_correlated)) %>%
  ggplot(aes(x=group_name, y=proportion)) +
  geom_bar(stat="identity", width=0.7, fill="black") +
  theme_Publication() +
  labs(y="Proprtion p5-HT Pairs Correlated",
       x="") +
  theme(text=element_text(size=20)) +
  scale_x_discrete(labels=axes_labels) +
  ggsave(file.path(fig_dir, "p5ht-p5ht_Rsc.png"))

diff_mean_plot(df_sr_si, x="group_name", y='pearson_mag') +
  theme_Publication() +
  theme(legend.position="none") +
  labs(y="Magnitude of Correlation") +
  theme(text=element_text(size=20)) +
  scale_x_discrete(labels=axes_labels) 

model <- lm(pearson_mag ~ group_name, data=df_sr_si)
anova(model)
pairwise.t.test(df_sr_si$pearson_mag, df_sr_si$group_name, 
                paired=F, p.adjust.method = "bonferroni")

#########################################################################
