library(car)
library(tidyverse)
library(lsmeans)
source(file.path("rscripts", "theme_pub.r"))
source(file.path("rscripts", "diff_mean_plot.r"))


data_dir <- file.path("data")
fig_dir <- file.path("figs")

axes_labels <- c("SAL", "CIT", "WITH")

df <- read_csv(file.path(data_dir, "spikecount_corr_1s_sr.csv"))
df_groups <- read_csv((file.path(data_dir, "chronic_baseline.csv")))


dfa <- df_groups %>% 
  select(neuron_id, group_name) %>%
  rename(neuron_1=neuron_id) %>%
  right_join(df, by="neuron_1") %>%
  mutate(pair_id=rownames(.),
         pearson_mag=abs(pearson_r),
         significantly_correlated = p<0.05)

dfa %>%
  group_by(group_name) %>%
  summarise(proportion=mean(significantly_correlated)) %>%
  ggplot(aes(x=group_name, y=proportion)) +
  geom_bar(stat="identity", width=0.7, fill="black") +
  theme_Publication() +
  labs(y="p5HT - p5HT\nProprtion Correlated",
       x="") +
  theme(text=element_text(size=18)) +
  scale_x_discrete(labels=axes_labels) +
  lims(y=c(0, 1)) +
  ggsave(file.path(fig_dir, "p5ht-p5ht_Rsc_prop.png"))

diff_mean_plot(dfa, x="group_name", y='pearson_mag') +
  theme_Publication() +
  theme(legend.position="none") +
  labs(y="pHT - p5HT\nMagnitude of Correlation") +
  theme(text=element_text(size=20)) +
  scale_x_discrete(labels=axes_labels) +
  ggsave(file.path(fig_dir, "p5ht-p5ht_Rsc_mag.png"))

model_prop <- glm(significantly_correlated ~ group_name, family="binomial", data=dfa)
Anova(model_prop)
lsmeans(model_prop, pairwise ~ group_name)

model_mag <- lm(pearson_mag ~ group_name, data=dfa)
anova(model_mag)
