library(tidyverse)
library(lsmeans)
source(file.path("rscripts", "theme_pub.r"))
source(file.path("rscripts", "diff_mean_plot.r"))


data_dir <- file.path("data")
fig_dir <- file.path("figs")

axes_labels <- c("SAL", "CIT", "WITH")

df <- read_csv(file.path(data_dir, "population_coupling.csv"))
df_groups <- read_csv((file.path(data_dir, "chronic_baseline.csv")))

dfa <- df_groups %>% 
  select(neuron_id, group_name, type) %>%
  right_join(df) 

df1 <- dfa %>%
  filter(time_sec==0)


#############################
# 5ht
#############################

df_5ht = df1 %>%
          filter(type == "slow_regular")

diff_mean_plot(df=df_5ht, x="group_name", y="zscore") +
  theme_Publication() +
  scale_x_discrete(labels=axes_labels) +
  theme(text=element_text(size=20)) +
  labs(color="", x="", y="Z Score Cross Correlation", title="p-5HT") +
  ggsave(file.path(fig_dir, "sr_population_cross_corr.png"))
  

model_5ht <- lm(zscore ~ group_name, data=df_5ht)
anova(model_5ht)
pairwise.t.test(df_5ht$zscore, df_5ht$group_name, 
                paired=F, p.adjust.method = "bonferroni")

#############################
# si
#############################

df_si = df1 %>%
  filter(type == "slow_irregular")

diff_mean_plot(df=df_si, x="group_name", y="zscore") +
  theme_Publication()+
  scale_x_discrete(labels=axes_labels) +
  theme(text=element_text(size=20)) +
  labs(color="", x="", y="Z Score Cross Correlation", title="Slow Irregular") +
  ggsave(file.path(fig_dir, "si_population_cross_corr.png"))

model_si <- lm(zscore ~ group_name, data=df_si)
anova(model_si)
pairwise.t.test(df_si$zscore, df_si$group_name, 
                paired=F, p.adjust.method = "bonferroni")



#############################
# ff
#############################

df_ff = df1 %>%
  filter(type == "fast_firing")

diff_mean_plot(df=df_ff, x="group_name", y="zscore") +
  theme_Publication() +
  scale_x_discrete(labels=axes_labels) +
  theme(text=element_text(size=20)) +
  labs(color="", x="", y="Z Score Cross Correlation", title="Fast Firing") +
  ggsave(file.path(fig_dir, "ff_population_cross_corr.png"))

model_ff <- lm(zscore ~ group_name, data=df_ff)
anova(model_ff)
pairwise.t.test(df_ff$zscore, df_ff$group_name, 
                paired=F, p.adjust.method = "bonferroni")

##########################################################
# Example neurons
##########################################################
NEURON_POS <- 1541
NEURON_NULL <- 1559

dfa %>%
  filter(neuron_id %in% c(1541, 1559)) %>%
  mutate(response=if_else(neuron_id==NEURON_POS, "Positively-coupled", "Non-Population-coupled")) %>%
  ggplot() +
  geom_line(aes(x=time_sec, y=zscore, color=as.factor(response))) +
  theme_Publication() +
  scale_colour_Publication() +
  labs(color="", x="Time [sec]", y="Z Score Cross Correlation") +
  theme(text=element_text(size=20)) +
  ggsave(file.path(fig_dir, "example_population_cross_corr.png"))
  
