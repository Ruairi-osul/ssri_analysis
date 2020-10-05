# 05: Drug Effects
#
# Purpose:
#   Describe the effect of citalopram adminstration on the different neuron types
# 
# Requires:
#   ../data/baseline.csv
#     created by: ../notebooks/'01: Estimation of Neurotransmitter Expression.ipynb'
#   ../data/cit_effects.csv
#     created_by: ../notebooks/'04: Drug Effects NEW.ipynb'
#   ../data/cit_zcores.csv
#     created_by: ../notebooks/'04: Drug Effects NEW.ipynb'
#
# Produces:
#   plots:    
#     Barplot of Response counts by neuron type
#     Boxplot of effect size: AUC
#     Boxplot of effect size: Firing Rate Delta
#     Lineplots of zscore time series
#   models:
#     Association between neuron type and footshock response?
#     Association between neuron type and inhibition effect size?


library(tidyverse)
library(scales)

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
                  axis.text.x = element_text(color="black", size=15, angle = 45, hjust=0.9),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=15)))

# load datasets
df <- read_csv(file.path("data", "cit_effects.csv")) %>%
  filter(cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster, 
                          labels=c("Fast Firing", "Slow Irregular", "Slow Regular")))

dfz <- read_csv(file.path("data", "cit_zscores.csv")) %>%
  filter(cluster != "no_baseline") %>%
  mutate(cluster = factor(cluster, 
                          labels=c("Fast Firing", "Slow Irregular", "Slow Regular")),
         time = bin/60
         ) %>%
  filter(time <= 20)


# Plot: Proportion of Inhibited Neurons by Group

df %>% 
  mutate( was_inhibited=response_dm == "Inhibited",
          treatment=factor(treatment, labels=c("CIT", "SAL"))) %>%
  drop_na() %>%
  group_by(cluster, treatment) %>%
  summarise(frac_inhibited = mean(was_inhibited)) %>%
  ungroup() %>%
  ggplot(aes(x=treatment, y=frac_inhibited, fill=cluster)) +
  geom_col(position="identity") +
  facet_grid(cols=vars(cluster)) +
  scale_fill_manual(values=colors,
                    labels=c("Fast Firing", "Slow Irregular", "Slow Regular"),
                    name="") +  
  scale_y_continuous(breaks=scales::pretty_breaks(2)) +
  labs(x="", y="Proportion\nInhibited") +
  theme(strip.text = element_blank(),
        axis.text.x = element_text(size=13, angle=45, hjust = 0.7),
        legend.text = element_text(size=15),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) +
  ggsave(file.path(fig_dir, "DrugEffects_propInhibited.svg"))

# Plot: AUC Effect Size for Inhibited Neurons

df %>%
  filter(response_dm == "Inhibited",
         treatment == "Citalopram") %>%
  ggplot(aes(x=cluster, y=auc, color=cluster)) +
  geom_boxplot(width=0.3, lwd=1) +
  scale_color_manual(values=colors,
                    labels=c("Fast Firing", "Slow Irregular", "Slow Regular"),
                    name="") +
  labs(x="Cluster", y="AUC") +
  theme(axis.text.x=element_blank()) +
  ggsave(file.path(fig_dir, "DrugEffects_effectsize.svg"))

# Plot Firing Rate Delta for Inhibited Neurons

df %>%
  filter(response_dm == "Inhibited",
         treatment == "Citalopram") %>%
  ggplot(aes(x=cluster, y=diff_of_means, color=cluster)) +
  geom_boxplot(width=0.3, lwd=1) +
  scale_color_manual(values=colors,
                     name="") +
  labs(x="Cluster", y="Firing Rate Delta") +
  theme(axis.text.x=element_blank(),
          axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))+
  ggsave(file.path(fig_dir, "DrugEffects_effectsizeMFR.svg"))

# Plot Zscore Lines

dfz %>%
  filter(cluster=="Fast Firing") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram=colors[["Fast Firing"]])) +
  scale_fill_manual(values=c(Saline="#7E8283",Citalopram=colors[["Fast Firing"]]), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time [min]", y="Z Score", color="Fast Firing", fill="") +
  ggsave(file.path(fig_dir, "DrugEffects_LineFF.svg"))

dfz %>%
  filter(cluster=="Slow Regular") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram=colors[["Slow Regular"]])) +
  scale_fill_manual(values=c(Saline="#7E8283",Citalopram=colors[["Slow Regular"]]), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time [min]", y="Z Score", color="Slow Regular", fill="") +
  ggsave(file.path(fig_dir, "DrugEffects_LineSR.svg"))

dfz %>%
  filter(cluster=="Slow Irregular") %>%
  ggplot(aes(x=time, y=zscore, color=treatment, fill=treatment)) +
  geom_smooth(method="loess", alpha=0.3, size=1.3, span=0.8) +
  geom_vline(xintercept=0, linetype="dotted", size=1.1) +
  scale_color_manual(values=c(Saline="#7E8283", Citalopram=colors[["Slow Irregular"]])) +
  scale_fill_manual(values=c(Saline="#7E8283",Citalopram=colors[["Slow Irregular"]]), guide="none") +
  coord_cartesian(ylim=c(-4.5,3)) +
  scale_x_continuous(breaks=c(-10, 0, 10, 20)) +
  scale_y_continuous(breaks=c(-3,  0, 3)) +
  labs(x="Time [min]", y="Z Score", color="Slow Irregular", fill="") +
  ggsave(file.path(fig_dir, "DrugEffects_LineSIR.svg"))

# Model: Effect size difference between groups

mod <- df %>%
  filter(response_dm == "Inhibited",
         treatment == "CIT") %>%
  lm(auc ~ cluster, data=.)

anova(mod)
summary(mod)

# Model: Inhibited difference in prevelance between groups
df <- mutate(df, 
             cluster=factor(
               cluster, 
               levels=c("fast_firing", "slow_irregular","slow_regular"),
               labels=c("Fast Firing", "Slow Irregular", "Slow Regular")
               )
             )

contrasts(df$cluster) <- cbind(
  "others_v_sir"=c(1, -2, 1),
  "ff_v_sr"=c(-1, 0, 1)
)

mod <- df %>% 
  mutate( was_inhibited=response_dm== "Inhibited") %>%
  glm(was_inhibited ~ cluster * treatment, data=., family="binomial")
car::Anova(mod, type="III")
summary(mod)








