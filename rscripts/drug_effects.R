library(tidyverse)
library(scales)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  stimulated="#F6931D",
  inhibited="#31A6D9"
)

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=20),
                  axis.title.y = element_text(colour = "black", size=20),
                  strip.text = element_text(color="black", size=15),
                  axis.text.y = element_text(color="black", size=15),
                  axis.text.x = element_text(color="black", size=15, angle = 0, hjust=0.9),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=20)))

df <- read_csv(file.path("data", "cit_effects.csv"))


df <- mutate(df, cluster=factor(cluster, 
                                levels=c("fast_firing", 
                                         "slow_irregular",
                                         "slow_regular")))

contrasts(df$cluster) <- cbind(
  "others_v_sir"=c(1, -2, 1),
  "ff_v_sr"=c(-1, 0, 1)
)
df

mod <- df %>% 
  mutate( was_inhibited=response_dm== "Inhibited") %>%
  glm(was_inhibited ~ cluster * treatment, data=., family="binomial")
car::Anova(mod, type="III")
summary(mod)


df %>% 
  mutate( was_inhibited=response_dm== "Inhibited",
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
  labs(x="Treatment", y="Proportion Inhibited") +
  theme(strip.text = element_blank(),
        axis.text.x = element_text(size=13, angle=45, hjust = 0.7),
        legend.text = element_text(size=15),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

df %>%
  filter(response_dm == "Inhibited",
         treatment == "Citalopram") %>%
  ggplot(aes(x=cluster, y=auc, color=cluster)) +
  geom_boxplot(width=0.3, lwd=1) +
  scale_color_manual(values=colors,
                    labels=c("Fast Firing", "Slow Irregular", "Slow Regular"),
                    name="") +
  labs(x="Cluster", y="AUC") +
  theme(axis.text.x=element_blank())

mod <- df %>%
  filter(response_dm == "Inhibited",
         treatment == "CIT") %>%
  lm(auc ~ cluster, data=.)

anova(mod)
summary(mod)

##################################################


df %>%
  filter(response_dm == "Inhibited",
         treatment == "CIT") %>%
  select(neuron_id, cluster, diff_of_means, auc) %>%
  rename("\u0394 Spike Rate [Hz]"=diff_of_means, "Z Score AUC"=auc) %>%
  gather(metric, score, -neuron_id, -cluster) %>%
  ggplot(aes(x=cluster, y=score, color=cluster)) +
  geom_boxplot(width=0.3, lwd=1) +
  scale_color_manual(values=colors,
                    labels=c("Fast Firing", "Slow Irregular", "Slow Regular"),
                    name="") +
  labs(x="Inhibited\nNeurons", y="") +
  coord_flip() +
  theme(axis.text.y=element_blank()) +
  facet_grid(cols=vars(metric), scales="free")





