library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggthemes)
library(ramify)
library(latex2exp)

df <- read_csv(file.path("data", "population_coupling.csv")) %>%
  mutate(cluster = factor(cluster, labels=c("Fast Firing", "Slow Irregular", "Slow Regular")),
         pop_coupled = abs(population_coupling) >= 3)


colors <- c(
  "Slow Regular"="#FF0100",
  "Fast Firing"="#00B34E",
  "Slow Irregular"="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)

theme_set(theme_ipsum()+ 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.text.x=element_text(size=13),
                  plot.title=element_text(size=12),
                  axis.title.y = element_text(colour = "black", size=16),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5),
                  axis.text = element_text(color="black", size=13),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=18),
                  legend.position = "none"))

df %>%
  ggplot(aes(x=population_coupling, y=cluster, fill=cluster)) +
  geom_density_ridges(scale=0.5) +
  geom_vline(xintercept = -3, linetype = "dashed") +
  geom_vline(xintercept = 3, linetype = "dashed") +
  labs(y="", x=TeX("Population Coupling Index ($Z_{cc, t=0}$)")) +
  xlim(-30, 30) +
  scale_fill_manual(values=colors) +
  ggtitle("All Neurons")

#####################################################################################

# Prop coupled

df %>%
  group_by(cluster) %>%
  summarise(prop_coupled = mean(pop_coupled) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=prop_coupled, y=cluster, color=cluster)) +
  geom_point(size=4) +
  xlim(0, 70) +
  scale_color_manual(values=colors) +
  labs(x="% Population Coupled", y="", title="All Neurons")

mod <- glm(pop_coupled ~ cluster, data=df, family="binomial")
anova(mod, test="Chisq")
pairwise.prop.test(table(df$cluster, df$pop_coupled))  # FF more than SR and SIR

######################################################################################

# Magnitude

df %>%
  filter(pop_coupled == T) %>%
  ggplot(aes(x=cluster, y=log(abs(population_coupling)), color=cluster)) +
  geom_boxplot(width=0.3, lwd=1) +
  coord_flip() +
  scale_color_manual(values=colors) +
  labs(x="", y="Log Magnitude of Coupling", title="Coupled Neurons")

mod <- df %>%
  filter(pop_coupled == T) %>%
  lm(abs(population_coupling) ~ cluster, data=.)

anova(mod)

######################################################################################

# Prop Neg

df_neg <- df %>% 
  filter(pop_coupled == T) %>%
  mutate(is_neg = population_coupling < 0)

df_neg %>%
  group_by(cluster) %>%
  summarise(prop_neg = mean(is_neg)) %>%
  ungroup() %>%
  ggplot(aes(x=prop_neg * 100, y=cluster, color=cluster)) +
  geom_point(size=4) +
  xlim(0, 70) +
  scale_color_manual(values=colors) +
  labs(x="% Negative", y="", title="Coupled Neurons")

mod <- glm(is_neg ~ cluster, data=df_neg, family="binomial")
anova(mod, test="Chisq")
pairwise.prop.test(table(df_neg$cluster, df_neg$is_neg))
