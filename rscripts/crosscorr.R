library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggthemes)
library(ramify)


hrbrthemes::import_roboto_condensed()

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

dfd <- read_csv(file.path("data", "distance.csv"))

################ short

df_small <- read_csv(file.path("data", "cross_corr_simp.csv")) %>%
  filter(binsize==0.001) %>%
  mutate(comb = factor(comb, levels=c("sr_sr", "sr_sir", "sr_ff", "sir_sir", "sir_ff", "ff_ff"),
                       labels=c("SR:SR", "SR:SIR", "SR:FF", "SIR:SIR", "SIR:FF", "FF:FF")),
         label = ave(.$time_bin, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(comb, label),
         was_correlated = lowest_p < 0.05) %>%
  left_join(dfd)

df_small %>%
  group_by(comb_labeled) %>%
  summarise("% of Pairs Correlated"=mean(lowest_p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb_labeled)) + 
  geom_point(size=4) +
  labs(y="") +
  xlim(0, 100) +
  ggtitle("All Neuron Pairs")

# Dist

df_small %>%
  filter(lowest_p < 0.05) %>%
  mutate(label = ave(.$time_bin, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(as.character(comb), label))%>%
  ggplot(aes(x=time_bin, y=comb_labeled)) +
  geom_density_ridges(scale=0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y="", x="Time of Peak Cross-correlation [sec]") +
  xlim(-0.2, 0.2) +
  ggtitle("Significantly Cross-correlated Neuron Pairs")


mod <- glm(was_correlated ~ distance, data=df_small, family="binomial")
anova(mod, test="Chisq")
summary(mod)

df_small %>%
  ggplot(aes(x=distance, y=as.numeric(was_correlated))) +
    geom_point(alpha=0.002, size=2, position = position_jitter(w = 8, h = 0)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE, size=1.3) +
  theme(legend.position="left",
        axis.title.y = element_text(colour = "black", size=13),
        axis.title.x = element_text(colour = "black", size=13)) +
  labs(x="Distance Between Channels [um]", y="Probability of Correlation")

################ long

df_big <- read_csv(file.path("data", "cross_corr_simp.csv")) %>%
  filter(binsize==0.01) %>%
  mutate(comb = factor(comb, levels=c("sr_sr", "sr_sir", "sr_ff", "sir_sir", "sir_ff", "ff_ff"),
                       labels=c("SR:SR", "SR:SIR", "SR:FF", "SIR:SIR", "SIR:FF", "FF:FF")),
         label = ave(.$time_bin, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(comb, label),
         was_correlated = lowest_p < 0.05) %>%
  left_join(dfd)

df_big %>%
  group_by(comb_labeled) %>%
  summarise("% of Pairs Correlated"=mean(lowest_p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb_labeled)) + 
  geom_point(size=4) +
  labs(y="") +
  xlim(0, 100) +
  ggtitle("All Neuron Pairs")

# Dist

df_big %>%
  filter(lowest_p < 0.05) %>%
  mutate(label = ave(.$time_bin, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(as.character(comb), label))%>%
  ggplot(aes(x=time_bin, y=comb_labeled)) +
  geom_density_ridges(scale=0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y="", x="Time of Peak Cross-correlation [sec]") +
  xlim(-2, 2) +
  ggtitle("Significantly Cross-correlated Neuron Pairs")


df_big <- df_big %>%
  left_join(dfd) %>%
  mutate(was_correlated = lowest_p < 0.05)

mod <- glm(was_correlated ~ distance, data=df_big, family="binomial")
anova(mod, test="Chisq")
summary(mod)

df_big %>%
  mutate(predicted=predict(mod, newdata=df_big, type="response"),
         was_correlated = was_correlated*1,
         bin=cut(.$distance, seq(0, 400, 100))) %>%
  ggplot(aes(x=distance, y=predicted)) +
  geom_line()

df_big %>%
  ggplot(aes(x=distance, y=as.numeric(was_correlated))) +
    geom_point(alpha=0.002, size=2, position = position_jitter(w = 8, h = 0)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE, size=1.3) +
  theme(legend.position="left",
        axis.title.y = element_text(colour = "black", size=13),
        axis.title.x = element_text(colour = "black", size=13)) +
  labs(x="Distance Between Channels [um]", y="Probability of Correlation")

