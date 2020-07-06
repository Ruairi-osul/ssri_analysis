library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggthemes)
library(EnvStats)

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


###############################################################################

dfd <- read_csv(file.path("data", "distance.csv"))

df1 <- read_csv(file.path("data", "rsctest_LC_sizes.csv")) %>%
  filter(binsize==1) %>%
  mutate(comb = factor(comb, levels=c("sr_sr", "sr_sir", "sr_ff", "sir_sir", "sir_ff", "ff_ff"),
                       labels=c("SR:SR", "SR:SIR", "SR:FF", "SIR:SIR", "SIR:FF", "FF:FF")),
         label = ave(.$R_spike_count, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(comb, label))

df1 %>%
  filter(p < 0.05) %>%
  mutate(label = ave(.$R_spike_count, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(as.character(comb), label))%>%
  ggplot(aes(x=R_spike_count, y=comb_labeled)) +
  geom_density_ridges(scale=0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y="", x="Pearson Correlation Coefficient") +
  ggtitle("Significantly Correlated Neuron Pairs") +
  xlim(-1, 1) +
  theme(axis.text.x=element_text(size=13))


df1 %>%
  group_by(comb_labeled) %>%
  summarise("% of Pairs Correlated"=mean(p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb_labeled)) + 
  geom_point(size=4) +
  labs(y="") +
  xlim(0, 100) +
  ggtitle("All Neuron Pairs")
  
#####################################################################

df200 <- read_csv(file.path("data", "rsctest_LC_sizes.csv")) %>%
  filter(binsize==0.2) %>%
  mutate(comb = factor(comb, levels=c("sr_sr", "sr_sir", "sr_ff", "sir_sir", "sir_ff", "ff_ff"),
                       labels=c("SR:SR", "SR:SIR", "SR:FF", "SIR:SIR", "SIR:FF", "FF:FF")),
         label = ave(.$R_spike_count, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(comb, label)) %>%
  left_join(dfd)

###########################################################################


df200 %>%
  filter(p < 0.05) %>%
  mutate(label = ave(.$R_spike_count, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(as.character(comb), label))%>%
  ggplot(aes(x=R_spike_count, y=comb_labeled)) +
  geom_density_ridges(scale=0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(y="", x="Pearson Correlation Coefficient") +
  xlim(-1, 1) +
  ggtitle("Significantly Correlated Neuron Pairs")


df200_wc <- df200 %>%
  mutate(was_correlated = p < 0.05)

chisq.test(df200_wc$was_correlated, factor(df200_wc$comb_labeled))

pairwise.prop.test(table(df200_wc[, c("comb_labeled", "was_correlated")]))



###########################################

df200 %>%
  group_by(comb_labeled) %>%
  summarise("% of Pairs Correlated"=mean(p < 0.05) * 100) %>%
  ungroup() %>%
  ggplot(aes(x=`% of Pairs Correlated`, y=comb_labeled)) + 
  geom_point(size=4) +
  labs(y="") +
  xlim(0, 100) +
  ggtitle("All Neuron Pairs")

##############################################

# Were correlations stronger in certain groups?

df200_mag <- df200 %>%
  filter(p < 0.05) %>%
  mutate(was_correlated = p < 0.05,
         corr_mag = abs(R_spike_count))

mod <- lm(corr_mag ~ comb, data=df200_mag)
anova(mod)  

pairwise.t.test(x=df200_mag$corr_mag, g=df200_mag$comb)

df200_mag %>%
  mutate(comb=factor(comb),
         comb=fct_reorder(comb, corr_mag, mean)) %>%
  ggplot(aes(x=comb, y=corr_mag)) +
  geom_boxplot(width=0.3) +
  coord_flip() +
  labs(y="Magnitude of Correlation", x="")

##################################################

# Were negative correlations more common in certain groups

df200_2 <- df200 %>%
  filter(p < 0.05) %>%
  mutate(was_negative = R_spike_count < 0) 



df200_2 %>%
  mutate(label = ave(.$R_spike_count, .$comb, FUN=function(x)paste0("(n=",length(x), ")")),
         comb_labeled = paste(as.character(comb), label),
         ) %>%
  group_by(comb_labeled) %>%
  summarise("% of Correlated Pairs Negative"=mean(R_spike_count < 0) * 100) %>%
  ungroup() %>%
  mutate(comb_labeled=fct_reorder(comb_labeled, `% of Correlated Pairs Negative`)) %>%
  ggplot(aes(x=`% of Correlated Pairs Negative`, y=comb_labeled)) + 
  geom_point(size=4) +
  labs(y="") +
  xlim(0, 40) +
  ggtitle("Significantly Correlated Pairs")

chisq.test(df200_2$was_negative, factor(df200_2$comb))
pairwise.prop.test(table(df200_2[, c("comb", "was_negative")]))

####################################################

# distance


df1 <- df200 %>%
  mutate(was_correlated = p < 0.05)

df1 %>%
  ggplot(aes(x=distance, y=as.numeric(was_correlated))) +
    geom_point(alpha=0.002, size=2, position = position_jitter(w = 8, h = 0)) +
  stat_smooth(method="glm",
              method.args = list(family="binomial"), se=TRUE,
              fullrange=TRUE, size=1.3) +
  theme(legend.position="left",
        axis.title.y = element_text(colour = "black", size=13),
        axis.title.x = element_text(colour = "black", size=13)) +
  labs(x="Distance Between Channels [um]", y="Probability of Correlation")

mod <- glm(was_correlated ~ distance, data=df1, family="binomial")
anova(mod, test="Chisq")
summary(mod)
