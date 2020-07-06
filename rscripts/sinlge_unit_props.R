library(tidyverse)
library(ggthemes)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  no_baseline="#7E8283",
  baseline="#F6931D"
)
theme_set(theme_gdocs() + 
            theme(axis.title.x = element_text(colour = "black", size=16),
                  axis.title.y = element_text(colour = "black", size=16),
                  axis.text = element_text(color="black", size=14),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=15)))

data_dir <- file.path("data")

dfb = read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(group_name %in% c("acute_citalopram", 
                          "acute_saline", 
                          "shock", 
                          "sham", 
                          "acute_cit", 
                          "acute_sal"),
         cluster != "no_baseline") %>%
  mutate(recording_method="Silicon\nProbe") %>%
  select(neuron_id, recording_method, cluster)

dfs <- read_csv(file.path(data_dir, "single_unit_labelled.csv")) %>%
  mutate(neuron_id=as.numeric(rownames(.)),
         recording_method="Glass\nPipette")

dfs_sub <- dfs %>%
  select(neuron_id, recording_method, cluster)

df <- bind_rows(dfb, dfs)

####################################################################

z.test <- function(z){
  2 * pnorm(-abs(z))
}

tab <- table(df$cluster, df$recording_method)


mod <- chisq.test(tab)

mod

dfz <- as.data.frame(mod$stdres) %>%
  rename(response=Var1, cluster=Var2, z=Freq)

dfz %>%
  mutate(p = z.test(z),
         pa = p.adjust(p, method="BH"))

######################################################

df %>% 
  ggplot(aes(x=recording_method, fill=cluster)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=colors, labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) + 
  labs(x="", y="Neuron Count") +
  theme(axis.text.x = element_text(color="black", size=15, angle = 0, hjust=0.5))


######################################################

# Scatter


dfs %>%
  rename(mean_firing_rate="Mean Firing Rate", cv2_isi="CV(ISI)") %>%
  ggplot(aes(x=cv2_isi, y=mean_firing_rate, color=cluster)) +
  geom_point(alpha=0.4) +
  scale_color_manual(values=colors, 
                     labels=c("Fast Firing", "Slow Irregular", "Slow Regular")) +
  xlim(0, 1.5) +
  labs(y="Mean Firing Rate [Hz]", x="Spike Regularity [CV(ISI)]") +
  theme(legend.position = c(0.7, 0.8), legend.background= element_rect(fill="NA"),
        axis.title.y=element_text(size=14),
        axis.title.x=element_text(size=14)) 


