library(tidyverse)
library(ggthemes)
library(ggcorrplot)

colors <- c(
  slow_regular="#FF0100",
  fast_firing="#00B34E",
  slow_irregular="#806A9D",
  "Non Responder"="#7E8283",
  Stimulated="#F6931D",
  Inhibited="#31A6D9"
)

data_dir <- file.path("data")

theme_set(theme_minimal() + 
            theme(axis.title.x = element_text(colour = "black", size=18),
                  axis.title.y = element_text(colour = "black", size=18),
                  strip.text = element_text(colour = "black", size=10.1, hjust=0.5),
                  axis.text.y = element_text(color="black", size=15),
                  axis.text.x = element_text(color="black", size=15, angle = 45, hjust=0.9),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size=15)))


df <- read_csv(file.path(data_dir, "psth_responses.csv")) %>%
  mutate(response_baseshock=factor(response_baseshock, levels=c("Inhibited", "Non Responder", "Stimulated")),
         response_first_5=factor(response_first_5, levels=c("Inhibited", "Non Responder", "Stimulated")),
         response_chalshock=factor(response_chalshock, levels=c("Inhibited", "Non Responder", "Stimulated"))
  ) %>%
  filter(cluster %in% c("slow_regular", "slow_irregular", "fast_firing")) %>%
  mutate(cluster=factor(cluster, labels=c("FF", "SIR", "SR")))

 

df %>% 
  ggplot(aes(x=cluster, fill=response_baseshock)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=colors) + 
  labs(x="Cluster", y="Neuron Count") +
  theme(axis.text.x = element_text(color=c("#00B34E", "#806A9D", "#FF0100"), size=15, angle = 0, hjust=0.5))

####################################################################

mod <- chisq.test(table(df$response_baseshock, df$cluster))
mod2 <- fisher.test(df$response_baseshock, df$cluster)
mod2
mod

mod$stdres
z.test <- function(z){
  2 * pnorm(-abs(z))
}

dfz <- as.data.frame(mod$stdres) %>%
  rename(response=Var1, cluster=Var2, z=Freq)

dfz %>%
  mutate(p = z.test(z),
         pa = p.adjust(p, method="hochberg"))

####################################################################

base <- df %>%
  select(response_baseshock, response_first_5) %>%
  drop_na() %>%
  pull(response_baseshock)

ff <- df %>%
  select(response_baseshock, response_first_5) %>%
  drop_na() %>%
  pull(response_first_5)

cs <- df %>%
  select(response_baseshock, response_chalshock) %>%
  drop_na() %>%
  pull(response_chalshock)

base_cs <- df %>%
  select(response_baseshock, response_chalshock) %>%
  drop_na() %>%
  pull(response_baseshock)

ff_cs <- df %>%
  select(response_first_5, response_chalshock) %>%
  drop_na() %>%
  pull(response_first_5)

cor.test(as.numeric(base), as.numeric(ff), method="spearman")
cor.test(as.numeric(base_cs), as.numeric(cs), method="spearman")
cor.test(as.numeric(ff_cs), as.numeric(cs), method="spearman")

mat <- matrix(c(1, 0.9137672, 0.845074, 0.9137672, 1, 0.8347166, 0.845074, 0.8347166, 1), nrow=3)

colnames(mat) <- c("First Period", "First Period\n5 min", "Second\nPeriod")
rownames(mat) <- c("First Period", "First Period\n5 min", "Second\nPeriod")

ggcorrplot(mat, lab=T, type="lower")
ggcorrplot(mat, lab=T)
