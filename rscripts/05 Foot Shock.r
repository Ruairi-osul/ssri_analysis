# 05: Footshock Responses
#
# Purpose:
#   The effect of the foot shock on DRN neuronal activity
# 
# Requires:
#   ../data/baseline.csv
#     created by: ../notebooks/'01: Estimation of Neurotransmitter Expression.ipynb'
#   ../psth_responses.csv
#     created_by: ../notebooks/'03: Baseshock Responses.ipynb'
#
# Produces:
#   plots:    
#     Barplot of Response counts by neuron type
#     Heatmap of response correlations at different time points
#   models:
#     Test hypothesis of association between neuron type and footshock response


library(tidyverse)
library(ggthemes)
library(ggcorrplot)
library(caret)

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

# dir setup
data_dir <- file.path("data")
fig_dir <- file.path("figs")

# load datasets
df <- read_csv(file.path(data_dir, "psth_responses.csv")) %>%
  mutate(response_baseshock=factor(response_baseshock, levels=c("Inhibited", "Non Responder", "Stimulated")),
         response_first_5=factor(response_first_5, levels=c("Inhibited", "Non Responder", "Stimulated")),
         response_last_5=factor(response_first_5, levels=c("Inhibited", "Non Responder", "Stimulated")),
         response_chalshock=factor(response_chalshock, levels=c("Inhibited", "Non Responder", "Stimulated"))
  ) %>%
  mutate(cluster=factor(cluster, labels=c("FF", "NSA", "SIR", "SR")))

# Plot How did the Different Neuron Types Respond?

df %>% 
  ggplot(aes(x=cluster, fill=response_baseshock)) +
  geom_bar(position=position_dodge()) +
  scale_fill_manual(values=colors) + 
  labs(x="Cluster", y="Neuron Count") +
  theme(
    axis.text.x = element_text(
      color=c(colors[["Fast Firing"]], colors[["Slow Irregular"]], colors[["Slow Regular"]]),
      size=15, angle = 0, hjust=0.5)
    ) +
  ggsave(file.path(fig_dir, "Footshock_ClusterResponseCounts.svg"))

# Model: was baseshock associated with neuron type?

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

## Did neurons change how they responded?

# SR neurons
library(yardstick)

df1 <- df %>%
  filter(cluster == "SR")


df1
# mat <- conf_mat(df1, response_first_5, response_last_5, dnn=c("1st Period", "1st Period\nFirst 5min"))
# autoplot(mat, type="heatmap") +
#   ggsave(file.path(fig_dir, "confMat_sr_ff.svg"))

mat <- conf_mat(df1, response_baseshock, response_chalshock, dnn=c("1st Period", "1st Period\nFirst 5min"))
# autoplot(mat, type="heatmap") +
  # ggsave(file.path(fig_dir, "confMat_base_chal.svg"))

df2 <- df %>%
  filter(cluster != "SR")

mat <- conf_mat(df2, response_first_5, response_last_5, dnn=c("1st Period", "1st Period\nFirst 5min"))
autoplot(mat, type="heatmap") +
  ggsave(file.path(fig_dir, "confMat_ff_other.svg"))

mat <- conf_mat(df2, response_baseshock, response_chalshock, dnn=c("1st Period", "1st Period\nFirst 5min"))
autoplot(mat, type="heatmap") +
  ggsave(file.path(fig_dir, "confMat_base_chal_other.svg"))

## No NSA

df3 <- df %>%
  filter(cluster != "SR") %>%
  filter(cluster != "NSA")

mat <- conf_mat(df3, response_first_5, response_last_5, dnn=c("1st Period", "1st Period\nFirst 5min"))
autoplot(mat, type="heatmap") +
  ggsave(file.path(fig_dir, "confMat_ff_other_NONSA.svg"))

mat <- conf_mat(df3, response_baseshock, response_chalshock, dnn=c("1st Period", "1st Period\nFirst 5min"))
autoplot(mat, type="heatmap") +
  ggsave(file.path(fig_dir, "confMat_base_chal_other_NONSA.svg"))
#### Just SR


df2 %>% count(response_last_5)


df %>%
  filter(cluster == "SR") %>%
  ggplot(aes(x=response_baseshock)) +
  geom_bar(color="black", fill="#332288", lwd=1, width=0.7) +
  labs(y="Count", x="") +
  ggsave(file.path(fig_dir, "SR_baseshock_responses.svg"))


## Just FF
df %>%
  filter(cluster == "FF") %>%
  ggplot(aes(x=response_baseshock)) +
  geom_bar(color="black", fill="#117733", lwd=1, width=0.7) +
  labs(y="Count", x="") +
  ggsave(file.path(fig_dir, "FF_baseshock_responses.svg"))

## Just SIR
df %>%
  filter(cluster == "SIR") %>%
  ggplot(aes(x=response_baseshock)) +
  geom_bar(color="black", fill="#CC6677", lwd=1, width=0.7) +
  labs(y="Count", x="") +
  ggsave(file.path(fig_dir, "SIR_baseshock_responses.svg"))

##
df %>%
  filter(cluster == "NSA") %>%
  ggplot(aes(x=response_baseshock, group=cluster)) +
  geom_bar(color="black", fill="#7E8283", lwd=1, width=0.7) +
  labs(y="Count", x="") +
  ggsave(file.path(fig_dir, "NSA_baseshock_responses.svg")) 


df %>% count(cluster, response_baseshock)
