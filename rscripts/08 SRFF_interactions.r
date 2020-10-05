library(tidyverse)
library(scales)

# plot setup
colors <- c(
  `Slow Regular`="#332288",
  `Fast Firing`="#117733",
  `Slow Irregular`="#CC6677",
  `Not Significant`="#7E8283",
  `Significant`="#117733",
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

# load data
dfd <- read_csv(file.path("data", "distance.csv"))

df_sc <- read_csv(file.path("data", "rsctest_LC_sizes.csv")) %>%
  filter(binsize == 0.2) %>%
  filter(comb == "sr_ff")

df_cc <- read_csv(file.path("data", "cross_corr_simp.csv")) %>%
  filter(comb=="sr_ff")

df_ev <- read_csv(file.path("data", "evoked_rsc.csv")) %>%
  filter(comb=="sr_ff") %>%
  left_join(dfd)


# Plot: RSC Correlation Coef Dist
df_sc %>%
  mutate(significant= fct_rev(factor(p < 0.05, labels=c("Not Significant", "Significant")))) %>%
  ggplot(aes(x=R_spike_count, fill=significant)) +
  geom_histogram(bins=80, alpha=0.7) +
  geom_vline(xintercept=0, linetype="dashed", lwd=1) +
  scale_x_continuous(limits=c(-1, 1), breaks=c(-1, 0, 1)) +
  scale_fill_manual(values=colors) +
  labs(x="Pearson Correlation", y="Pair Counts") +
  theme(
    axis.text.x=element_text(angle=0)
  ) +
  ggsave(file.path(fig_dir, "SRFF_RscDist.svg"))


# Plot: Cross Correlation Long Dist

df_cc %>%
  filter(binsize==0.01) %>%
  filter(lowest_p < 0.05) %>%
  ggplot(aes(x=time_bin)) +
  geom_histogram(bins=80, alpha=0.7, fill="#117733") +
  geom_vline(xintercept=0, linetype="dashed", lwd=1) +
  scale_x_continuous(limits=c(-2, 2), breaks=c(-2, 0, 2)) +
  scale_fill_manual(values=colors) +
  labs(x="Lag of Max\nCross Correlation", y="Pair Counts") +
  theme(
    axis.text.x=element_text(angle=0)
  ) +
  ggsave(file.path(fig_dir, "SRFF_CCLDist.svg"))

df_cc %>%
  filter(binsize==0.001) %>%
  filter(lowest_p < 0.05) %>%
  ggplot(aes(x=time_bin)) +
  geom_histogram(bins=80, alpha=0.7, fill="#117733") +
  geom_vline(xintercept=0, linetype="dashed", lwd=1) +
  scale_x_continuous(limits=c(-0.2, 0.2), breaks=c(-0.2, 0, 0.2)) +
  scale_fill_manual(values=colors) +
  labs(x="Lag of Max\nCross Correlation", y="Pair Counts") +
  theme(
    axis.text.x=element_text(angle=0)
  ) +
  ggsave(file.path(fig_dir, "SRFF_CCSDist.svg"))

## Evoked

df_ev %>%
  mutate(significant=fct_rev(factor(p < 0.05, labels=c("Not Significant", "Significant")))) %>%
  ggplot(aes(x=r, fill=significant)) +
  geom_histogram(bins=80, alpha=0.7) +
  geom_vline(xintercept=0, linetype="dashed", lwd=1) +
  scale_x_continuous(limits=c(-1, 1), breaks=c(-1, 0, 1)) +
  scale_fill_manual(values=colors) +
  labs(x="Pearson Correlation", y="Pair Count") +
  theme(
    axis.text.x=element_text(angle=0,)
  ) +
  ggsave(file.path(fig_dir, "SRFF_EvDist.svg"))


# Plot: % Correlated: Rsc

# Plot: % Rsc Correlated
df_sc %>%
  summarise(pct = mean(p< 0.05),
            lab= percent(pct),
            pct = pct * 100
  ) %>%
  ggplot(aes(x=1, y=pct)) +
  geom_col(color="black", lwd=2, fill="#117733", alpha=0.7) +
  geom_text(aes(label=(lab)), size=8, position=position_stack(vjust=0.5)) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=c(0, 50, 100), limits = c(0, 100)) +
  labs(x="", y="% Pairs Correlated") +
  ggsave(file.path(fig_dir, "SRFF_RscPercent.svg"))

# Plot: % Correlated: Cross Correlated long

df_cc %>%
  filter(binsize==0.01) %>%
  summarise(pct = mean(lowest_p< 0.05),
            lab= percent(pct),
            pct = pct * 100
  ) %>%
  ggplot(aes(x=1, y=pct)) +
  geom_col(color="black", lwd=2, alpha=0.7, fill="#117733") +
  geom_text(aes(label=(lab)), size=7, position=position_stack(vjust=0.5), ) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=c(0, 50, 100), limits = c(0, 100)) +
  labs(x="", y="% Pairs Correlated") +
  ggsave(file.path(fig_dir, "SRFF_CCLPercent.svg"))

# Plot: % Correlated: Cross correlated short
df_cc %>%
  filter(binsize==0.001) %>%
  summarise(pct = mean(lowest_p< 0.05),
            lab= percent(pct),
            pct = pct * 100
  ) %>%
  ggplot(aes(x=1, y=pct)) +
  geom_col(color="black", lwd=2, alpha=0.7, fill="#117733") +
  geom_text(aes(label=(lab)), size=7, position=position_stack(vjust=0.5), ) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=c(0, 50, 100), limits = c(0, 100)) +
  labs(x="", y="% Pairs Correlated") +
  ggsave(file.path(fig_dir, "SRFF_CCSPercent.svg"))

# Plot % Correlated: Evoked

df_ev %>%
  summarise(pct = mean(p< 0.05),
            lab= percent(pct),
            pct = pct * 100
  ) %>%
  ggplot(aes(x=1, y=pct)) +
  geom_col(color="black", lwd=2, alpha=0.7, fill="#117733") +
  # geom_text(aes(label=(lab)), size=7, position=position_stack(vjust=0.5), ) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=c(0, 50, 100), limits = c(0, 100)) +
  labs(x="", y="% Pairs Correlated") +
  ggsave(file.path(fig_dir, "SRFF_EvPercent.svg"))
