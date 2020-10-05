library(tidyverse)
library(scales)

# plot setup
colors <- c(
  `Slow Regular`="#332288",
  `Fast Firing`="#117733",
  `Slow Irregular`="#CC6677",
  `FALSE`="#7E8283",
  `TRUE`="#F6931D",
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
  filter(comb == "sr_sr")

df_cc <- read_csv(file.path("data", "cross_corr_simp.csv")) 

## RSC

# Plot: % pairs correlated
df_sc %>%
  summarise(pct = mean(p< 0.05),
            lab= percent(pct),
            pct = pct * 100
            ) %>%
  ggplot(aes(x=1, y=pct)) +
  geom_col(color="black", lwd=2, alpha=0.7) +
  geom_text(aes(label=(lab)), size=9, position=position_stack(vjust=0.5)) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=c(0, 50, 100), limits = c(0, 100)) +
  labs(x="", y="% Pairs Correlated")