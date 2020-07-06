library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggthemes)
library(clusteval)

# cluster_similarity(a,b, similarity="jaccard", method="independence")


df_cc <- read_csv(file.path("data", "cross_corr_simp.csv")) 

df_cc %>%
  select(spiketrain_1, spiketrain_2, binsize, lowest_p) %>%
  spread(binsize, lowest_p) %>%
  rename(CC_s=`0.001`, CC_l=`0.01`) %>%
  mutate(CC_s = CC_s < 0.05, CC_l= CC_l < 0.05)

df_rsc <- 

df_ersc <- read_csv(file.path("data", "evoked_rsc.csv")) %>%
  mutate(ersc= p < 0.05) %>%
  select(spiketrain_1, spiketrain_2, ersc)


