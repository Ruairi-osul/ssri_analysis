library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(ggthemes)
library(clusteval)
library(latex2exp)
library(ggcorrplot)

# cluster_similarity(a,b, similarity="jaccard", method="independence")


df_cc <- read_csv(file.path("data", "cross_corr_simp.csv")) %>%
  select(spiketrain_1, spiketrain_2, binsize, lowest_p) %>%
  spread(binsize, lowest_p) %>%
  rename(CC_s=`0.001`, CC_l=`0.01`) %>%
  mutate(CC_s = CC_s < 0.05, CC_l= CC_l < 0.05) 

df <- read_csv(file.path("data", "rsctest_LC_sizes.csv")) %>%
  filter(binsize==1) %>%
  mutate(rsc = p < 0.05) %>%
  select(spiketrain_1, spiketrain_2, rsc) %>%
  left_join(df_cc)

df

dfs <- read_csv(file.path("data", "evoked_rsc.csv")) %>%
  mutate(ersc= p < 0.05) %>%
  select(spiketrain_1, spiketrain_2, ersc) %>%
  left_join(df)

RSC_RSC <- cluster_similarity(df$rsc, df$rsc, similarity="jaccard", method="independence")
RSC_ERSC <- cluster_similarity(dfs$rsc, dfs$ersc, similarity="jaccard", method="independence")
RSC_CCS <- cluster_similarity(df$rsc, df$CC_s, similarity="jaccard", method="independence")
RSC_CCL <- cluster_similarity(df$rsc, df$CC_l, similarity="jaccard", method="independence")

ERSC_RSC <- cluster_similarity(dfs$ersc, dfs$rsc, similarity="jaccard", method="independence")
ERSC_ERSC <- cluster_similarity(dfs$ersc, dfs$ersc, similarity="jaccard", method="independence")
ERSC_CCS <- cluster_similarity(dfs$ersc, dfs$CC_s, similarity="jaccard", method="independence")
ERSC_CCL <- cluster_similarity(dfs$ersc, dfs$CC_l, similarity="jaccard", method="independence")

CCS_RSC <- cluster_similarity(df$CC_s, df$rsc, similarity="jaccard", method="independence")
CCS_ERSC <- cluster_similarity(dfs$CC_s, dfs$ersc, similarity="jaccard", method="independence")
CCS_CCS <- cluster_similarity(df$CC_s, df$CC_s, similarity="jaccard", method="independence")
CCS_CCL <- cluster_similarity(df$CC_s, df$CC_l, similarity="jaccard", method="independence")

CCL_RSC <- cluster_similarity(df$CC_l, df$rsc, similarity="jaccard", method="independence")
CCL_ERSC <- cluster_similarity(dfs$CC_l, dfs$ersc, similarity="jaccard", method="independence")
CCL_CCS <- cluster_similarity(df$CC_l, df$CC_s, similarity="jaccard", method="independence")
CCL_CCL <- cluster_similarity(df$CC_l, df$CC_l, similarity="jaccard", method="independence")


mat <- matrix(c(RSC_RSC, 
                RSC_ERSC, 
                RSC_CCS, 
                RSC_CCL, 
                
                ERSC_RSC, 
                ERSC_ERSC, 
                ERSC_CCS, 
                ERSC_CCL, 
                
                CCS_RSC,
                CCS_ERSC,
                CCS_CCS,
                CCS_CCL,
                
                CCL_RSC,
                CCL_ERSC,
                CCL_CCS,
                CCL_CCL), nrow=4)

labs <- c(TeX("Spontaneous $R_{sc}$"), TeX("Evoked $R_{sc}$"), TeX("$CC_s$"),  TeX("$CC_l$"))
labs <- c("Spike Count: S", "Spike Count: Ev", "CC: Short", "CC: Long")
colnames(mat) <- labs
rownames(mat) <- labs

ggcorrplot(mat, lab=T, type="lower")
ggcorrplot(mat, lab=T) + labs(fill = "legend title")
