# 02: Silicon Probe Yield
#
# Purpose:
#   Describe the number of neurons recorded in each session
# 
# Requires:
#   ../data/baseline.csv
#     created by: notebooks/'01: Estimation of Neurotransmitter Expression.ipynb'


library(tidyverse)

data_dir <- file.path("data")
fig_dir <- file.path("figs")

theme_set(
  theme_void() +
  theme(
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        )
)

df <- read_csv(file.path(data_dir, "baseline.csv")) %>%
  filter(
    group_name %in% c(
      "acute_citalopram",
      "acute_saline",
      "shock",
      "sham",
      "acute_cit",
      "acute_sal"
    ),
    cluster != "no_baseline"
  )

# Get mean recorded neurons per session

df %>%
  count(session_name) %>%
  summarise(m = mean(n))

# plot number of neurons per session with mean

df %>%
  count(session_name) %>%
  mutate(id = factor(rownames(.)),
         id = fct_reorder(id, n)) %>%
  ggplot(aes(y = id, x = n)) +
  geom_col(fill = "#7E8283", lwd = 10) +
  geom_vline(
    xintercept = 24.6,  # result from mean calculation
    linetype = "dashed",
    size = 1.3,
    color = "black"
  ) +
  theme_minimal() +
  labs(x = "Number of Neurons Recorded", y = "Individual Recording Sessions") +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
  ) +
  ggsave(file.path(fig_dir, "NumberOfNeuronsXSession.svg"))
