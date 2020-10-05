library(tidyverse)
library(ggthemes)
library(ggcorrplot)

# plot setup
colors <- c(
  `Slow Regular`="#332288",
  `Fast Firing`="#117733",
  `Slow Irregular`="#CC6677",
  `Not Spontaneously Active`="#7E8283",
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
df <- read_csv(file.path(data_dir, "baseshock_ramping.csv")) %>%
  mutate(response_baseshock=factor(response_baseshock, levels=c("Inhibited", "Non Responder", "Stimulated"),
                                   labels = c("Inhibited", "Non\nResponder", "Stimulated")),
         response_shock=factor(response_shock, levels=c("Inhibited", "Non Responder", "Stimulated"),
                               labels=c("Decrease", "No Change", "Increase"))
  ) %>%
  mutate(cluster=factor(cluster, labels=c("Fast Firing", "Not Spontaneously Active", "Slow Regular", "Slow Irregular")))

## Slow Regular Units

df %>%
  filter(cluster == "Slow Regular") %>%
  mutate(response_baseshock = fct_rev(response_baseshock)) %>%
  ggplot(aes(x=response_shock)) +
  geom_bar(color="black", fill="#332288", lwd=1, width=0.45) +
  labs(x="", y="Unit Count") +
  ggtitle("Firing Rate Change\nDuring Shock Period") + 
  ggsave(file.path( fig_dir, "ramp_responses_sr.svg"))


df %>%
  filter(cluster == "Slow Regular") %>%
  mutate(response_baseshock = fct_rev(response_baseshock))%>%
  ggplot(aes(x=response_shock)) +
    geom_bar(color="black", fill="#332288", lwd=1, width=0.45) +
  facet_grid(rows=vars(response_baseshock), scales="free") +
  scale_y_continuous(breaks=scales::pretty_breaks(3)) +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size=12.8)
        ) +
  labs(x="", y="Unit Count") +
  ggsave(file.path(fig_dir, "SR_ramp_dist.svg"))


df %>%
  filter(cluster == "Slow Regular") %>%
  mutate(response_shock = fct_rev(response_shock)) %>%
  ggplot(aes(x=response_baseshock, y=DOM_shock)) +
  geom_boxplot(lwd=1, color="#332288", width=0.3) +
  scale_y_continuous(breaks=scales::pretty_breaks(3)) +
  facet_grid(rows= vars(response_shock), scales="free") +
  labs(y="Spike Rate Change", x="Foot Shock Response") +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size=12.8)
  ) +
  ggsave(file.path(fig_dir, "SR_ramp_mag.svg"))

## Other Units

df %>%
  filter(cluster != "Slow Regular") %>%
  filter(cluster !="Not Spontaneously Active") %>%
  mutate(response_baseshock = fct_rev(response_baseshock)) %>%
  ggplot(aes(x=response_shock, fill=cluster)) +
  geom_bar(color="black", lwd=1, width=0.65, position = position_dodge(preserve = "single")) +
  labs(x="", y="Unit Count") +
  scale_fill_manual(values=colors) +
  scale_y_continuous(breaks=scales::pretty_breaks(5)) +
  ggtitle("Firing Rate Change\nDuring Shock Period") +
  theme(legend.position = "none") + 
  ggsave(file.path( fig_dir, "ramp_responses_other.svg"))


df %>%
  filter(cluster != "Slow Regular") %>%
  filter(cluster !="Not Spontaneously Active") %>%
  mutate(response_baseshock = fct_rev(response_baseshock))%>%
  ggplot(aes(x=response_shock, fill=cluster)) +
  geom_bar(color="black", lwd=1, width=0.7, position=position_dodge(preserve = "single")) +
  facet_grid(rows=vars(response_baseshock), scales="free") +
  scale_y_continuous(breaks=scales::pretty_breaks(3)) +
  scale_fill_manual(values=colors) +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size=12.8)
  ) +
  theme(legend.position = "none") +
  labs(x="", y="Unit Count") +
  ggsave(file.path(fig_dir, "Other_ramp_dist.svg"))


df %>%
  filter(cluster != "Slow Regular") %>%
  filter(cluster !="Not Spontaneously Active") %>%
  mutate(response_shock = fct_rev(response_shock)) %>%
  ggplot(aes(x=response_baseshock, y=DOM_shock, color=cluster)) +
  geom_boxplot(lwd=1, width=0.7, position=position_dodge2(preserve = "single", padding=0.2)) +
  scale_y_continuous(breaks=scales::pretty_breaks(3)) +
  scale_color_manual(values=colors) +
  facet_grid(rows= vars(response_shock), scales="free") +
  labs(y="Spike Rate Change", x="Foot Shock Response") +
  theme(panel.spacing = unit(1.5, "lines"),
        strip.text = element_text(size=12.8)
  ) +
  theme(legend.position = "none", legend.margin=margin(), legend.box="vertical") +
  ggsave(file.path(fig_dir, "Other_ramp_mag.svg"))
