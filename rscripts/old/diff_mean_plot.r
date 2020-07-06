diff_mean_plot <- function(df, x="group_name", y='pearson_mag') {
  p <- df %>%
    ggplot(aes_(x=as.name(x), y=as.name(y))) + 
    geom_jitter(colour="black", alpha=0.2, width=0.1, show.legend = F) +
    geom_point(stat="summary", fun.y="mean", show.legend = F) + 
    geom_errorbar(stat="summary", fun.data="mean_se", 
                  fun.args = list(mult = 1.96), width=0.3,
                  show.legend = F) +
    labs(x='')
  return(p)
}