library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)

path <- here('data', 'clean_data.rds')
data_og <- readRDS(path)
df <- data_og

#1. based on site, investigate adverse effects, dropout, and AE_present (almost V)
#2. see if diastolic and systolic and cholesterol are connected to site


df_summary <- df %>%
  group_by(site_id) %>%
  summarise(mean_adverse_events = mean(adverse_events),
            mean_dp = mean(as.numeric(dropout) - 1),
            mean_systolic = mean(systolic_bp),
            mean_diastolic = mean(diastolic_bp)) %>%
  arrange(desc(mean_adverse_events))

summary(df_summary)

colnames_summary <- colnames(df_summary)[-1]
colnames_summary

colors <- c('green', 'salmon', 'blue', 'goldenrod')
color_id = 1
summary_plots <- list()
for(name in colnames_summary){
  p <- ggplot(df_summary, aes(x = site_id, y = .data[[name]], fill = .data[[name]]))+
          geom_col() +
          scale_fill_gradient(low = paste0('light', colors[color_id]), high = paste0('dark', colors[color_id]))
          theme_minimal()
  summary_plots[[name]] <- p
  color_id <- color_id + 1
}

grid.arrange(grobs = summary_plots, nrow = 2, ncol = 2)


#figure out a line plot, where y is the quantile of data, and x is the mean number of events (0.38, 2.83)
#ecdf manually


summary(df_summary$mean_adverse_events)

ad_events_range <- range(df_summary$mean_adverse_events)
possible_events_num <- seq(ad_events_range[1] - 0.001, ad_events_range[2] + 0.001, by = 0.001)

perc_ae_cases_left <- c()

for(i in 1:length(possible_events_num)){
  x <- mean(df_summary$mean_adverse_events > possible_events_num[i])
  perc_ae_cases_left[i] <- x
}

#Remark: lower boundary was 0.98, because 0.36364 > 0.3636363636...
#end even if it wasn't, >= would be needed

#Remark: upper boundary was 0.2, because 2.833 < 2.83333, so there was
#always left an x = 2.83333333... which was the remaining site (0.02).
plot(possible_events_num, perc_ae_cases_left, type = 'l')

mean(df_summary$mean_adverse_events > 1.57)
ae_cutoff <- 1.57

flagged_by_ae <- as.numeric(unlist(df_summary[df_summary$mean_adverse_events > ae_cutoff, 'site_id']))

#ecdf with a function

plot(ecdf(df_summary$mean_dp))


mean(df_summary$mean_dp > 0.297)
dp_cutoff <- 0.297
  
flagged_by_dp <- as.numeric(unlist(df_summary[df_summary$mean_dp > dp_cutoff, 'site_id']))

top_10_sites_ae <- as.numeric(unlist(df_summary[order(df_summary$mean_adverse_events, decreasing = TRUE)[1:10], 'site_id']))
top_10_sites_dp <- as.numeric(unlist(df_summary[order(df_summary$mean_dp, decreasing = TRUE)[1:10], 'site_id']))

inter_top_10 <- intersect(top_10_sites_dp, top_10_sites_ae)

flagged_sites <- sort(union(union(flagged_by_ae, flagged_by_dp), inter_top_10))
flagged_sites

#start preparing markdown report