library('here')
library('tidyverse')
library('ggplot2')
library('gridExtra')

imported <- here('data', 'clean_data.rds')
data <- readRDS(imported)
df <- data
#View(df)

str(df)

summary(df)

col_to_plot_num <- c("age", "systolic_bp", "diastolic_bp", "cholesterol_level", 'adverse_events')  # numeric only

plots <- list()

for(name in col_to_plot_num){
  p <- ggplot(df, aes(x = treatment_group, y = .data[[name]], fill = treatment_group)) +
    stat_summary(fun = mean, geom = "bar") +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),  # SD
                 geom = "errorbar", width = 0.2) +
    ylab(paste("Mean", name)) +
    labs(fill = "Treatment Group") +
    theme_minimal() 
  
  plots[[name]] <- p
}

grid.arrange(grobs = plots, nrow = 3, ncol = 2)

min_date <- as.Date(summary(df$enrollment_date)[1])
max_date <- as.Date(summary(df$enrollment_date)[6])


ggplot(df, aes(treatment_group, enrollment_date, fill = treatment_group)) +
  stat_summary(fun = 'mean', geom = 'bar') +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = 'errorbar', width = 0.2) +
  coord_cartesian(ylim = c(min_date, max_date)) +
  theme_minimal()

col_to_plot_perc <- c('gender', 'dropout', 'AE_present')

plots_perc <- list()

for(name in col_to_plot_perc){
  print(name)
  p <- ggplot(df, aes(treatment_group, fill = .data[[name]]))+
    geom_bar(position = 'fill')+
    scale_y_continuous(labels = scales::percent_format()) +
    ylab('Percentage') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  plots_perc[[name]] <- p
}
grid.arrange(grobs = plots_perc, nrow = 1, ncol = 3)

#plots_perc


#Remarks:

#Drug B patients have smaller variance of adverse effects, and less
#adverse effects on average

#Drug B patients seem to drop out more often

#However, differences are minimal, and not a basis for further investigation

#MANOVA

#Predictor: treatment group
#Outcome: diastolic_bp, systolic_bp
#Covariates: cholesterol level, age, gender

mancova_model <- manova(cbind(systolic_bp, diastolic_bp) ~ treatment_group + age + gender + cholesterol_level, data = df)
summary(mancova_model, test = 'Wilks')

#normality of residuals assumption

res <- residuals(mancova_model)

par(mfrow = c(1,2))

#systolic_bp qqplot
qqnorm(res[, 'systolic_bp'], main = 'systolic_bp')
qqline(res[, 'systolic_bp'], col='red')

#diastolic_bp qqplot
qqnorm(res[, 'diastolic_bp'], main = 'diastolic_bp')
qqline(res[, 'diastolic_bp'], col = 'blue')

#Multivariate normality was violated, thus I am moving to permutative MANOVA

