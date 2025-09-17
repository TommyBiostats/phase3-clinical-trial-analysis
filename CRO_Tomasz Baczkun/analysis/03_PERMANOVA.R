#Predictor: treatment group
#Outcome: diastolic_bp, systolic_bp
#Covariates: cholesterol level, age, gender

library(here)
library(vegan)

way <- here('data', 'clean_data.rds')
data <- readRDS(way)
df <- data

df$systolic_bp_scaled <- scale(df$systolic_bp)
df$diastolic_bp_scaled <- scale(df$diastolic_bp)

#homogeneity of dispersion assumption

X <- df[c('systolic_bp_scaled', 'diastolic_bp_scaled')]
D <- dist(X, 'euclidian')

bd <- betadisper(D, df$treatment_group)
perm_results <- permutest(bd, permutations = 1000)
perm_results #assumption is met

#permanova testing

set.seed(1976)

fit <- adonis2(D ~ treatment_group + cholesterol_level + age + gender,
               data = df, permutations = 1000, parallel = 20, by = 'margin')

fit


