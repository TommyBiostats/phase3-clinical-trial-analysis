library(here)
library(dplyr)
library(stringr)

#import the data
data <- here('data', 'synthetic_clinical_trial_data.csv')
trial_data <- read.csv(data)
#trial_data <- read_csv("C:/Users/Bakugan/Desktop/R/home_projects/CRO_analysis_2025.09/data/synthetic_clinical_trial_data.csv")

#check for NA
anyNA(trial_data)

#inspect duplicates
duplicates <- trial_data[duplicated(trial_data) | duplicated(trial_data, fromLast = TRUE), ]

#rename
df <- trial_data
colnames(df) <- tolower(colnames(df))
colnames(df)

#fix column classes
str(df)

##gender
unique(trial_data$Gender)
df$gender <- factor(df$gender)
class(df$gender)

##date
df$enrollment_date<- as.Date(df$enrollment_date, format = '%m/%d/%y')

##treamtent_group
df$treatment_group <- factor(df$treatment_group)
class(df$treatment_group)

##droput
df$dropout <- factor(df$dropout, levels = c(0,1), labels = c('No', "Yes"))

str(df)

#overview
summary(df)

df[df$'cholesterol_level' < 100,]
#unlikely, but possible

#adverse events category
table(df$adverse_events)
df$AE_present <- factor(df$adverse_events > 0,
                       levels = c(TRUE, FALSE),
                       labels = c('Yes', 'No')
                       )

#View(df[c('AE_present', 'adverse_events')])

View(df)

#saveRDS(df, 'clean_data.rds')

#write.csv(df, 'heart_data_clean.csv', row.names = FALSE)


