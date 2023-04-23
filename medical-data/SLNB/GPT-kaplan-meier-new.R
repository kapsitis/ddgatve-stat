# install.packages("survival")

library(survival)
library(ggplot2)
library(survminer)

# Read data from CSV file
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)


data$Gads <- as.Date(data$Gads, format = "%m/%d/%y")
data$Mirsanasdatums <- as.Date(data$Mirsanasdatums, format = "%m/%d/%Y")
data$survival_time <- as.numeric(difftime(data$Mirsanasdatums, data$Gads, units = "days"))


# Create a censor variable (1 for event, 0 for censor):
data$censor <- ifelse(is.na(data$Mirsanasdatums), 0, 1)

# Create a disease-specific death variable (1 for disease-specific death, 0 for other deaths):

data$disease_specific_death <- ifelse(data$Navescelonis == "Ļaundabīga ādas melanoma", 1, 0)


# Create a Surv object for the Kaplan-Meier analysis:

km_data <- Surv(data$survival_time, data$disease_specific_death)

# Fit the Kaplan-Meier model:

km_fit <- survfit(km_data ~ 1)

# Plot the Kaplan-Meier survival curve:
  
ggsurvplot(km_fit, data = data, risk.table = TRUE, conf.int = FALSE,
           pval = TRUE, legend.labs = "Survival Probability")

