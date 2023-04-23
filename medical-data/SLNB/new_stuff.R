library(survival)
library(ggplot2)
library(survminer)

# Read data from CSV file
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)

# Convert the date columns to Date objects and calculate the survival time
data$Gads <- as.Date(data$Gads, format = "%m/%d/%y")
data$Mirsanasdatums <- as.Date(data$Mirsanasdatums, format = "%m/%d/%Y")
data$survival_time <- as.numeric(difftime(data$Mirsanasdatums, data$Gads, units = "days"))
data$survival_time <- ifelse(is.na(data$survival_time), 3000, data$survival_time)


# Create a censor variable (1 for event, 0 for censor)
data$censor <- ifelse(is.na(data$Mirsanasdatums) | (data$Navescelonis != "Ļaundabīga ādas melanoma"), 0, 1)

# Create a Surv object for the Kaplan-Meier analysis
km_data <- Surv(data$survival_time, data$censor)

# Fit the Kaplan-Meier model
km_fit <- survfit(km_data ~ 1)

# Plot the Kaplan-Meier survival curve
ggsurv_plot <- ggsurvplot(km_fit, data = data, risk.table = FALSE, conf.int = FALSE, 
           pval = TRUE, legend.labs = "Survival Probability")

ggsurv_plot$plot <- ggsurv_plot$plot + xlim(0, 3000) + ylim(0.7, 1) 
print(ggsurv_plot)

