# Load required libraries
library(survival)
library(ggplot2)
library(survminer)

# Read data from CSV file
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)

# Convert date columns to Date format
data$Gads <- as.Date(data$Gads, format = "%m/%d/%y")
data$Mirsanasdatums <- as.Date(data$Mirsanasdatums, format = "%m/%d/%Y")

# Create a new column 'time' as the difference in days between hospitalization and death
data$time <- as.numeric(difftime(data$Mirsanasdatums, data$Gads, units = "days"))
data$time <- ifelse(is.na(data$time), 3000, data$time)


# Create a new column 'event' to indicate death event (1 for death, 0 for censored)
data$censor <- ifelse(is.na(data$Mirsanasdatums), 0, 1)
# data$censor <- ifelse(is.na(data$Mirsanasdatums) | (data$Navescelonis != "Ļaundabīga ādas melanoma"), 0, 1)


# Create a survival object
surv_obj <- Surv(time = data$time, event = data$censor)

# Fit the Kaplan-Meier model
km_fit <- survfit(surv_obj ~ 1)

# Create the Kaplan-Meier plot using ggplot2
png(file="GPT-kaplan-meier-single-survival.png", width=500, height=500)
single_survival <- ggsurvplot(km_fit, data = data, risk.table = FALSE, pval = FALSE, conf.int = FALSE,
           xlab = "Time (days)", ylab = "Survival Probability",
           title = "Kaplan-Meier (General) Survival Curve")
single_survival$plot <- single_survival$plot + xlim(0, 3000) + ylim(0.7, 1) 
print(single_survival) 
dev.off()




data$censor <- ifelse(is.na(data$Mirsanasdatums) | (data$Navescelonis != "Ļaundabīga ādas melanoma"), 0, 1)

# (2) Grafiks Kaplan–Meier melanoma-specific survival 
# Compare strata data$SLNB=="veica" and data$SLNB=="neveica"
km_fit <- survfit(Surv(time, event=data$censor) ~ SLNB, data = data)
png(file="GPT-kaplan-meier-specific-survival-by-slnb.png", width=500, height=700)
survival_slnb <- ggsurvplot(km_fit, data = data, risk.table = FALSE, pval = TRUE, conf.int = FALSE,
           xlab = "Time (days)", ylab = "Survival Probability",
           title = "Kaplan-Meier (Specific) Survival by SLNB")
# survival_slnb$plot <- survival_slnb$plot + xlim(0, 3000) + ylim(0.7, 1) 
print(survival_slnb) 
dev.off()


# (3) Grafiks Kaplan–Meier melanoma-specific survival 
# Compare strata data$BreslowIndekss<2 and data$BreslowIndekss>=2 
data$Breslow <- ifelse(data$BreslowIndekss. < 2, "Breslow<2", "Breslow2+")
km_fit <- survfit(Surv(time, event=data$censor) ~ Breslow, data = data)
png(file="GPT-kaplan-meier-specific-survival-by-breslow.png", width=500, height=700)
survival_breslow <- ggsurvplot(km_fit, data = data, risk.table = FALSE, pval = TRUE, conf.int = FALSE,
           xlab = "Time (days)", ylab = "Survival Probability",
           title = "Kaplan-Meier (Specific) Survival Curve")
# survival_breslow$plot <- survival_breslow$plot + xlim(0, 3000) + ylim(0.7, 1) 
print(survival_breslow) 
dev.off()







# GPT-kaplan-meier-overall-survival-by-slnb.png

data$censor <- ifelse(is.na(data$Mirsanasdatums), 0, 1)
# data$censor <- ifelse(is.na(data$Mirsanasdatums) | (data$Navescelonis != "Ļaundabīga ādas melanoma"), 0, 1)


# Create a survival object
surv_obj <- Surv(time = data$time, event = data$censor)

# Fit the Kaplan-Meier model
#km_fit <- survfit(surv_obj ~ SLNB)

km_fit <- survfit(Surv(time, event=data$censor) ~ SLNB, data = data)

# Create the Kaplan-Meier plot using ggplot2
png(file="GPT-kaplan-meier-overall-survival-by-slnb.png", width=500, height=500)
single_survival <- ggsurvplot(km_fit, data = data, risk.table = FALSE, pval = TRUE, conf.int = FALSE,
                              xlab = "Time (days)", ylab = "Survival Probability",
                              title = "Kaplan-Meier (Overall) Survival Curve")
#single_survival$plot <- single_survival$plot + xlim(0, 3000) + ylim(0.5, 1) 
print(single_survival) 
dev.off()




#####################################################################
# Progression free survival
######################################################################


data$fixnotikums = paste0("12/31/", data$notikums)

data$status = rep(1, nrow(data))
for (i in 1:nrow(data)) {
  if (is.na(data$notikums[i])) {
    data$status[i] = 0
    data$fixnotikums[i] = "12/31/2022"
  }
} 
data$fixnotikums <- as.Date(data$fixnotikums, format = "%m/%d/%Y")

#data$time = as.numeric(mdy(data$fixnotikums)) - as.numeric(mdy(data$Gads))
data$time <- as.numeric(difftime(data$fixnotikums, data$Gads, units = "days"))



km_fit <- survfit(Surv(time, status) ~ SLNB, data=data)

# Create the Kaplan-Meier plot using ggplot2
png(file="GPT-kaplan-meier-progressionfree-survival-by-slnb.png", width=500, height=500)
single_survival <- ggsurvplot(km_fit, data = data, risk.table = FALSE, pval = TRUE, conf.int = FALSE,
                              xlab = "Time (days)", ylab = "Survival Probability",
                              title = "Kaplan-Meier Progression-Free Survival")
# single_survival$plot <- single_survival$plot + xlim(0, 3000) + ylim(0.5, 1) 
print(single_survival) 
dev.off()

