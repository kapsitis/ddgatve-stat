# (1) -demorāfisko eglīti
# (2) -stabiņi cik slnb taisīja un cik netaisīja
# (3) -pa vecumiem slnb veica slnb neveica
# (4) -stadijas(IA,IB, IIA ,IIB, IIC) slnb veica slnb neveica
# (5) -lokalizācijas slnb veica slnb neveica
# (6) -lokalizācijas male/female
# ***(7) -Kaplana-Meijera ar izčūlojumu un bez izčūlojuma (un Logrank p-value)



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
# data$censor <- ifelse(is.na(data$Mirsanasdatums), 0, 1)
data$censor <- ifelse(is.na(data$Mirsanasdatums) | (data$Navescelonis != "Ļaundabīga ādas melanoma"), 0, 1)


# Create a survival object
surv_obj <- Surv(time = data$time, event = data$censor)

km_fit <- survfit(Surv(time, event=data$censor) ~ ulceracija, data = data)
png(file="GPT(7)-kaplan-meier-specific-survival-by-ulceracija.png", width=500, height=700)
survival_slnb <- ggsurvplot(km_fit, data = data, risk.table = FALSE, pval = TRUE, conf.int = FALSE,
                            xlab = "Time (days)", ylab = "Survival Probability",
                            title = "Kaplan-Meier (Specific) Survival by Ulceration")
# survival_slnb$plot <- survival_slnb$plot + xlim(0, 3000) + ylim(0.7, 1) 
print(survival_slnb) 
dev.off()
