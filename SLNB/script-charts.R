library(purrr)
library(RColorBrewer)

library(survival)
library(survminer)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stats)


# Change this directory to the directory containing SLNB-MASTER-COPY.csv
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/SLNB')

df <- read.table(file='SLNB-MASTER-COPY.csv', header=TRUE, sep=",") 
df$month <- substring(df$gads,1,2)

# Create and save a barplot (joslu diagramma), 
png(file="hospitalization-by-month.png",
    width=500, height=500)
colors <- brewer.pal(4, "Set2")
barplot(table(df$month), col=colors[1])
dev.off()


# Kaplan-Meier diagrams
# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/


# "veica", "neveica" in "SLNB" column sometimes followed by spaces - clean up.
df$SLNB2 = str_trim(df$SLNB)

df$fixnotikums = paste0("12/31/", df$notikums)

df$status = rep(1, nrow(df))
for (i in 1:nrow(df)) {
  if (is.na(df$notikums[i])) {
    df$status[i] = 0
    df$fixnotikums[i] = "12/31/2022"
  }
} 

df$time = as.numeric(mdy(df$fixnotikums)) - as.numeric(mdy(df$gads))

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

km <- with(df, Surv(time, status))
km
km_fit <- survfit(Surv(time, status) ~ SLNB2, data=df)
summary(km_fit, times = c(1,30,60,90*(1:10)))

png(file="kaplan-meier.png",
    width=500, height=500)
autoplot(km_fit)
dev.off()


# our actual patients per month (January to December)
patientsByMonth <- as.vector(table(df$month))
# distribution, if the distribution is uniform
uniformByMonth <- rep(133/12, rep=12)
chiSquareExpr <- sum((patientsByMonth - uniformByMonth)^2/uniformByMonth)
# Find the probability for 12-1 = 11 degrees of freedom
dchisq(chiSquareExpr, df=11)
