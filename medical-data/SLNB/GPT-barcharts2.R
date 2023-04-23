# (1) -demorāfisko eglīti
# ***(2) -stabiņi cik slnb taisīja un cik netaisīja
# (3) -pa vecumiem slnb veica slnb neveica
# (4) -stadijas(IA,IB, IIA ,IIB, IIC) slnb veica slnb neveica
# (5) -lokalizācijas slnb veica slnb neveica
# (6) -lokalizācijas male/female
# (7) -Kaplana-Meijera ar izčūlojumu un bez izčūlojuma (un Logrank p-value)


library(ggplot2)
library(readr)
library(RColorBrewer)


# Set colors for male and female
palette <- brewer.pal(n = 8, name = "Dark2")
male_color <- palette[1]
female_color <- palette[2]



setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)

# Create stacked barchart
barchart2b <- ggplot(data, aes(x = SLNB, fill = dzimums)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Barchart of SLNB by Gender",
       x = "SLNB",
       y = "Count of Patients",
       fill = "Gender") +
  scale_fill_manual(values = c("vir" = male_color, "siev" = female_color)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  theme_minimal()

png(file="GPT(2a)-bar-chart-by-SLNB-gender.png", width=400, height=400)
barchart2b
dev.off()




barchart2a <- ggplot(data, aes(x = SLNB)) +
  geom_bar(position = "stack", fill="lightblue") +
  labs(title = "Stacked Barchart of SLNB by Gender",
       x = "SLNB",
       y = "Count of Patients") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  theme_minimal()


png(file="GPT(2b)-bar-chart-by-SLNB.png", width=400, height=400)
barchart2a
dev.off()
