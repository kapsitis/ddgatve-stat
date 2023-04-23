# ***(1) -demorāfisko eglīti
# (2) -stabiņi cik slnb taisīja un cik netaisīja
# (3) -pa vecumiem slnb veica slnb neveica
# (4) -stadijas(IA,IB, IIA ,IIB, IIC) slnb veica slnb neveica
# (5) -lokalizācijas slnb veica slnb neveica
# (6) -lokalizācijas male/female
# (7) -Kaplana-Meijera ar izčūlojumu un bez izčūlojuma (un Logrank p-value)
# 

library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Read data from CSV file
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)




# Create new data frame with age and gender columns
df <- data.frame(Age = data$vecums, Gender = data$dzimums)

# Create age groups and count the number of patients in each group and gender:
  
# Define age intervals
age_intervals <- seq(21, 91, by = 10)
age_intervals[1] <- 18

# Create age groups
df$AgeGroup <- cut(df$Age, breaks = age_intervals, right = FALSE, include.lowest = TRUE)

# Count patients in each age group and gender
df_summary <- df %>%
  group_by(AgeGroup, Gender) %>%
  summarise(Count = n()) %>%
  mutate(Gender = ifelse(Gender == "siev", "Female", "Male"))


# Set colors for male and female
palette <- brewer.pal(n = 2, name = "Dark2")
male_color <- palette[1]
female_color <- palette[2]

# Create population pyramid
ggsurv_plot <- ggplot(df_summary, aes(x = AgeGroup, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_fill_manual(values = c(Female = female_color, Male = male_color)) +
  labs(title = "Population Pyramid",
       x = "Age Group",
       y = "Number of Patients",
       fill = "Gender") +
  theme_minimal()


png(file="GPT(1a)-population-pyramid.png", width=400, height=500)
ggsurv_plot
dev.off()


# Consider the following `data-file.csv` file storing data about individual patients: 
#   
#   "Perskods","Kods","Perskods","Gads","Mirsanasdatums","5gadudzivildze","Navescelonis","lokalizacija","vecums","dzimums","hosplgums","izmers","limfmezgli","SLNB","celonis","SLNBrezultats","stadija","arstesana","Pigmentacija","AugsRaksturs","SunuTips","BreslowIndekss ","KlarkaLimenis","ulceracija","pilns","invazija ","mikrosateliti","mitozuSkaits ","RL ","regresija","TIL","desmoplazija","Taktika","specifiskaTh","notikums","progresija"," recidivs"
# "*****-*****","4f21","******-*****",04/28/15,,,,"rumpis dorsal",71,"siev",2,"2cmØ",,"neveica",,,"IB","ekscīzija vispārējā anestēzijā","nav aprakstīts","virspusēji izplatošā","nav aprakstīts","1","II","ir",,"nav",,3,"negatīvas",,"vāji ",,"novērošana",,,"nekonstatē","no"
# 
# Can you suggest R code to draw population pyramid to summarize patients' age (data$vecums - integer values) and gender (data$dzimums -- two values "siev" (female) and "vir"(male) ). It should use the following age intevals: 
# 
# ```
# age_intervals <- seq(21, 91, by = 10)
# ```
# 
# Bars associated with male numbers should be shown to the left; bars associated with female counts - to the right. The age intervals should be shown on the central axis of the pyramid. Bars in the pyramid should be displayed as in a histogram - without spaces between the bars. 

data$age_group <- cut(data$vecums, breaks = c(age_intervals, Inf), right = FALSE)

# Load ggplot2 Package
library(ggplot2)

# Create a data frame for age intervals labels
# age_intervals_df <- data.frame(age_group = factor(age_intervals, levels = unique(data$age_group)))
age_intervals_df <- data.frame(age_group = sort(unique(data$age_group)), 
                               y = 10*(1:7), x = rep(21, times=7))

label_data <- data.frame(interval = c("[18,30]", "[31,40]"), x = c(0,0), y = c(0,30))

pop_pyramid <- ggplot(data, aes(x = age_group)) +
  geom_bar(data = subset(data, dzimums == "vir"), aes(fill = dzimums, y = ..count..), width = 1, position = "identity", color = "black") +
  geom_bar(data = subset(data, dzimums == "siev"), aes(fill = dzimums, y = -..count..), width = 1, position = "identity", color = "black") +
  scale_y_continuous(labels = function(x) abs(x), limits = c(-30, 30)) + # Adjust limits according to your data
  coord_flip() +
  geom_text(aes(y = ..count.., label = ..count..), data = subset(data, dzimums == "vir"), stat = "count", position = position_stack(vjust = 0.5), size = 3) +
  geom_text(aes(y = -..count.., label = ..count..), data = subset(data, dzimums == "siev"), stat = "count", position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Population Pyramid",
       x = "Age Group",
       y = "Count") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept = 0.5), linetype = "solid", color = "white", size = 3) +
  geom_text(data = age_intervals_df, aes(x = 0.5, y = 0, label = age_group), 
            nudge_y = 21,  nudge_x = (1:7) - 0.5,
            size = 4, hjust = 0.5, vjust = 0.5, color = "black")


# Display Population Pyramid
png(file="GPT(1b)-population-pyramid.png", width=400, height=500)
pop_pyramid
dev.off()


