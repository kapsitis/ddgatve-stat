# (1) -demorāfisko eglīti
# (2) -stabiņi cik slnb taisīja un cik netaisīja
# (3) -pa vecumiem slnb veica slnb neveica
# (4) -stadijas(IA,IB, IIA ,IIB, IIC) slnb veica slnb neveica
# (5) -lokalizācijas slnb veica slnb neveica
# (6) -lokalizācijas male/female
# (7) -Kaplana-Meijera ar izčūlojumu un bez izčūlojuma (un Logrank p-value)
# 



library(ggplot2)
library(readr)
library(RColorBrewer)
library(dplyr)


# Read data from CSV file
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)


# Define age intervals
age_intervals <- seq(21, 91, by = 10)
age_intervals[1] <- 18


data$age_group <- cut(data$vecums, breaks = c(age_intervals, Inf), right = FALSE)

palette <- brewer.pal(n = 8, name = "Dark2")
male_color <- palette[1]
female_color <- palette[2]
veica_color <- palette[5]
neveica_color <- palette[3]


barchart3 <- ggplot(data, aes(x = age_group, fill = SLNB)) +
  geom_bar(position = "stack") +
  labs(title = "Age groups stacked by SLNB",
       x = "Age Groups",
       y = "Count of Patients") +
  scale_fill_manual(values = c("veica" = veica_color, "neveica" = neveica_color)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  theme_minimal()


png(file="GPT(3)-bar-chart-age-groups-by-SLNB.png", width=600, height=400)
barchart3
dev.off()


barchart4 <- ggplot(data, aes(x = stadija, fill = SLNB)) +
  geom_bar(position = "stack") +
  labs(title = "Stages stacked by SLNB",
       x = "Stages",
       y = "Count of Patients") +
  scale_fill_manual(values = c("veica" = veica_color, "neveica" = neveica_color)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  theme_minimal()


png(file="GPT(4)-bar-chart-stadija-by-SLNB.png", width=600, height=400)
barchart4
dev.off()


barchart5 <- ggplot(data, aes(x = lokalizacija, fill = SLNB)) +
  geom_bar(position = "stack") +
  labs(title = "Localizations stacked by SLNB",
       x = "Localization",
       y = "Count of Patients") +
  scale_fill_manual(values = c("veica" = veica_color, "neveica" = neveica_color)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  # theme_minimal()
  theme(axis.text.x = element_text(face = "plain", color = "black",
                                   size = 12, angle = 9))


png(file="GPT(5)-bar-chart-lokalizacija-by-SLNB.png", width=600, height=400)
barchart5
dev.off()



# (6) -lokalizācijas male/female
barchart6 <- ggplot(data, aes(x = lokalizacija, fill = dzimums)) +
  geom_bar(position = "stack") +
  labs(title = "Localizations stacked by SLNB",
       x = "Localization",
       y = "Count of Patients") +
  # scale_fill_manual(values = c("veica" = veica_color, "neveica" = neveica_color)) +
  scale_fill_manual(values = c("vir" = male_color, "siev" = female_color)) +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5)) +
  # theme_minimal()
  theme(axis.text.x = element_text(face = "plain", color = "black",
                                   size = 12, angle = 9))


png(file="GPT(6a)-bar-chart-lokalizacija-by-dzimums.png", width=600, height=400)
barchart6
dev.off()





# Set up the color palette
unique_locations <- unique(data$lokalizacija)
colors <- brewer.pal(n = length(unique_locations), name = "Dark2") 
named_colors <- setNames(colors, unique_locations)


# data_grouped <- data %>%
#   group_by(dzimums, lokalizacija) %>%
#   summarise(count = n()) %>%
#   mutate(percentage = count / sum(count))
# 
# # Create pie charts
# pie_chart7 <- ggplot(data_grouped, aes(x = "", y = count, fill = lokalizacija)) +
#   geom_bar(width = 1, stat = "identity") +
#   coord_polar("y", start = 0) +
#   scale_fill_manual(values = named_colors) +
#   theme_void() +
#   facet_wrap(~dzimums) +
#   geom_text(aes(label = count, y = cumsum(count) - 0.5 * count), col = "black") +
#   labs(title = "Disease location for male and female patients",
#        subtitle = "Side-by-side pie charts")
# 
# 
# png(file="GPT(6b)-pie-chart-lokalizacija-by-dzimums.png", width=700, height=400)
# pie_chart7
# dev.off()



# Group by gender and location and count the number of patients
data_grouped <- data %>%
  group_by(dzimums, lokalizacija) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

# Calculate positions for labels
data_grouped <- data_grouped %>%
  arrange(dzimums, desc(lokalizacija)) %>%
  mutate(pos = cumsum(percentage) - 0.5 * percentage)

# Create pie charts
pie_chart7b <- ggplot(data_grouped, aes(x = "", y = percentage, fill = lokalizacija)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0, direction = -1) +
  scale_fill_manual(values = named_colors) +
  theme_void() +
  facet_wrap(~dzimums) +
  geom_text(aes(label = count, y = pos), col = "white") +
  labs(title = "Disease location for male and female patients",
       subtitle = "Side-by-side pie charts")

png(file="GPT(6b)-pie-chart-lokalizacija-by-dzimums.png", width=700, height=400)
pie_chart7b
dev.off()
