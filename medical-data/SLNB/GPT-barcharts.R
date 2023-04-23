library(ggplot2)
library(dplyr)
library(readr)
library(RColorBrewer)

# ############################################################################
# Part One
# ############################################################################
# Vai var lokalizāciju kaut kā atspoguļot apaļā diagrammā 
# head and neck, lower extremity, upper extremity, trunk ventral, 
# trunk dorsal un dala kam nebuja aprakstīts noradīt tur kādi 2 cilvēki

# Read data from CSV file
setwd('/Users/kapsitis/workspace-public/ddgatve-stat/medical-data/SLNB')
data <- read.csv("SLNB_MASTER_COPY3.csv", stringsAsFactors = FALSE)

# Prepare data for the pie chart
location_count <- data %>%
  group_by(lokalizacija) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count),
         label = paste0("", count, ""))

# Set up the color palette
unique_locations <- unique(data$lokalizacija)
colors <- brewer.pal(n = length(unique_locations), name = "Dark2") 
named_colors <- setNames(colors, unique_locations)


# Create the pie chart
pie_chart <- ggplot(location_count, aes(x = "", y = percentage, fill = lokalizacija)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  # scale_fill_manual(values = colors) +
  scale_fill_manual(values = named_colors) +
  theme_void() +
  guides(fill=guide_legend(title="Lokalizācija")) +
  geom_text(aes(x=1.6, label = label), position = position_stack(vjust = 0.5), color = "#000000", size = 4)

# Display the pie chart
png(file="GPT-pie-chart-location.png", width=500, height=700)
pie_chart
dev.off()




# ###################################
# Part Two
# ###################################

# Lokalizācija saistībā ar vecumu 
# Categorize age groups
data <- data %>%
  mutate(age_group = case_when(
    vecums < 40  ~ "39 or less",
    vecums >= 40 & vecums < 50  ~ "40-49",
    vecums >= 50 & vecums < 60  ~ "50-59",
    vecums >= 60 & vecums < 70  ~ "60-69",
    vecums >= 70 & vecums < 80  ~ "70-79",
    vecums >= 80 ~ "80 or more"
  ))

#unique_locations <- unique(stacked_data$lokalizacija)
#colors <- scales::hue_pal()(length(unique_locations))
#named_colors <- setNames(colors, unique_locations)


# Prepare data for the stacked bar chart
stacked_data <- data %>%
  group_by(age_group, lokalizacija) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(lokalizacija = factor(lokalizacija, levels = unique(lokalizacija)))


# Set up the color palette
# colors <- scales::hue_pal()(7)

# Create the stacked bar chart
stacked_bar_chart <- ggplot(stacked_data, aes(x = age_group, y = count, fill = lokalizacija)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = colors) +
  scale_fill_manual(values = named_colors) +
  labs(x = "Age Group", y = "Count", fill = "Location") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the stacked bar chart
png(file="GPT-bar-chart-by-age.png", width=500, height=700)
stacked_bar_chart
dev.off()




# Lokalizāciju sasaistīt ar dzimumu, vai vecumu, vai hospitalizācijas mēnešiem 
# (mums jau ir grafiks ar mēnešiem) un pievienot kurā mēnesī kādas lokalizācijas. 
data$month <- substring(data$Gads,1,2)

stacked_data <- data %>%
  group_by(month, lokalizacija) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(lokalizacija = factor(lokalizacija, levels = unique(lokalizacija)))

# Create the stacked bar chart
stacked_bar_chart2 <- ggplot(stacked_data, aes(x = month, y = count, fill = lokalizacija)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = colors) +
  scale_fill_manual(values = named_colors) +
  labs(x = "Month", y = "Count", fill = "Location") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the stacked bar chart
png(file="GPT-bar-chart-by-month.png", width=500, height=700)
stacked_bar_chart2
dev.off()

