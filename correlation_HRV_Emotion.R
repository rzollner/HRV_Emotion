rm(list=ls())
setwd("D:/HEMS/ROSINA")
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
# load data
HRV_Emotion_data <- read_excel("HRV_Emotion_data.xlsx")
# remove all rows that have NA values in any column
data = na.omit(HRV_Emotion_data) 


# Specify the columns representing emotions and HRV measures
emotion_columns <- c(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34)
hrv_columns <- c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
timepoints <- unique(data$Time_rel)

# Initialize an empty list to store correlation results for each timepoint
correlation_list <- list()

# Calculate correlations for each timepoint
for (time in timepoints) {
  # Filter data for the current timepoint
  data_time <- data %>% filter(Time_rel == time)
  
  # Initialize a matrix to store correlation results for this timepoint
  correlation_matrix <- matrix(nrow = length(emotion_columns), ncol = length(hrv_columns))
  colnames(correlation_matrix) <- c("Hr [1/min]", "HrvHf [ms^2]", "HrvLf [ms^2]", "HrvLfHf []", "HrvPnn50 [%]", "HrvRmssd [ms]", "HrvSd1 [ms]", "HrvSd2 [ms]", "HrvSd2Sd1 [ms]", "HrvSdnn [ms]", "HrvSdsd [ms]")
  rownames(correlation_matrix) <-c("pa_calm1", "pa_calm2", "pa_lively1", "pa_lively2", "na_nervous1", "na_nervous2", "na_weakend1", "na_weakened2", "na_stress1", "na_stress2", "pa_interested1", "pa_interested2", "pa_happy1", "pa_happy2", "na_sad1
", "na_sad2")
  
  # Calculate correlations and fill the matrix
  for (i in 1:length(emotion_columns)) {
    for (j in 1:length(hrv_columns)) {
      correlation_matrix[i, j] <- cor(data_time[[emotion_columns[i]]], data_time[[hrv_columns[j]]])
    }
  }
  
  # Convert matrix to data frame
  correlation_df <- as.data.frame(correlation_matrix)
  
  # Add row names as a column
  correlation_df <- correlation_df %>%
    rownames_to_column(var = "Emotion")
  
  # Reshape to long format
  correlation_long_df <- correlation_df %>%
    pivot_longer(cols = starts_with("HR"), names_to = "HRV_Measure", values_to = "Correlation") %>%
    mutate(Time_rel = time)
  
  # Append to the list
  correlation_list[[time+1]] <- correlation_long_df
}

# Combine all data frames into one
final_correlation_df <- bind_rows(correlation_list)

#change order of the columns
final_correlation_df<- final_correlation_df[,c(1,2,4,3)]

# Save the final_correlation_df to an Excel file
write_xlsx(final_correlation_df, "correlation_table_long_format.xlsx")


# PLOTTING


  ggplot(final_correlation_df, aes(x = HRV_Measure, y = Emotion, fill = Correlation)) +
    geom_tile() +
    facet_wrap(~ Time_rel) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    labs(title = "Correlation Between Emotions and HRV Measures Across Timepoints",
        x = "HRV Measure",
        y = "Emotion")
  ggsave("correlation_heatmap.png", width = 10, height = 8)
