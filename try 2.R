library(tidyverse)
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(ggplot2)    # For plotting
library(writexl)    # For writing to Excel
library(tidyr)  


# Step 1: Unzip and Set Directory
setwd("C:/Users/samee/OneDrive/Documents/STA100/Extracted_Files")

# Step 2: Process Ladder File
ladder_file <- "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Ladder.csv"
ladder_data <- read_csv(ladder_file, skip = 17)  # Skip first 17 rows
colnames(ladder_data) <- c("Time", "Intensity")# Rename columns
#sample_data <- read_csv(sample_file, skip = 1061)
ladder_data <- ladder_data %>% na.omit()
ladder_data <- ladder_data %>% mutate(Sample = "Ladder")

#Convert 'Time' and 'Intensity' to numeric
ladder_data <- ladder_data %>%
  mutate(
    Time = as.numeric(Time),
    Intensity = as.numeric(Intensity)
  )

sample_data <- sapply(sample_data, function(x) as.numeric(as.character(x)))


# Step 3: Process Sample Files
sample_files <- list.files(pattern = ".csv")
#sample_files <- list.files(pattern = "^Sample\\d+\\.csv$") #what does $ do?
sample_data_list <- lapply(sample_files, function(file) {
  data <- read_csv(file, skip = 17)# Skip first 17 rows
  colnames(data) <- c("Time", "Intensity")  # Rename columns
  data <- data %>% na.omit() # Remove NA rows
  return(data)
})


if (length(sample_data_list) > 0) {
  sample_data <- Reduce(function(x, y) bind_cols(x, y[, 2, drop = FALSE]), sample_data_list)
  colnames(sample_data) <- paste0("Intensity_Sample", seq_along(sample_files))
} else {
  sample_data <- data.frame()
  cat("No sample files found")
}

#remove last row from data frame
sample_data <- sample_data[-nrow(sample_data),]

# Step 4: Generate Plots
# Plot ladder trace 
#change x and y values
ladder_plot <- ggplot(ladder_data, aes(x = Time, y = Intensity, group = Sample)) +
  geom_line() +
  ggtitle("Ladder Trace") +
  xlab("Time") +
  ylab("Intensity") +
  #scale_x_continuous(0.0, 1500.0) +
  #scale_y_continuous(0.0, 50.0) +
  theme_minimal() +
  annotate("text", x = 75, y = -5, label = "Time", size = 5, angle = 0) +
  annotate("text", x = 0, y = 25, label = "Intensity", size = 5, angle = 90)
ladder_plot
ggsave("ladder_trace_plot.png", ladder_plot, width = 8, height = 6)


# Overlay sample traces
#no ladder data in the overlay and use excel
overlay_plot <- ggplot(ladder_data, aes(x = Time, y = Intensity)) +
  geom_line(color = "red", linetype = "dashed", linewidth = 1.2) +
  ggtitle("Overlay of Sample Traces with Ladder") +
  xlab("Time") +
  ylab("Intensity") +
  theme_minimal()
overlay_plot

#remove all time columns
#make all columns numerical
if (ncol(sample_data) > 1) {
  for (i in seq(1, ncol(sample_data), by = 2)) {
    overlay_plot <- overlay_plot +
      geom_line(aes_string(x = colnames(sample_data)[1], y = colnames(sample_data)[2 * i]),
                data = sample_data, linewidth = 0.8)
  }
}
overlay_plot

ggsave("overlay_plot.png", overlay_plot, width = 8, height = 6)

if (ncol(sample_data) > 0) {
  combined_data <- bind_cols(ladder_data, sample_data)
} else {
  combined_data <- ladder_data
  cat("No sample data available to combine.\n")
}

# Step 5: Combine Ladder and Sample Data
combined_data <- bind_cols(ladder_data, sample_data)

# Step 6: Save Combined Data to Excel
write_xlsx(combined_data, "combined_data.xlsx")

# Step 7: Success Message
cat("Processing complete! Plots saved as 'ladder_trace_plot.png' and 'overlay_plot.png'.\n")
cat("Combined data saved as 'combined_data.xlsx'.\n")

