# Install and load required packages
#install.packages(c("readr", "tidyverse", "ggplot2", "writexl"))
library(readr)
library(tidyverse)
library(ggplot2)
library(writexl)

# Set Working Directory (Modify as needed)
setwd("C:/Users/samee/OneDrive/Documents/STA100/Extracted_Files")

# Step 1: Process Ladder File
ladder_file <- "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Ladder.csv"
if (file.exists(ladder_file)) {
  ladder_data <- read_csv(ladder_file, skip = 17, col_types = cols(Time = col_double(), Value = col_double())) %>%
    rename(Time = Time, Intensity = Value) %>%
    filter(!is.na(as.numeric(Time)) & !is.na(as.numeric(Intensity))) %>%
    mutate(Time = as.numeric(Time), Intensity = as.numeric(Intensity))
  cat("Ladder file loaded with", nrow(ladder_data), "rows.\n")
} else {
  stop("Ladder file not found.")
}

# Step 2: Process Sample Files
sample_files <- c(
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample1.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample2.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample3.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample4.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample5.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample6.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample7.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample8.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample9.csv",
  "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample10.csv"
)

# Check if all sample files exist
if (!all(file.exists(sample_files))) {
  stop("Some sample files are missing. Please check the file paths.")
}

# Read and process each sample file
sample_data_list <- lapply(sample_files, function(file) {
  sample_name <- gsub("2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample", "Sample", file) %>%
    gsub(".csv", "", .)
  data <- read_csv(file, skip = 17, col_types = cols(Time = col_double(), Value = col_double())) %>%
    rename(Time = Time, Intensity = Value) %>%
    filter(!is.na(as.numeric(Time)) & !is.na(as.numeric(Intensity))) %>%
    mutate(Time = as.numeric(Time), Intensity = as.numeric(Intensity))
  cat(sprintf("Sample %s loaded with %d rows.\n", sample_name, nrow(data)))
  return(data %>% select(Time, Intensity))
})

# Combine sample data using bind_cols()
if (length(sample_data_list) > 0) {
  # Ensure all data frames have the same Time values
  time_check <- lapply(sample_data_list, function(df) df$Time)
  if (!all(sapply(time_check, identical, time_check[[1]]))) {
    stop("Time values across sample files do not match. Please ensure all files have consistent time points.")
  }
  
  combined_sample_data <- bind_cols(
    sample_data_list[[1]] %>% select(Time),
    setNames(lapply(sample_data_list, function(df) df$Intensity),
             paste0("Intensity_", sapply(sample_files, function(f) gsub(".csv", "", basename(f)))))
  )
  cat("Sample data combined with", ncol(combined_sample_data) - 1, "intensity columns.\n")
} else {
  stop("No valid sample data was read. Please check the files.")
}

# Step 3: Combine Ladder and Sample Data
# Ensure Time values match between ladder and samples (using tolerance for floating-point)
if (!all.equal(ladder_data$Time, combined_sample_data$Time, tolerance = 1e-6)) {
  cat("Warning: Time values do not match between ladder and samples (using tolerance for floating-point comparison).\n")
  # Align ladder data to match sample Time values
  ladder_data <- ladder_data[match(combined_sample_data$Time, ladder_data$Time), ]
  cat("Aligned Time values between ladder and samples.\n")
}

# Add ladder intensity to the combined data, preserving all sample columns
combined_data <- combined_sample_data %>%
  mutate(Intensity_Ladder = ladder_data$Intensity) %>%
  select(Time, Intensity_Ladder, starts_with("Intensity_"))
cat("Combined data includes", ncol(combined_data) - 1, "intensity columns (including ladder).\n")

# Step 4: Generate Ladder Plot
ladder_plot <- ggplot(ladder_data, aes(x = Time, y = Intensity)) +
  geom_line(color = "red", linetype = "dashed", linewidth = 1.2) +
  labs(title = "Ladder Trace", x = "Time", y = "Intensity") +
  theme_minimal()
ladder_plot
ggsave("ladder_trace_plot.png", ladder_plot, width = 8, height = 6, dpi = 300)

# Step 5: Generate Overlay Plot (All Intensity Columns with Unique Colors)
long_format_data <- combined_data %>%
  pivot_longer(cols = -Time, names_to = "Sample", values_to = "Intensity") %>%
  mutate(Sample = gsub("Intensity_", "", Sample))

cat("Number of unique samples in long format data:", length(unique(long_format_data$Sample)), "\n")

overlay_plot <- ggplot(long_format_data, aes(x = Time, y = Intensity, color = Sample)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Overlay of All Intensity Traces (Ladder + Samples)", x = "Time", y = "Intensity") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set3")

overlay_plot
ggsave("overlay_plot.png", overlay_plot, width = 12, height = 8, dpi = 300)

# Step 6: Save Combined Data to Excel
#write_xlsx(combined_data, "combined_data.xlsx")
#cat("Processing complete! Plots and data saved.\n")