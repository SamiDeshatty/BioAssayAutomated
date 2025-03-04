# Install and load required packages
#install.packages(c("readr", "tidyverse", "ggplot2", "writexl", "pracma"), dependencies = TRUE)
library(readr)
library(tidyverse)
library(ggplot2)
library(writexl)
library(pracma)  # For peak detection (findpeaks function)

# Set Working Directory (Modify as needed)
setwd("C:/Users/samee/OneDrive/Documents/BioAssayAutomated")

# Helper function to find the first peak (local maximum) in a vector
find_first_peak <- function(intensity, time) {
  if (length(intensity) < 3) return(NULL)  # Need at least 3 points for a peak
  
  # Try using pracma::findpeaks
  peak_result <- tryCatch({
    peaks <- findpeaks(intensity, nups = 1, ndowns = 1, minpeakdistance = 5)
    if (is.null(peaks) || nrow(peaks) == 0) NULL else peaks[1, 1]  # Return index of first peak
  }, error = function(e) {
    cat("pracma::findpeaks failed, using fallback method.\n")
    NULL
  })
  
  if (!is.null(peak_result)) {
    peak_idx <- peak_result
    return(c(time[peak_idx], intensity[peak_idx]))
  }
  
  # Fallback: Look for the first significant rise (base R method)
  diffs <- diff(intensity)
  rise_idx <- which.max(diffs) + 1  # Find where intensity starts rising significantly
  if (length(rise_idx) > 0 && rise_idx <= length(time)) {
    return(c(time[rise_idx], intensity[rise_idx]))
  }
  return(NULL)  # No peak or significant rise found
}

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

# Find the first peak of the ladder
ladder_peak <- find_first_peak(ladder_data$Intensity, ladder_data$Time)
if (is.null(ladder_peak)) stop("Could not find a peak in the ladder data.")
cat("Ladder first peak at Time:", ladder_peak[1], "with Intensity:", ladder_peak[2], "\n")

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

# Read and process each sample file, aligning peaks
sample_data_list <- lapply(sample_files, function(file) {
  sample_name <- gsub("2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample", "Sample", file) %>%
    gsub(".csv", "", .)
  data <- read_csv(file, skip = 17, col_types = cols(Time = col_double(), Value = col_double())) %>%
    rename(Time = Time, Intensity = Value) %>%
    filter(!is.na(as.numeric(Time)) & !is.na(as.numeric(Intensity))) %>%
    mutate(Time = as.numeric(Time), Intensity = as.numeric(Intensity))
  cat(sprintf("Sample %s loaded with %d rows.\n", sample_name, nrow(data)))
  
  # Find the first peak of this sample
  sample_peak <- find_first_peak(data$Intensity, data$Time)
  if (is.null(sample_peak)) {
    cat(sprintf("Warning: Could not find a peak in Sample %s, skipping alignment.\n", sample_name))
    return(data %>% select(Time, Intensity))
  }
  cat(sprintf("Sample %s first peak at Time: %f with Intensity: %f\n", sample_name, sample_peak[1], sample_peak[2]))
  
  # Calculate the time shift needed to align sample peak with ladder peak
  time_shift <- abs(ladder_peak[1] - sample_peak[1])
  
  # Shift the Time values for this sample
  data <- data %>%
    mutate(Time = Time + time_shift)
  
  return(data %>% select(Time, Intensity))
})

data$Time = 

# Combine sample data using bind_cols(), allowing for tolerance in Time values
if (length(sample_data_list) > 0) {
  # Ensure all data frames have similar Time values (using tolerance for floating-point comparison)
  time_check <- lapply(sample_data_list, function(df) df$Time)
  # Use isTRUE() to handle all.equal() output and ensure logical result
  if (!all(sapply(time_check[-1], function(x) isTRUE(all.equal(x, time_check[[1]], tolerance = 1e-6))))) {
    cat("Warning: Time values across sample files differ slightly after alignment. Attempting to standardize...\n")
    # Standardize Time values to the first sample's Time sequence
    reference_time <- time_check[[1]]
    standardized_samples <- lapply(sample_data_list, function(df) {
      approx(df$Time, df$Intensity, xout = reference_time)$y %>%
        data.frame(Time = reference_time, Intensity = .) %>%
        select(Time, Intensity)
    })
    sample_data_list <- standardized_samples
    cat("Standardized Time values across samples.\n")
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
# Extract raw numeric vectors for comparison, ensuring they are numeric
ladder_time <- as.numeric(unlist(ladder_data$Time))
sample_time <- as.numeric(unlist(combined_sample_data$Time))
if (!isTRUE(all.equal(ladder_time, sample_time, tolerance = 1e-6))) {
  cat("Warning: Time values do not match between ladder and samples (using tolerance for floating-point comparison).\n")
  # Align ladder data to match sample Time values using interpolation
  ladder_data <- data.frame(
    Time = sample_time,
    Intensity = approx(ladder_time, ladder_data$Intensity, xout = sample_time)$y
  )
  cat("Aligned Time values between ladder and samples using interpolation.\n")
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

# Step 5: Generate Overlay Plot (All Intensity Columns with Unique Colors, No Legend)
# Reshape data to long format for plotting all intensity columns, simplifying sample names
long_format_data <- combined_data %>%
  pivot_longer(cols = -Time, names_to = "Sample", values_to = "Intensity") %>%
  mutate(Sample = case_when(
    Sample == "Intensity_Ladder" ~ "Ladder",
    TRUE ~ gsub("Intensity_2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Sample", "Sample", Sample)
  ))

cat("Number of unique samples in long format data:", length(unique(long_format_data$Sample)), "\n")

overlay_plot <- ggplot(long_format_data, aes(x = Time, y = Intensity, color = Sample)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Overlay of All Intensity Traces (Ladder + Samples, Peaks Aligned)", x = "Time", y = "Intensity") +
  theme_minimal() +
  theme(legend.position = "right")  # Remove the legend entirely
overlay_plot
ggsave("overlay_plot.png", overlay_plot, width = 12, height = 8, dpi = 300)

# Step 6: Save Combined Data to Excel
write_xlsx(combined_data, "combined_data.xlsx")
cat("Processing complete! Plots and data saved.\n")
