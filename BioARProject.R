install.packages("readr")


library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(ggplot2)    # For plotting
library(writexl)    # For writing to Excel

unzip("C:\\Users\\samee\\OneDrive\\Documents\\STA100\\Archive (1).zip", exdir = "C:\\Users\\samee\\OneDrive\\Documents\\STA100\\Extracted_Files")
setwd("C:\\Users\\samee\\OneDrive\\Documents\\STA100\\Extracted_Files")

list.files()

# Step 1: Read the ladder file
ladder_file <- "2100 expert_Eukaryote Total RNA Pico_DE13805791_2015-08-26_14-49-17_Ladder.csv"
ladder_data <- read.csv(ladder_file) # read.csv is not limited
head(ladder_data)
ladder_data <- ladder_data[-c(1:17),] #selection feature, portion before rows and after column
head(ladder_data)

# Step 2: Read all sample files
sample_files <- list.files(pattern = "Sample\\d+\\.csv")
sample_data_list <- lapply(sample_files, read.csv) 
sample_data_list <- lapply(sample_data_list, function(df) {
  df[-(1:17), ]  # Exclude rows 1 through 17
})
head(sample_data_list)

# Step 3: Combine sample data into a single data frame
# Add a column indicating the sample name for easier identification later
# 10 columns instead of rows combined 
# rbind -> rows; cbind -> columns
#remove 17 rows for sample data 
#sample_data_list <- sample_data_list
#instead of lapply use for loop to help with python knowledge
sample_data <- bind_cols(
  lapply(seq_along(sample_data_list), function(i) {
    sample_data_list[[i]] %>%
      mutate(Sample = gsub(".csv", "", sample_files[i]))
  })
)
head(sample_data) #if hard to visualize can write as csv file
write.csv(sample_data, "save.csv")
#fix problem of everu third column that showed up, join code link


# Step 4: Plot overlay figures
# Ensure your data has columns like "Time" and "Intensity" (adjust column names as necessary)
# Plot ladder trace
ladder_plot <- ggplot(ladder_data, aes(x = Time, y = Intensity)) +
  geom_line(color = "red", linewidth = 1.2) +
  ggtitle("Ladder Trace") +
  xlab("Time") +
  ylab("Intensity") +
  theme_minimal()

# Overlay sample traces
overlay_plot <- ggplot(sample_data, aes(x = Time, y = Intensity, color = Sample)) +
  geom_line(linewidth = 0.8) +
  geom_line(data = ladder_data, aes(x = Time, y = Intensity), color = "red", linetype = "dashed", linewidth = 1.2) +
  ggtitle("Overlay of Sample Traces with Ladder") +
  xlab("Time") +
  ylab("Intensity") +
  theme_minimal() +
  labs(color = "Sample")

# Save the plots as images
ggsave("ladder_trace_plot.png", ladder_plot, width = 8, height = 6)
ggsave("overlay_plot.png", overlay_plot, width = 8, height = 6)

# Step 5: Combine data for Excel
# Assuming ladder data and sample data have similar structures
ladder_data <- ladder_data %>% mutate(Sample = "Ladder")
combined_data <- bind_rows(ladder_data, sample_data)

# Save the combined data to an Excel file
write_xlsx(combined_data, "combined_data.xlsx")



# Step 6: Print a success message
cat("Processing complete! Plots saved as 'ladder_trace_plot.png' and 'overlay_plot.png'.\n")
cat("Combined data saved as 'combined_data.xlsx'.\n")

