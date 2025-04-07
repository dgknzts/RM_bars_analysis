# Load necessary libraries
library(tidyverse) 

# --- Find Files for Exp1A, Exp1B, and Exp1C ---

# Define directories for Exp1B and Exp1C
expBC_dirs <- c("datasets/Exp1B/", "datasets/Exp1C/")
# Define directory for Exp1A control files
expA_control_dir <- "datasets/Exp1A/one_bar/"

# List files for Exp1B and Exp1C (main experiment files pattern)
files_BC <- unlist(lapply(expBC_dirs, function(dir) {
  list.files(path = dir, 
             pattern = "^[0-9]+_exp1_.*\\.csv$", 
             full.names = TRUE) 
}))

# List files for Exp1A (control files pattern in the specific subfolder)
files_A <- list.files(path = expA_control_dir,
                      pattern = "^[0-9]+_control_.*\\.csv$",
                      full.names = TRUE)

# Combine all file paths
files <- c(files_A, files_BC)

# --- Load and Process Files ---

# Initialize an empty list to store raw data frames
raw_dfs <- list()

# Load each listed CSV file into the raw_dfs list
for (i in seq_along(files)) {
  file_path <- files[i] # Use the full path
  # Wrap read.csv in tryCatch to handle potential file reading errors
  tryCatch({
    raw_dfs[[i]] <- read.csv(file_path, header = TRUE)
    names(raw_dfs)[i] <- basename(file_path) # Optional: Name list element by file name
  }, error = function(e) {
    # Print error message if a file cannot be read
    message("Error reading file: ", file_path, "\n", e)
  })
}

# Initialize an empty data frame for the combined control task results
one_bar_width <- data.frame()

# Loop through each loaded data frame
for (y in seq_along(raw_dfs)) {
  
  # Get the current data frame
  df <- raw_dfs[[y]]  
  # Get the corresponding file path to determine experiment version
  current_file_path <- files[y] 
  
  # Initialize data frame for processed data for this file
  df_processed_file <- NULL 
  
  # Determine experiment version and apply specific processing
  if (grepl("Exp1A/one_bar/", current_file_path, fixed = TRUE)) {
    # --- Processing for Exp1A Files ---
    # ASSUMPTION: All rows in these files are relevant one-bar trials.
    # ASSUMPTION: Column names are 'participant', 'w', 'response_width_degree'.
    # PLEASE VERIFY AND ADJUST THIS BLOCK BASED ON YOUR ACTUAL Exp1A DATA STRUCTURE
    
    # Check if required columns exist
    required_cols_A <- c("participant", "w", "response_width_degree")
    if (all(required_cols_A %in% colnames(df))) {
      df_processed_file <- df %>%
        # Select relevant columns (adjust names if different in Exp1A)
        select(
          participant,
          w, # Correct width
          response_width_degree # Participant's adjusted width
          # Add starting_width_deg if available and needed
        ) %>%
        # Remove rows with any missing values in the selected columns
        na.omit() %>% 
        # Standardize response width (absolute value)
        mutate(response_width_degree = abs(response_width_degree)) %>%
        # Calculate the deviation between response and correct width
        mutate(
          dev_width_deg = (response_width_degree - w)
        ) %>%
        # Rename columns for clarity and consistency
        rename(correct_width = w,
               width_deviation = dev_width_deg) %>%
        # Add the experiment version column
        mutate(exp_version = "Exp1A") %>%
        # Ensure final columns match the structure of Exp1B/C processing
        # Add NA for columns present in B/C but not A, if any (e.g., starting_width_deg)
        # select(participant, correct_width, width_deviation, exp_version) # Adjust selection if needed
        # For now, assume the rename covers necessary columns
        select(participant, correct_width, width_deviation, exp_version)
      
      
    } else {
      message("Skipping file (Exp1A) due to missing columns: ", basename(current_file_path))
    }
    
  } else if (grepl("Exp1B", current_file_path, fixed = TRUE) || grepl("Exp1C", current_file_path, fixed = TRUE)) {
    # --- Processing for Exp1B / Exp1C Files ---
    current_exp_version <- ifelse(grepl("Exp1B", current_file_path, fixed = TRUE), "Exp1B", "Exp1C")
    
    # Check if 'first.thisN' column exists for filtering control trials
    if (!"first.thisN" %in% colnames(df)) {
      message("Skipping file (Exp1B/C) - 'first.thisN' column missing: ", basename(current_file_path))
      next # Skip to the next file
    }
    
    # Filter rows belonging to the control task and process
    df_filtered <- df %>% 
      filter(!is.na(first.thisN)) 
    
    # If filtering resulted in zero rows, skip this part
    if(nrow(df_filtered) > 0) {
      # Check if required columns exist after filtering
      required_cols_BC <- c("participant", "w", "response_width_degree")
      if (all(required_cols_BC %in% colnames(df_filtered))) {
        df_processed_file <- df_filtered %>%
          # Select relevant columns for the one-bar task
          select(
            participant,
            w, # Correct width
            # starting_width_deg, # Initial probe width (include if needed)
            response_width_degree # Participant's adjusted width
          ) %>%
          # Remove rows with any missing values in the selected columns
          na.omit() %>% 
          # Standardize response width (absolute value)
          mutate(response_width_degree = abs(response_width_degree)) %>%
          # Calculate the deviation between response and correct width
          mutate(
            dev_width_deg = (response_width_degree - w)
          ) %>%
          # Rename columns for clarity and consistency
          rename(correct_width = w,
                 width_deviation = dev_width_deg) %>%
          # Add the experiment version column
          mutate(exp_version = current_exp_version) %>%
          # Ensure final columns match structure
          select(participant, correct_width, width_deviation, exp_version)
      } else {
        message("Skipping control trials in file (Exp1B/C) due to missing columns: ", basename(current_file_path))
      }
    }
    
  } else {
    message("Skipping file - could not determine experiment type: ", basename(current_file_path))
  } # End of if/else block for experiment type
  
  # Append the processed data for this file (if any) to the combined data frame
  if (!is.null(df_processed_file) && nrow(df_processed_file) > 0) {
    one_bar_width <- bind_rows(one_bar_width, df_processed_file)
  }
  
} # End of loop through files

# Remove temporary variables from the environment
rm(df, y, raw_dfs, files, files_A, files_BC, expA_control_dir, expBC_dirs, file_path, i, df_processed_file, df_filtered, current_exp_version, current_file_path, required_cols_A, required_cols_BC) 


# Save the combined control task data (now includes Exp1A, Exp1B, Exp1C)
write.csv(one_bar_width, "datasets/one_bar_Exp1ABC.csv", row.names = FALSE)


# --- Generate Box Plot (Including Exp1A) ---
participant_col_name <- "participant" # Adjust to "subID" if necessary

# Calculate the number of trials for each participant
trial_counts <- one_bar_width %>%
  group_by(.data[[participant_col_name]]) %>% # Use the correct participant column
  summarise(
    number_of_trials = n() # Count the number of rows (trials) for each group
  )


width_boxplot_abc <- ggplot(one_bar_width, 
                            aes(x = width_deviation, 
                                y = as.factor(.data[[participant_col_name]]), # Use correct participant ID column
                                color = as.factor(.data[[participant_col_name]]))) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0, height = 0.1, size = 0.5, alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Width Deviation per Participant by Experiment and Correct Width (Exp1A, 1B, 1C)",
    x = "Width Deviation (degrees)",
    y = "Participant ID",
    color = "Correct Width (deg)" # Legend title
  ) +
  facet_wrap(~ correct_width) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 7), # May need smaller text with more participants
    strip.text.y = element_text(angle = 0), 
    panel.spacing = unit(0.8, "lines"),
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(-0.5,0.5))

# Display the plot
print(width_boxplot_abc)

