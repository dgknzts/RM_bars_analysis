library(tidyverse)


# Initialize the list of directory paths with corresponding experiment versions
project_info <- list(
  list(dir = "datasets/Exp1A/", exp_version = "Exp1A"),
  list(dir = "datasets/Exp1B/", exp_version = "Exp1B"),
  list(dir = "datasets/Exp1C/", exp_version = "Exp1C")
)

# Initialize an empty data frame to store combined results from all projects
final_data <- data.frame()

# Loop through each directory and experiment information
for (project in project_info) {
  # Extract directory and experiment version
  project_dir <- project$dir
  exp_version <- project$exp_version
  
  
  # Initialize an empty list to store data frames
  raw_dfs <- list()
  
  # List all files in the directory with the control pattern
  files <- list.files(path = project_dir, pattern = "^[0-9]+_exp1_.*\\.csv$")
  
  # Load data files
  for (i in seq_along(files)) {
    file_name <- files[i]
    full_file_path <- file.path(project_dir, file_name)
    # Read the CSV file and store it in the list
    raw_dfs[[i]] <- read.csv(full_file_path, header = TRUE)
    # Assign names to each data frame in the list for easier access
    names(raw_dfs)[i] <- file_name
  }
  
  # Initialize a temporary data frame to store combined results for this directory
  data <- data.frame()
  
  # Loop through each data frame in raw_dfs
  for (y in seq_along(raw_dfs)) {
    # Extract the data frame
    df <- raw_dfs[[y]]
    
    # Finding the first row after practice trials
    start_point <- as.numeric(which(!is.na(df$key_resp_4.rt)))
    # Finding the last trial of the main experiment
    end_point <- ifelse(nrow(df) == 129, 
                        as.numeric(which(!is.na(df$text.started))) - 1, 
                        as.numeric(nrow(df)))
    
    df <- df[(start_point + 1):(end_point), ]
    
    df <- df %>%
      # Selecting relevant columns
      select(
        participant,
        stim_length,
        amount,
        center_to_center,
        w,
        trial_type,
        loop.thisN,
        response,
        key_resp.rt,
        starting_width_deg,
        starting_space_deg,
        response_width_degree,
        response_spacing_degree,
        next_5.rt,
        resp_group
      ) %>%
      # Remove rows that contains between block rows
      na.omit() %>%
      # Taking the absolute value of the response width
      mutate(response_width_degree = abs(response_width_degree)) %>%
      # Getting rid of the "num_" prefix in the response column
      mutate(response = as.numeric(gsub("num_", "", response))) %>%
      # Handle response_spacing_degree
      mutate(
        response_spacing_degree = case_when(
          response_spacing_degree == "9999" ~ 0,
          as.numeric(response_spacing_degree) >= 0 ~ as.numeric(response_spacing_degree)
        ),
        starting_space_deg = case_when(
          starting_space_deg > 3000 ~ 0,
          starting_space_deg < 3000 ~ starting_space_deg
        )
      ) %>%
      # Add the experiment version information
      mutate(exp_version = exp_version)
    
    data <- bind_rows(data, df)
  }
  
  # Combine results from this directory into final_data
  final_data <- bind_rows(final_data, data)

}


# Re-arrange col order
col_order <- c("participant", "resp_group", "trial_type", "loop.thisN", "amount", "response" , "key_resp.rt", "center_to_center", "response_spacing_degree", "starting_space_deg", "w", "response_width_degree", "starting_width_deg", "next_5.rt", "stim_length", "exp_version")
final_data <- final_data[, col_order]

#adding plus one to get rid of zeros
final_data$loop.thisN <- final_data$loop.thisN + 1 

#renaming columns
colnames(final_data) <- c("subID", "keyboard_condition", "trial_type", "trial_number", "correct_num", "response_num", "response_rt", "correct_space", "response_space", "probe_space", "correct_width", "response_width", "probe_width", "adjustment_duration", "stim_length", "exp_version")

final_data$subID <- as.factor(final_data$subID)
df <- final_data


# Computing variables
df <- df %>%
  mutate(
    number_deviation = response_num - correct_num,
    width_deviation = response_width - correct_width,
    spacing_deviation = response_space - correct_space,
    
    response_stim_length = (response_space * (response_num- 1)) + response_width,
    compression_rate = response_stim_length / stim_length,
    
    actual_pooled_width = correct_width * correct_num,
    response_pooled_width = response_width * response_num,
    pooled_width_deviation = response_pooled_width - actual_pooled_width,
    
    actual_width_density = actual_pooled_width / stim_length,
    response_width_density = response_pooled_width / response_stim_length,
    width_density_deviation = response_width_density - actual_width_density,
    
    actual_edge_to_edge_spacing = correct_space - correct_width,
    response_edge_to_edge_spacing = response_space - response_width,
    edge_to_edge_spacing_deviation = response_edge_to_edge_spacing - actual_edge_to_edge_spacing
  )

# Counting filtered trials
source("analysis/counting_exclusions.R")


# Filtering noise
df <-  df %>%
  filter(adjustment_duration > 1) %>% #adjustment time is too quick
  filter(adjustment_duration < 15) %>% #NOT SURE! 15 sec is too long
  filter(response_rt < 10) %>% #Yildirim & Sayim. Low accuracy and high confidence in redundancy masking
  filter(number_deviation < 4) %>% filter(number_deviation > -4) %>% #same
  filter(response_edge_to_edge_spacing > 0 | response_num == 1)
  

# add spacing condition
df <- df %>%
  mutate(spacing = if_else(exp_version == "Exp1A", 
                           case_when(
                             correct_space <= 0.6 ~ "small",
                             correct_space > 0.6 & correct_space <= 0.8 ~ "middle",
                             correct_space > 0.8 ~ "large",
                             TRUE ~ NA_character_
                           ), 
                           if_else(
                             exp_version == "Exp1B",
                             case_when(
                               correct_space <= 0.8 ~ "small",
                               correct_space > 0.8 & correct_space <= 1 ~ "middle",
                               correct_space > 1 ~ "large",
                               TRUE ~ NA_character_
                             ),
                             case_when(
                               correct_space == 0.5 ~ "small",
                               correct_space == 0.9 ~ "large",
                               TRUE ~ NA_character_
                             )
                             )
                           ))


# Save the combined data
write.csv(df, "datasets/processed.csv", row.names = FALSE)
