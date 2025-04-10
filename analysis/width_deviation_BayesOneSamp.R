# 1. Setup - Load Libraries
# ----------------------------------------------------------------------------
library(tidyverse)
library(BayesFactor)

# ----------------------------------------------------------------------------
# 2. USER PARAMETERS
# ----------------------------------------------------------------------------
input_file_path <- "datasets/processed.csv" # Path to processed data
subject_id_var <- "subID"                    # Subject ID column name
experiment_version_var <- "exp_version"      # Experiment version column name
grouping_vars_within_exp <- c("correct_width", "number_deviation") # Columns defining conditions within exp_version
test_var <- "width_deviation"                # Dependent variable to test

# Filtering within each experiment version
filter_needed <- TRUE
filter_var <- "number_deviation"
filter_values <- c("-1", "0") # Values to keep

# Bayesian Test Parameter
mu_value <- 0 # Null hypothesis value

# ----------------------------------------------------------------------------
# 3. Data Loading and Initial Preparation
# ----------------------------------------------------------------------------
data_raw <- tryCatch({
  read.csv(input_file_path)
}, error = function(e) {
  stop("Error loading file: '", input_file_path, "'. ", e$message)
})

# Basic Type Conversion (assuming required columns exist)
data_prepared_full <- data_raw %>%
  mutate(
    across(all_of(c(subject_id_var, experiment_version_var, grouping_vars_within_exp)), as.factor),
    across(all_of(test_var), as.numeric)
  )

# Identify unique experiment versions
exp_versions_to_run <- levels(data_prepared_full[[experiment_version_var]])
if (is.null(exp_versions_to_run)) {
  exp_versions_to_run <- unique(data_prepared_full[[experiment_version_var]])
}
if(length(exp_versions_to_run) == 0) {
  stop("Error: No unique values found in column: ", experiment_version_var)
}

# List to store results
all_results <- list()

# ----------------------------------------------------------------------------
# 4. Loop Through Each Experiment Version and Perform Analysis
# ----------------------------------------------------------------------------
for (current_exp_version in exp_versions_to_run) {
  
  # --- 4a. Subset Data ---
  data_subset <- data_prepared_full %>%
    filter(!!rlang::sym(experiment_version_var) == current_exp_version)
  
  if(nrow(data_subset) == 0) {
    next # Skip if no data for this version
  }
  
  # --- 4b. Filtering Step (within subset) ---
  data_subset_filtered <- data_subset
  if (filter_needed) {
    data_subset_filtered <- data_subset_filtered %>%
      filter(!!rlang::sym(filter_var) %in% filter_values) %>%
      mutate(across(all_of(filter_var), factor)) # Keep as factor
    
    if(nrow(data_subset_filtered) == 0) {
      next # Skip if no data after filtering
    }
  }
  
  # --- 4c. Subject-Level Aggregation ---
  subject_mean_col <- paste0("mean_", test_var)
  
  data_grouped_subset <- data_subset_filtered %>%
    group_by(across(all_of(c(subject_id_var, grouping_vars_within_exp)))) %>%
    summarise(
      mean_value = mean(!!rlang::sym(test_var), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(!!subject_mean_col := mean_value)
  
  if(nrow(data_grouped_subset) == 0) {
    next # Skip if no data after aggregation
  }
  
  # --- 4d. Bayesian One-Sample T-Tests ---
  bayes_results_subset <- tryCatch({
    data_grouped_subset %>%
      group_by(across(all_of(grouping_vars_within_exp))) %>%
      filter(n() > 1) %>% # Ensure group has enough data for test
      nest() %>%
      mutate(
        bf_test = map(data, ~ ttestBF(x = .[[subject_mean_col]], mu = mu_value))
      ) %>%
      ungroup()
  }, error = function(e){
    warning("Bayesian T-test calculation failed for ", current_exp_version, ": ", e$message)
    NULL # Return NULL if analysis fails
  })
  
  # Check if analysis failed or yielded no results
  if(is.null(bayes_results_subset) || nrow(bayes_results_subset) == 0) {
    next
  }
  
  # --- 4e. Extract and Format Results ---
  results_subset <- bayes_results_subset %>%
    mutate(
      # Use tryCatch here as well in case bf object is malformed
      bf10 = map_dbl(bf_test, ~ tryCatch(as.data.frame(.)$bf, error = function(e) NA_real_)),
      bf01 = 1 / bf10
    ) %>%
    mutate(!!experiment_version_var := current_exp_version) %>%
    select(all_of(experiment_version_var), all_of(grouping_vars_within_exp), bf10, bf01)
  
  # Store results
  all_results[[current_exp_version]] <- results_subset
  
} # End of loop

# ----------------------------------------------------------------------------
# 5. Combine and Display Final Results
# ----------------------------------------------------------------------------
cat("\n--- Combined Final Results ---\n")
if (length(all_results) > 0) {
  final_summary_table <- bind_rows(all_results)
  print(final_summary_table, n = Inf) # Print all rows
} else {
  cat("No results were successfully generated.\n")
}

cat("\n--- Interpretation Guide ---\n")
cat(" BF10: Evidence H1 (mean != ", mu_value, ") / H0 (mean == ", mu_value, ")\n", sep="")
cat(" BF01: Evidence H0 (mean == ", mu_value, ") / H1 (mean != ", mu_value, ")\n", sep="")
cat("   >10: Strong Evidence; 3-10: Moderate Evidence; 1-3: Anecdotal Evidence\n")
cat("   (Applies to the hypothesis in the numerator)\n")
cat("--- Analysis Complete ---\n")
