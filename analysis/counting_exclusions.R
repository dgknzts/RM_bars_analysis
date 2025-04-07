df_unfiltered <- df

# Save the initial row count for each experimental version
initial_count <- df_unfiltered %>%
  group_by(exp_version) %>%
  summarize(initial_row_count = n(), .groups = "drop")

# Filtering steps and tracking remaining rows at each step
filter_steps <- df_unfiltered %>%
  group_by(exp_version) %>%
  mutate(initial_row_count = n()) %>%
  summarize(step = "start", step_no = 1, remaining_rows = n(), .groups = "drop") %>%
  bind_rows(
    df_unfiltered %>%
      filter(response_rt < 10) %>%
      group_by(exp_version) %>%
      summarize(step = "response_rt < 10", step_no = 2, remaining_rows = n(), .groups = "drop")
  ) %>%
  bind_rows(
    df_unfiltered %>%
      filter(response_rt < 10, number_deviation < 4, number_deviation > -4) %>%
      group_by(exp_version) %>%
      summarize(step = "number_deviation < 4 & > -4", step_no = 3, remaining_rows = n(), .groups = "drop")
  ) %>%
  bind_rows(
    df_unfiltered %>%
      filter(response_rt < 10, number_deviation < 4, number_deviation > -4,
             adjustment_duration > 1) %>%
      group_by(exp_version) %>%
      summarize(step = "adjustment_duration > 1", step_no = 4, remaining_rows = n(), .groups = "drop")
  ) %>%
  bind_rows(
    df_unfiltered %>%
      filter(response_rt < 10, number_deviation < 4, number_deviation > -4,
             adjustment_duration > 1, adjustment_duration < 15) %>%
      group_by(exp_version) %>%
      summarize(step = "adjustment_duration < 15", step_no = 5, remaining_rows = n(), .groups = "drop")
  ) %>%
  bind_rows(
    df_unfiltered %>%
      filter(response_rt < 10, number_deviation < 4, number_deviation > -4,
             adjustment_duration > 1, adjustment_duration < 15,
             response_edge_to_edge_spacing > 0 | response_num == 1) %>%
      group_by(exp_version) %>%
      summarize(step = "response_edge_to_edge_spacing > 0", step_no = 6, remaining_rows = n(), .groups = "drop")
  )

# Calculate the number of trials eliminated at each step
elimination_summary <- filter_steps %>%
  group_by(exp_version) %>%
  arrange(exp_version, step_no) %>%
  mutate(
    trials_eliminated = lag(remaining_rows, default = first(remaining_rows)) - remaining_rows
  )


# Ensure filtering steps are ordered correctly
elimination_summary$step <- factor(
  elimination_summary$step,
  levels = c("start", "response_rt < 10", 
             "number_deviation < 4 & > -4", 
             "adjustment_duration > 1", 
             "adjustment_duration < 15", 
             "response_edge_to_edge_spacing > 0")
)


# Create the plot with correctly ordered steps and text labels
remaining_rows <- ggplot(elimination_summary, aes(x = step, y = trials_eliminated, fill = exp_version)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_text(aes(label = trials_eliminated), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, 
            size = 4) +
  facet_wrap(~exp_version) +
  labs(
    title = "Number of Trials Eliminated at Each Step",
    x = "Filtering Steps (Ordered)",
    y = "Trials Eliminated",
    fill = "Experiment Version"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Use a better color palette
  theme_minimal(base_size = 14) +       # Adjust font size for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11)
  )

ggsave("figures/removed_trials_all_exps.svg", plot = remaining_rows, width = 4, height = 4, units = "in", dpi = 300)


