library(tidyverse)

# Make sure Dataset is imported
data <- battedBallData

# Calculating the true exit speed
data <- data %>%
  mutate(
    true_exit_speed = case_when(
      !is.na(speed_A) & !is.na(speed_B) ~ 0.85 * speed_A + 0.15 * speed_B, # Weighted average
      !is.na(speed_A) ~ speed_A, # Use speed_A if speed_B is NA
      !is.na(speed_B) ~ speed_B  # Use speed_B if speed_A is NA
    )
  )

# Group by batter and calculate average speed
batter_avg_speed <- data %>%
  group_by(batter) %>%
  summarize(
    avg_speed = mean(true_exit_speed, na.rm = TRUE)
  )

# Calculate the overall mean and standard deviation
mean_speed <- mean(batter_avg_speed$avg_speed, na.rm = TRUE)
sd_speed <- sd(batter_avg_speed$avg_speed, na.rm = TRUE)

# Define thresholds for performance categories
mean_plus_2sd <- mean_speed + 2 * sd_speed
mean_plus_1sd <- mean_speed + sd_speed
mean_minus_1sd <- mean_speed - sd_speed
mean_minus_2sd <- mean_speed - 2 * sd_speed

# Add the projected performance column next to avg_speed
batter_avg_speed <- batter_avg_speed %>%
  mutate(
    projected_performance = case_when(
      avg_speed > mean_plus_2sd ~ 'Great',
      avg_speed > mean_plus_1sd ~ 'Good',
      avg_speed > mean_speed ~ 'Above average',
      avg_speed > mean_minus_1sd ~ 'Below average',
      avg_speed > mean_minus_2sd ~ 'Bad',
      TRUE ~ 'Terrible'
    )
  )

# Ensure the column order is batter, avg_speed, and projected_performance
batter_avg_speed <- batter_avg_speed %>%
  select(batter, avg_speed, projected_performance)

# Save the updated results
# Edit username to find the results better
write.csv(batter_avg_speed, "/Users/username/Downloads/DataProjectProjections.csv", row.names = FALSE)