library(dplyr)

africa_abs <- read.csv(file = "Summary_sheets/Kal_abs_ref.csv")
  
# First step: Calculating min, max, and mean for each ID within each Species
africa_parameters <- africa_abs %>%
  mutate(abs = as.numeric(gsub("C", "", abs))) %>%
  group_by(Species, ID) %>%
  summarise(
    abs_min = min(abs, na.rm = TRUE),
    abs_max = max(abs, na.rm = TRUE),
    abs_mean = mean(abs, na.rm = TRUE),
    .groups = "drop"  # Drop the grouping
  )
# Second step: Calculating the mean of the min, max, and mean values for each Species
species_means <- africa_parameters %>%
  group_by(Species) %>%
  summarise(
    mean_of_mins = mean(abs_min, na.rm = TRUE),
    mean_of_maxs = mean(abs_max, na.rm = TRUE),
    mean_of_means = mean(abs_mean, na.rm = TRUE)
  ) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))
