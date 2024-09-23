# * TI1. Assignment Calculating Central Tendencies,
# * Dispersion and Sampling Distribution values

# * Libraries used:
library(ggplot2) # Data visualization
library(hrbrthemes) # Addition themes for ggplot2
library(openxlsx) # Read Excel files
library(dplyr) # For data manipulation
library(tidyr) # For pivot_longer
library(gridExtra) # For arranging plots

# * CENTRAL TENDENCY MEASURES, DISPERSION AND SAMPLE DISTRIBUTIONS
# * 1. The following table contains the ages of people who are frequent
# * customers of a vegetarian restaurant.

customer_ages <- c(
  28, 23, 31, 31, 21, 30, 28, 23, 29, 28, 36, 36, 31, 28, 28, 40, 22, 21, 28, 31
)

# * With this data, calculate:

# - The mean:

mean(customer_ages)
# R: 28.65

# - The median:

median(customer_ages)
# R: 28

# - The mode:

# To my surprise there is no native R function to obtain the mode!
# I guess it's because the mode is not always one value?

mode <- function(x) {
  unique_x <- unique(x) # Get unique values
  tabulated <- tabulate(match(x, unique_x)) # Tabulate occurrences
  max_freq <- max(tabulated) # Find the maximum frequency
  modes <- unique_x[tabulated == max_freq] # Find all values with max frequency
  return(modes)
}

print(mode(customer_ages))
# R: 28

# - The frequency histogram:

hist(customer_ages)

# Alternative (and prettier) option:

histogram <- ggplot(data.frame(customer_ages), aes(x = customer_ages)) +
  geom_histogram(aes(fill = ..count..), binwidth = 4, color = "black") +
  scale_fill_gradient(low = "#ff60e7", high = "#97005b") +
  theme_ipsum() +
  labs(
    title = "Histogram of customer ages",
    x = "Ages",
    y = "Frequency",
    fill = "Count"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    legend.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black")
  )

ggsave(
  "./assets/histogram.jpg",
  plot = histogram, width = 6, height = 4, units = "in", dpi = 600
)

# With the data in the table above, determine the variance and standard
# deviation.

var(customer_ages)

# R: 25.71316

sd(customer_ages)

# R: 5.070814

# Using the mean and two standard deviation, determine the Chebyshev interval
# and interpret its meaning in the context of the problem. (Please watch the
# following video for a better understanding: Teorema de Chebyshev

lower_bound <- mean(customer_ages) - sd(customer_ages) * 2
upper_bound <- mean(customer_ages) + sd(customer_ages) * 2

print(upper_bound)
# R: 38.79168
print(lower_bound)
# R: 18.50832

# Calculate percentage of data within the Chebyshev interval
chebyshev_interval <- customer_ages[
  customer_ages >= lower_bound & customer_ages <= upper_bound
]

length(chebyshev_interval) / length(customer_ages) * 100
# R: 95%

# Find the values outside the Chebyshev interval
print(customer_ages[customer_ages < lower_bound | customer_ages > upper_bound])
# R: 40

# Chebychev's theorem states that at least 75% of the data is within 2 standard
# deviations of the mean, in this case, 95% of the data is within 2 standard
# deviations of the mean (more than 75%), which is a good indicator that the
# data is normally distributed.

# Explain what the Central Tendency measures indicate and what the dispersion
# measures indicate.

# The central tendency measures (mean, median, mode) indicate the average value
# the middle value and the most frequent value in the data set respectively. In
# this case, the mean age of the customers is 28.65, the median age is 28 and
# the mode is 28. The dispersion measures (variance, standard deviation)
# indicate how spread out the data is from the mean. In this case, the variance
# is 25.71316 and the standard deviation is 5.070814, which means that the data
# is spread out around 5.07 units from the mean. We could say that the data is
# normally distributed.

# Using the data from the database titled POP BEVERAGES CONSUMPTION SURVEY, use
# R studio to determine, for each age category of consumers, the following about
# the Glasses of soft drink per day:

rural_youth <- read.xlsx(
  "./data/POP BEVERAGES CONSUMPTION SURVEY-1.xlsx",
  sheet = 1
)

urban_youth <- read.xlsx(
  "./data/POP BEVERAGES CONSUMPTION SURVEY-1.xlsx",
  sheet = 2
)

urban_20to40 <- read.xlsx(
  "./data/POP BEVERAGES CONSUMPTION SURVEY-1.xlsx",
  sheet = 3
)

rural_20to40 <- read.xlsx(
  "./data/POP BEVERAGES CONSUMPTION SURVEY-1.xlsx",
  sheet = 4
)

urban_40to60 <- read.xlsx(
  "./data/POP BEVERAGES CONSUMPTION SURVEY-1.xlsx",
  sheet = 5
)

rural_40to60 <- read.xlsx(
  "./data/POP BEVERAGES CONSUMPTION SURVEY-1.xlsx",
  sheet = 6
)

# *** Data wrangling

wrangling <- function(data) {
  # Remove the first and last column
  data <- data[, -c(1, ncol(data))]
  # Remove the first row
  data <- data[-1, ]
  # Reset row names
  rownames(data) <- NULL
  # Change column names
  colnames(data) <- c("soft_drinks_per_day", "age", "occupation")
  # Convert the first and second columns to numeric (they are characters)
  data$soft_drinks_per_day <- as.numeric(data$soft_drinks_per_day)
  data$age <- as.numeric(data$age)
  return(data)
}

rural_youth <- wrangling(rural_youth)
urban_youth <- wrangling(urban_youth)
urban_20to40 <- wrangling(urban_20to40)
rural_20to40 <- wrangling(rural_20to40)
urban_40to60 <- wrangling(urban_40to60)
rural_40to60 <- wrangling(rural_40to60)

View(rural_youth)
View(urban_youth)
View(urban_20to40)
View(rural_20to40)
View(urban_40to60)
View(rural_40to60)

# a) The histogram frequency for each age category in rural area and the bias
# presented by the data from the graph comparing the mean to the median

# Let's give all occupations a colorblind friendly palette (Okabe Ito palette)
all_occupations <- c(
  rural_youth$occupation, urban_youth$occupation, urban_20to40$occupation,
  rural_20to40$occupation, urban_40to60$occupation, rural_40to60$occupation
)

print(unique(all_occupations)) # Student, Housekeeper, Employee and Retired

occupation_colors <- c(
  "Student" = "#E69F00", "Housekeeper" = "#56B4E9",
  "Employee" = "#009E73", "Retired" = "#F0E442"
)

# Add a 'group' column to each dataframe
rural_youth$category <- "Youth"
rural_20to40$category <- "20 to 40"
rural_40to60$category <- "40 to 60"

# Combine the dataframes
rural_df <- rbind(rural_youth, rural_20to40, rural_40to60)

# Convert 'category' to a factor with levels in the desired order
rural_df$category <- factor(
  rural_df$category,
  levels = c("Youth", "20 to 40", "40 to 60")
)

# Calculate mean and median for each category
category_stats_rural <- rural_df %>%
  group_by(category) %>%
  summarise(
    mean = mean(soft_drinks_per_day),
    median = median(soft_drinks_per_day)
  ) %>%
  pivot_longer(-category, names_to = "LineType", values_to = "xintercept")

# Merge LineType with original dataframe to indicate mean or median
category_stats_rural$LineType <- factor(
  category_stats_rural$LineType,
  levels = c("mean", "median")
)

divided_rural_plot <- ggplot(
  rural_df, aes(x = soft_drinks_per_day, fill = occupation)
) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(
    values = occupation_colors,
    breaks = c("Student", "Housekeeper", "Employee", "Retired")
  ) +
  facet_wrap(~category, scales = "free_y") +
  geom_vline(
    data = category_stats_rural, aes(xintercept = xintercept, color = LineType),
    linetype = "dashed", size = 0.7
  ) +
  scale_color_manual(
    values = c("mean" = "red", "median" = "blue"),
    name = "Central T.", labels = c("Mean", "Median"),
    guide = guide_legend(override.aes = list(linetype = "dashed"))
  ) +
  scale_x_continuous(
    breaks = seq(min(rural_df$soft_drinks_per_day),
      max(rural_df$soft_drinks_per_day),
      by = 1
    )
  ) +
  labs(
    title = "Soft Drinks Consumption by Age Category",
    x = "Soft Drinks per Day",
    y = "Frequency",
    fill = "Occupation"
  ) +
  theme_ipsum() +
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    legend.title = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave(
  "./assets/divided_rural_plot.jpg",
  plot = divided_rural_plot,
  width = 10, height = 6, units = "in", dpi = 600
)

# b) The histogram frequency for each age category in urban area and the bias
# presented by the data from the graph comparing the mean to the median

# Add a 'group' column to each dataframe
urban_youth$category <- "Youth"
urban_20to40$category <- "20 to 40"
urban_40to60$category <- "40 to 60"

# Combine the dataframes
urban_df <- rbind(urban_youth, urban_20to40, urban_40to60)

# Convert 'category' to a factor with levels in the desired order
urban_df$category <- factor(
  urban_df$category,
  levels = c("Youth", "20 to 40", "40 to 60")
)

# Calculate mean and median for each category
category_stats_urban <- urban_df %>%
  group_by(category) %>%
  summarise(
    mean = mean(soft_drinks_per_day),
    median = median(soft_drinks_per_day)
  ) %>%
  pivot_longer(-category, names_to = "LineType", values_to = "xintercept")

# Merge LineType with original dataframe to indicate mean or median
category_stats_urban$LineType <- factor(
  category_stats_urban$LineType,
  levels = c("mean", "median")
)

# Adjust the plot code to include the mean and median lines
divided_urban_plot <- ggplot(
  urban_df, aes(x = soft_drinks_per_day, fill = occupation)
) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(
    values = occupation_colors,
    breaks = c("Student", "Housekeeper", "Employee", "Retired")
  ) +
  facet_wrap(~category, scales = "free_y") +
  geom_vline(
    data = category_stats_urban, aes(xintercept = xintercept, color = LineType),
    linetype = "dashed", size = 0.7
  ) +
  scale_color_manual(
    values = c("mean" = "red", "median" = "blue"),
    name = "Central T.", labels = c("Mean", "Median"),
    guide = guide_legend(override.aes = list(linetype = "dashed"))
  ) +
  scale_x_continuous(
    breaks = seq(min(urban_df$soft_drinks_per_day),
      max(urban_df$soft_drinks_per_day),
      by = 1
    )
  ) +
  labs(
    title = "Soft Drinks Consumption by Age Category",
    x = "Soft Drinks per Day",
    y = "Frequency",
    fill = "Occupation"
  ) +
  theme_ipsum() +
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    legend.title = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave(
  "./assets/divided_urban_plot.jpg",
  plot = divided_urban_plot,
  width = 10, height = 6, units = "in", dpi = 600
)

# With all the data related to the consumption in rural area and in urban area,
# find:

# The frequency histogram of the rural area

# Calculate mean and median for the entire rural_df
overall_mean <- mean(rural_df$soft_drinks_per_day)
overall_median <- median(rural_df$soft_drinks_per_day)

complete_rural_plot <- ggplot(
  rural_df, aes(x = soft_drinks_per_day, fill = occupation)
) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(
    values = occupation_colors,
    breaks = c("Student", "Housekeeper", "Employee", "Retired")
  ) +
  geom_vline(
    aes(xintercept = overall_mean, color = "Mean"),
    linetype = "dashed", size = 0.7
  ) +
  geom_vline(
    aes(xintercept = overall_median, color = "Median"),
    linetype = "dashed", size = 0.7
  ) +
  scale_color_manual(
    values = c("Mean" = "red", "Median" = "blue"),
    name = "Central T.",
    labels = c("Mean", "Median")
  ) +
  scale_x_continuous(
    breaks = seq(min(rural_df$soft_drinks_per_day, na.rm = TRUE),
      max(rural_df$soft_drinks_per_day, na.rm = TRUE),
      by = 1
    )
  ) +
  labs(
    title = "Soft Drinks Consumption in Rural Area",
    x = "Soft Drinks per Day",
    y = "Frequency",
    fill = "Occupation"
  ) +
  theme_ipsum() +
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    legend.title = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave(
  "./assets/complete_rural_plot.jpg",
  plot = complete_rural_plot,
  width = 7.7, height = 4, units = "in", dpi = 600
)

# The frequency histogram of the urban area.

# Calculate mean and median for the entire urban_df
overall_mean <- mean(urban_df$soft_drinks_per_day)
overall_median <- median(urban_df$soft_drinks_per_day)

complete_urban_plot <- ggplot(
  urban_df, aes(x = soft_drinks_per_day, fill = occupation)
) +
  geom_histogram(binwidth = 1) +
  scale_fill_manual(
    values = occupation_colors,
    breaks = c("Student", "Housekeeper", "Employee", "Retired")
  ) +
  geom_vline(
    aes(xintercept = overall_mean, color = "Mean"),
    linetype = "dashed", size = 0.7
  ) +
  geom_vline(
    aes(xintercept = overall_median, color = "Median"),
    linetype = "dashed", size = 0.7
  ) +
  scale_color_manual(
    values = c("Mean" = "red", "Median" = "blue"),
    name = "Central T.",
    labels = c("Mean", "Median")
  ) +
  scale_x_continuous(
    breaks = seq(min(urban_df$soft_drinks_per_day, na.rm = TRUE),
      max(urban_df$soft_drinks_per_day, na.rm = TRUE),
      by = 1
    )
  ) +
  labs(
    title = "Soft Drinks Consumption in Urban Area",
    x = "Soft Drinks per Day",
    y = "Frequency",
    fill = "Occupation"
  ) +
  theme_ipsum() +
  theme(
    plot.title = element_text(face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    legend.title = element_text(face = "bold", color = "black"),
    strip.text = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave(
  "./assets/complete_urban_plot.jpg",
  plot = complete_urban_plot,
  width = 7.7, height = 4, units = "in", dpi = 600
)

# What are the main differences between the urban and rural area?

# Both in the visualization made by category and in the complete visualization
# we can clearly see how people in urban areas have a much lower frequency of
# consumption than people in rural areas. Furthermore, in rural areas, people
# between 40 and 60 years of age are the ones who consume more soft drinks,
# while in urban areas it is the young people. In general, people in rural areas
# consume more soft drinks than people in urban areas, which we can verify by
# seeing how the mean and median have the same separation distance, but in the
# urban graph it is closer to the left.
