# Required libraries
library(tidyverse)
library(gmodels)

# Load the dataset
data <- read.csv("rolling_stone.csv")

# 1. Dimensions and column names of the dataset
data_info <- list(
  Dimensions = dim(data),
  Column_Names = colnames(data)
)
print("Dataset dimensions and column names:")
print(data_info)

# 2. Missing data analysis
missing_data <- data %>%
  summarise_all(~sum(is.na(.)))
print("Number of missing values:")
print(missing_data)

missing_data_percent <- data %>%
  summarise_all(~mean(is.na(.)) * 100)
print("Percentage of missing values (%):")
print(missing_data_percent)

# 3. Frequency analysis of categorical variables
categorical_summary <- data %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~list(table(.))))
print("Frequency analysis of categorical variables:")
print(categorical_summary)

# 4. Summary statistics for numerical variables
numerical_summary <- data %>%
  select(where(is.numeric)) %>%
  summarise_all(list(
    mean = ~mean(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE)
  ))
print("Summary statistics for numerical variables:")
print(numerical_summary)

# 5. Top 5 observations with the lowest rank in 2003
top_5_rows <- data %>%
  arrange(rank_2003) %>%
  head(5)
print("Top 5 observations with the lowest rank in 2003:")
print(top_5_rows)

# 6. Filter data based on specific conditions
filtered_data <- data %>%
  filter(rank_2012 > 50 & ave_age_at_top_500 < 100)
print("Data filtered based on specific conditions:")
print(filtered_data)

# 7. Add a new column
data <- data %>%
  mutate(rank_difference = rank_2003 - rank_2020)
print("New column (rank_difference) added:")
head(data)

# 8. Grouping and calculating averages
grouped_data <- data %>%
  group_by(genre) %>%
  summarize(avg_popularity = mean(spotify_popularity, na.rm = TRUE))
print("Average Spotify popularity by genre:")
print(grouped_data)

# 9. Pivoting the dataset (wide to long format)
long_data <- data %>%
  pivot_longer(
    cols = c(rank_2003, rank_2012, rank_2020),
    names_to = "year",
    values_to = "rank"
  )
print("Pivoted dataset (long format):")
head(long_data)

# 10. Relationship between categorical variables (cross-tabulation)
cross_tab <- CrossTable(data$genre, data$artist_gender, prop.chisq = FALSE)
print("Relationship between categorical variables (cross-tabulation):")
print(cross_tab)

# 11. Visualizing popularity by genre
grouped_data %>%
  ggplot(aes(x = reorder(genre, -avg_popularity), y = avg_popularity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Spotify Popularity by Genre",
    x = "Genre",
    y = "Average Popularity"
  ) +
  theme_minimal()









# Load necessary libraries
library(tidyverse)
library(gmodels)

# Load the dataset
data <- read.csv("rolling_stone.csv")

# 1. Bar chart: Distribution of genres
data %>%
  count(genre) %>%
  ggplot(aes(x = reorder(genre, -n), y = n, fill = genre)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of Genres", x = "Genre", y = "Count") +
  theme_minimal()

# 2. Histogram: Distribution of Spotify popularity
data %>%
  ggplot(aes(x = spotify_popularity)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Spotify Popularity", x = "Popularity", y = "Count") +
  theme_minimal()

# 3. Boxplot: Spotify popularity by genre
data %>%
  ggplot(aes(x = genre, y = spotify_popularity, fill = genre)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Spotify Popularity by Genre", x = "Genre", y = "Popularity") +
  theme_minimal()

# 4. Scatterplot: Rank 2003 vs. Rank 2020
data %>%
  ggplot(aes(x = rank_2003, y = rank_2020)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Rank 2003 vs. Rank 2020", x = "Rank 2003", y = "Rank 2020") +
  theme_minimal()

# 5. Line plot: Average rank over years
data_long <- data %>%
  pivot_longer(cols = c(rank_2003, rank_2012, rank_2020), names_to = "year", values_to = "rank")

data_long %>%
  group_by(year) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_rank, group = 1)) +
  geom_line(color = "red", size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Average Rank Over Years", x = "Year", y = "Average Rank") +
  theme_minimal()

# 6. Heatmap: Relationship between genre and artist gender
data %>%
  count(genre, artist_gender) %>%
  ggplot(aes(x = genre, y = artist_gender, fill = n)) +
  geom_tile(color = "white") +
  labs(title = "Genre vs. Artist Gender", x = "Genre", y = "Artist Gender", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Density plot: Spotify popularity by genre
data %>%
  ggplot(aes(x = spotify_popularity, fill = genre)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Spotify Popularity by Genre", x = "Popularity", y = "Density") +
  theme_minimal()

# 8. Pie chart: Genre proportions
data %>%
  count(genre) %>%
  ggplot(aes(x = "", y = n, fill = genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Genre Proportions") +
  theme_void()

# 9. Violin plot: Spotify popularity by artist gender
data %>%
  ggplot(aes(x = artist_gender, y = spotify_popularity, fill = artist_gender)) +
  geom_violin() +
  labs(title = "Spotify Popularity by Artist Gender", x = "Artist Gender", y = "Popularity") +
  theme_minimal()

# 10. Bubble chart: Genre by average rank difference
data %>%
  group_by(genre) %>%
  summarise(avg_rank_diff = mean(rank_2003 - rank_2020, na.rm = TRUE),
            count = n()) %>%
  ggplot(aes(x = genre, y = avg_rank_diff, size = count, color = genre)) +
  geom_point(alpha = 0.7) +
  labs(title = "Genre by Average Rank Difference", x = "Genre", y = "Average Rank Difference", size = "Count") +
  theme_minimal() +
  coord_flip()

# 11. Faceted scatterplot: Rank 2003 vs. Rank 2020 by genre
data %>%
  ggplot(aes(x = rank_2003, y = rank_2020, color = genre)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~genre) +
  labs(title = "Rank 2003 vs. Rank 2020 by Genre", x = "Rank 2003", y = "Rank 2020") +
  theme_minimal()

# 12. Bar chart: Top 10 most frequent artists
data %>%
  count(artist_name) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(artist_name, n), y = n, fill = artist_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Frequent Artists", x = "Artist Name", y = "Count") +
  theme_minimal()

# 13. Histogram: Distribution of rank differences
data %>%
  mutate(rank_difference = rank_2003 - rank_2020) %>%
  ggplot(aes(x = rank_difference)) +
  geom_histogram(binwidth = 5, fill = "purple", color = "white") +
  labs(title = "Distribution of Rank Differences", x = "Rank Difference", y = "Count") +
  theme_minimal()

# 14. Heatmap: Rank differences by genre
data %>%
  mutate(rank_difference = rank_2003 - rank_2020) %>%
  group_by(genre) %>%
  summarise(avg_diff = mean(rank_difference, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(genre, -avg_diff), y = avg_diff, fill = avg_diff)) +
  geom_tile() +
  labs(title = "Average Rank Difference by Genre", x = "Genre", y = "Average Difference", fill = "Difference") +
  theme_minimal()

# 15. Scatterplot: Spotify popularity vs. artist age
data %>%
  ggplot(aes(x = ave_age_at_top_500, y = spotify_popularity)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = "Spotify Popularity vs. Artist Age", x = "Average Age at Top 500", y = "Spotify Popularity") +
  theme_minimal()

# 16. Line plot: Artist frequency over years
artist_year_data <- data %>%
  pivot_longer(cols = c(rank_2003, rank_2012, rank_2020), names_to = "year", values_to = "rank")

artist_year_data %>%
  count(artist_name, year) %>%
  ggplot(aes(x = year, y = n, group = artist_name, color = artist_name)) +
  geom_line() +
  labs(title = "Artist Frequency Over Years", x = "Year", y = "Frequency") +
  theme_minimal()


# 17. Bar chart: Distribution of artists by gender
data %>%
  count(artist_gender) %>%
  ggplot(aes(x = artist_gender, y = n, fill = artist_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Artists by Gender", x = "Gender", y = "Count") +
  theme_minimal()

# 18. Scatterplot: Rank difference vs. Spotify popularity
data %>%
  mutate(rank_difference = rank_2003 - rank_2020) %>%
  ggplot(aes(x = rank_difference, y = spotify_popularity, color = genre)) +
  geom_point(alpha = 0.6) +
  labs(title = "Rank Difference vs. Spotify Popularity", x = "Rank Difference", y = "Spotify Popularity") +
  theme_minimal()

# 19. Density plot: Rank differences by artist gender
data %>%
  mutate(rank_difference = rank_2003 - rank_2020) %>%
  ggplot(aes(x = rank_difference, fill = artist_gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Rank Differences by Artist Gender", x = "Rank Difference", y = "Density") +
  theme_minimal()

# 20. Faceted bar chart: Spotify popularity distribution by genre
data %>%
  ggplot(aes(x = spotify_popularity, fill = genre)) +
  geom_histogram(binwidth = 5, color = "white", alpha = 0.7) +
  facet_wrap(~genre) +
  labs(title = "Spotify Popularity Distribution by Genre", x = "Spotify Popularity", y = "Count") +
  theme_minimal()





