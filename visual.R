install.packages("tidyverse")
install.packages("rnaturalearth")
install.packages("sf")
install.packages("readr")
install.packages("scales")
install.packages("gridExtra")
install.packages("plotly")


#======================================MAP=========================================
library(tidyverse)
library(rnaturalearth)
library(sf)
library(scales)
library(plotly)

# Load data
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv") 

# Calculation of the average post-natal care rate
average_care <- unicef_indicator_2 %>%
  group_by(alpha_3_code) %>%
  summarise(avg_postnatal_care = mean(obs_value, na.rm = TRUE))

# Load map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merging of average postpartum care data into world map data
world_with_data <- left_join(world, average_care, by = c("iso_a3" = "alpha_3_code"))

# Add a custom text column to the dataframe for plotly's mouseover text
world_with_data <- world_with_data %>%
  mutate(hover_text = sprintf("Country: %s\nAvg Postnatal Care: %.2f%%", name, avg_postnatal_care))

# Creating ggplot plots, referencing custom text
m <- ggplot(data = world_with_data) +
  geom_sf(aes(fill = avg_postnatal_care, text = hover_text)) + # Use text aesthetic to reference hover_text
  scale_fill_gradient(low = "#FFCCCC", high = "#E15252", name = "Avg Postnatal Care (%)") +
  labs(title = "Average Postnatal Care Percentage by Country",
       subtitle = "Percentage of newborns receiving postnatal care") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Converts ggplot plots to interactive plotly plots, specifying the use of custom mouse hover text
interactive_m <- ggplotly(m, tooltip = "text")

# Displaying Interactive Charts
interactive_m
#====================================BAR CHART=========================================
library(tidyverse)
library(readr)
library(ggplot2)
library(gridExtra)


# Load data
unicef_metadata <- read_csv("unicef_metadata.csv") 
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv") 

# Join the data on country and year = time_period
merged_data <- unicef_metadata %>%
  inner_join(unicef_indicator_2, by = c("country", "year" = "time_period"))

# Calculation of average life expectancy and average share of postnatal care per country
average_data <- merged_data %>%
  group_by(country) %>%
  summarise(
    avg_life_expectancy = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE),
    avg_postnatal_care = mean(obs_value, na.rm = TRUE)
  )

# top 10 countries in terms of average life expectancy
top_avg_life_expectancy <- average_data %>%
  arrange(desc(avg_life_expectancy)) %>%
  slice_head(n = 10)

# For the top 10 countries in terms of average share of postpartum care
top_avg_postnatal_care <- average_data %>%
  arrange(desc(avg_postnatal_care)) %>%
  slice_head(n = 10)

# 定义颜色向量，循环使用两种颜色
colors <- c("#E15252", "#52E1E1", "#E1E152", "#52E152", "#5252E1",
            "#E152E1", "#E18252", "#52E1A1", "#A1E152", "#E152A1")

# 更新p1图表
p1 <- ggplot(top_avg_life_expectancy, aes(x = reorder(country, avg_life_expectancy), y = avg_life_expectancy, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(title = "Top 10 Countries by Average Life Expectancy",
       x = "", y = "Average Life Expectancy (Years)") +
  theme(legend.title = element_blank(), legend.position = "none")

# 更新p2图表
p2 <- ggplot(top_avg_postnatal_care, aes(x = reorder(country, avg_postnatal_care), y = avg_postnatal_care, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(title = "Top 10 Countries by Average Postnatal Care Percentage",
       x = "", y = "Average Postnatal Care (%)") +
  theme(legend.title = element_blank(), legend.position = "none")

# Arranging shapes up and down using grid.arrange
grid.arrange(p1, p2, ncol = 1)

#=======================Scatterplot with a linear regression line============================
library(tidyverse)
library(plotly)
# Load data
unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")

# Join the data 
merged_data <- unicef_metadata %>%
  inner_join(unicef_indicator_2, by = c("country", "year" = "time_period"))

# Calculation of average postnatal care rates and GDP per capita per country
average_data <- merged_data %>%
  group_by(country) %>%
  summarise(avg_postnatal_care = mean(obs_value, na.rm = TRUE),
            avg_gdp_per_capita = mean(`GDP per capita (constant 2015 US$)`, na.rm = TRUE))

# Generate a scatterplot and add a linear regression line
s <- ggplot(average_data, aes(x = avg_gdp_per_capita, y = avg_postnatal_care)) +
  geom_point(aes(color = country), size = 2) +  # Mark different countries with colors
  geom_smooth(method = "lm", color = "blue", formula = y ~ x) +  # Adding a linear regression line
  coord_cartesian(ylim = c(0, 100)) +  # Limit the display range of the y-axis to ensure that it does not exceed 100 percent
  theme_minimal() +
  labs(title = "Average Postnatal Care Rate vs. GDP per Capita",
       x = "Average GDP per Capita (Constant 2015 US$)",
       y = "Average Postnatal Care Rate (%)") +
  theme(legend.position = "none") 

# Converting ggplot2 Graphs to Interactive Graphs
ggplotly(s)

#================================Time-series chart=================================
library(tidyverse)
library(plotly)
# Read data
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")

# Selection of countries with at least three years of data
countries_with_at_least_three_years <- unicef_indicator_2 %>%
  group_by(country) %>%
  filter(n_distinct(time_period) >= 3) %>%
  ungroup()

# Plotting time series
p <- ggplot(countries_with_at_least_three_years, aes(x = time_period, y = obs_value, group = country, color = country)) +
  geom_line() +  # Drawing Lines
  geom_point() +  # Add points to highlight each data point
  theme_minimal() +
  labs(title = "Postnatal Care Rate Trends Over Time by Country",
       x = "Year",
       y = "Postnatal Care Rate (%)") +
  theme(legend.position = "right")  # Place the legend on the right

# Converting ggplot2 Graphs to Interactive Graphs
ggplotly(p)

# Convert to plotly
p_plotly <- ggplotly(p)

# Setting plotly's legend interaction behavior
style(p_plotly,
      traces = c(1:nrow(countries_with_at_least_three_years)), # Apply to all lines
      hoverinfo = 'text', # Setting the message to be displayed when the mouse is hovering
      unselected = list(marker = list(opacity = 0.2))) # Unchecked data series transparency set to 0.2

# Show the chart
p_plotly