# COURSE: STAT317, SECTION: A

# FINAL EXAM PROJECT

# RAMEEN JALIL, ROLL NO#: 251710245

# TOPIC: Polio (Pol3) immunization coverage among 1-year-olds (%)


# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(gganimate)
library(sf) # for geospatial data
library(rworldmap) # for world maps
library(leaflet)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(dplyr)

# Data
library(readr)
data <- read_csv("STAT317A/data.csv")
View(data)

# Select relevant columns and rename
data_clean <- data %>%
  select(IndicatorCode, ParentLocation, Location, Period, FactValueNumeric) %>%
  rename(
    Region = ParentLocation,
    Country = Location,
    Year = Period,
    Coverage = FactValueNumeric
  ) %>%
  drop_na()

# Check cleaned data
summary(data_clean)

# Line plot for global trends
global_trends <- data_clean %>%
  group_by(Year) %>%
  summarise(AverageCoverage = mean(Coverage, na.rm = TRUE))

ggplot(global_trends, aes(x = Year, y = AverageCoverage)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(
    title = "Global Trends in Polio (Pol3) Immunization Coverage",
    x = "Year",
    y = "Average Coverage (%)"
  )




# Boxplot for regional disparities
ggplot(data_clean, aes(x = Region, y = Coverage, fill = Region)) + 
  geom_boxplot() + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") +  # You can change the color palette here
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotates the labels 45 degrees
    plot.margin = margin(10, 10, 10, 30)  # Adds space to the bottom
  ) + 
  labs(
    title = "Regional Disparities in Polio (Pol3) Immunization Coverage",
    x = "Region",
    y = "Coverage (%)"
  )

# Filter for specific countries
countries_of_interest <- c("India", "Pakistan", "Nigeria")
country_data <- data_clean %>% filter(Country %in% countries_of_interest)

# Line plot for selected countries
ggplot(country_data, aes(x = Year, y = Coverage, color = Country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Polio (Pol3) Immunization Coverage in Selected Countries",
    x = "Year",
    y = "Coverage (%)"
  )




# Load world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")

# Remove Antarctica from the map data
world_map <- world_map[world_map$name != "Antarctica", ]

set.seed(42)  # For reproducibility
data_clean <- data.frame(
  Country = world_map$name,  # All country names
  Coverage = runif(nrow(world_map), min = 10, max = 90)  # Coverage range from 10 to 90
)

# Merge your data with world map
map_data <- merge(world_map, data_clean, by.x = "name", by.y = "Country", all.x = TRUE)

# Create a color palette with the range from 10 to 90
palette <- colorBin(
  palette = "YlGnBu",  # Yellow-Green-Blue palette
  domain = map_data$Coverage,
  bins = seq(10, 90, by = 10)  # Custom bins from 10 to 90
)

# Create the interactive map
leaflet(map_data) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(
    fillColor = ~palette(Coverage),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste0(name, ": ", 
                    ifelse(is.na(Coverage), "No data", paste0(format(Coverage, digits = 3), "%"))),  # Tooltip with formatted coverage as percentage
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette,
    values = ~Coverage,
    title = "Polio (Pol3) Coverage (%)",
    position = "bottomright",
    opacity = 1,  # Adjust opacity of the color bar
    labFormat = labelFormat(suffix = "%")  # Format the legend labels as percentages
  ) %>%
  addControl(
    html = "<b>Polio Immunization Coverage</b><br>Hover over a country for details",
    position = "topright"
  )



# install.packages("countrycode")
library(countrycode)
library(plotly)
library(dplyr)

# Get all ISO-3166-1 alpha-3 country codes
countries <- countrycode::codelist$iso3c  # This gives the full list of country codes

# Remove Antarctica ('ATA') from the country list
countries <- countries[countries != "ATA"]

# Random data for each year (2000 to 2023) for each country
years <- 2000:2023
data <- expand.grid(country = countries, year = years)
data$value <- runif(nrow(data), min = 1, max = 100)  # Random data values

# Create animated choropleth map with reduced animation speed
plot_ly(data = data, 
        locations = ~country, 
        z = ~value, 
        color = ~value, 
        colors = 'YlGnBu',  # Color scale (you can change this if you like)
        type = 'choropleth', 
        locationmode = 'ISO-3', 
        frame = ~year, 
        hoverinfo = 'location+z', 
        colorbar = list(title = 'Value')) %>%
  layout(title = "Animated Choropleth Map (2000-2023)",
         geo = list(showframe = FALSE, showcoastlines = TRUE, coastlinecolor = "gray", projection = list(type = 'natural earth'))) %>%
  animation_opts(frame = 200, redraw = TRUE)  # Slower animation by increasing frame duration (200ms per frame)

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Load the data
data <- read_csv("data.csv")

# Select relevant columns and clean the data
data_clean <- data %>%
  select(IndicatorCode, ParentLocation, Location, Period, FactValueNumeric) %>%
  rename(
    Region = ParentLocation,
    Country = Location,
    Year = Period,
    Coverage = FactValueNumeric
  ) %>%
  drop_na()

# Categorize regions into developed and developing
developed_regions <- c("Europe", "Americas")
data_clean <- data_clean %>%
  mutate(DevelopmentStatus = ifelse(Region %in% developed_regions, "Developed", "Developing"))

# Calculate average coverage by development status and year
avg_coverage <- data_clean %>%
  group_by(DevelopmentStatus, Year) %>%
  summarize(AverageCoverage = mean(Coverage, na.rm = TRUE))

# Plot the data
ggplot(avg_coverage, aes(x = Year, y = AverageCoverage, color = DevelopmentStatus, group = DevelopmentStatus)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Polio Immunization Coverage: Developed vs. Developing Nations",
    x = "Year",
    y = "Average Coverage (%)",
    color = "Development Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )




# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Load the data
data <- read_csv("data.csv")

# Select relevant columns and clean the data
data_clean <- data %>%
  select(IndicatorCode, ParentLocation, Location, Period, FactValueNumeric) %>%
  rename(
    Region = ParentLocation,
    Country = Location,
    Year = Period,
    Coverage = FactValueNumeric
  ) %>%
  drop_na()

# Categorize regions into developed and developing
developed_regions <- c("Europe", "Americas")
data_clean <- data_clean %>%
  mutate(DevelopmentStatus = ifelse(Region %in% developed_regions, "Developed", "Developing"))

# Calculate average coverage by development status and year
avg_coverage <- data_clean %>%
  group_by(DevelopmentStatus, Year) %>%
  summarize(AverageCoverage = mean(Coverage, na.rm = TRUE))

# Create the plot with customized tooltip and formatted percentages
p <- ggplot(avg_coverage, aes(x = Year, y = AverageCoverage, group = DevelopmentStatus, color = DevelopmentStatus)) +
  geom_line(size = 1) +
  geom_point(size = 2, aes(text = paste(
    "Year:", Year,
    "<br>Average Coverage:", sprintf("%.1f%%", AverageCoverage), # Format to 1 decimal place
    "<br>Development Status:", DevelopmentStatus
  ))) +
  labs(
    title = "Polio Immunization Coverage: Developed vs. Developing Nations",
    x = "Year",
    y = "Average Coverage (%)",
    color = "Development Status"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1)) + # 1 decimal on the y-axis
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Convert to an interactive plot
interactive_plot <- ggplotly(p, tooltip = "text")

# Display the interactive plot
interactive_plot
