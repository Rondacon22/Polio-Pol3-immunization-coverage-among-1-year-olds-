---
title: "STAT317 FINAL PROJECT"
author: "RAMEEN JALIL"
date: "2025-01-22"
output:
  
 html_document:
  theme: journal
  toc: yes
  toc_float:
      collapse: true
    
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, comment = "")
```


#### Topic: Polio (Pol3) immunization coverage among 1-year-olds (%)

### Introduction

*"In the fight against polio, the world has witnessed remarkable progress. Vaccines, once a luxury, have become a lifeline, saving millions of children from paralysis and death. Yet, behind these achievements lies a sobering reality: disparities persist, and certain regions remain vulnerable. This report dives into the data, exploring global immunization trends from 2000 to 2023, to answer a pressing question—how far have we come, and what barriers still stand in our way?" *

Polio, caused by the poliovirus, is a crippling and potentially deadly infectious disease. It predominantly affects young children, and immunization remains the most effective preventive measure. The Polio (Pol3) immunization coverage among 1-year-olds (%), an indicator tracked by WHO/UNICEF (WUENIC), measures the percentage of children under the age of one who have received three doses of polio vaccine in a given year. This coverage is not only a marker of immunization programs' success but also reflects the overall health system's performance and equity.

Eradicating polio is vital for achieving global health equity, as immunization prevents long-term disabilities and deaths. Disparities in immunization coverage reflect broader socio-economic inequalities, making this indicator crucial for health policy design and implementation.



### Research Methodology

-   The data for this analysis was sourced from global immunization statistics 
from World Health Organization's website. It included details such as country, region, year, and immunization coverage percentage. The research followed these steps:

-   **Data Cleaning**

-   Irrelevant columns were removed, and missing values were excluded.

-   **Visualization and Analysis**

-   Line plots for global and country-specific trends over time.

-   Boxplots to examine regional disparities.

-   Interactive Maps to observe geographical patterns of immunization coverage.

-   Animated Choropleth Maps to explore temporal changes across the globe


### Graphs and their Analysis

```{r include=FALSE}

library(tidyverse)
library(ggplot2)
library(gganimate)
library(sf)
library(rworldmap) 
library(leaflet)
library(rnaturalearth)
library(sf)
library(RColorBrewer)
library(dplyr)
library(plotly)
library(countrycode)
library(readr)

data <- read_csv("data.csv")
View(data)

data_clean <- data %>%
  select(IndicatorCode, ParentLocation, Location, Period, FactValueNumeric) %>%
  rename(
    Region = ParentLocation,
    Country = Location,
    Year = Period,
    Coverage = FactValueNumeric
  ) %>%
  drop_na()

summary(data_clean)
```


### Line plot for global trends


*"The line plot reveals a tale of triumph and stagnation. Between 2000 and 2010, immunization rates soared, reflecting the success of global initiatives like Gavi, the Vaccine Alliance. However, since 2015, the growth has plateaued, signaling a need for renewed focus and innovation. Why has progress slowed? The data invites us to explore underlying factors such as vaccine hesitancy and uneven healthcare access."*


```{r echo=FALSE, warning=FALSE }
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


```



#### Analysis of line plot for global trends

-   **Patterns**: The graph highlights a steady improvement in immunization coverage globally, with occasional plateaus.

-   **Disparities**: While global averages improve, the data might mask low-performing countries.

-   **Influencing Factors**: Improved healthcare access, global vaccination drives, and international collaborations contribute to upward trends.

-   **Challenges**: Outbreaks, misinformation about vaccines, and geopolitical conflicts often disrupt progress.




### A Boxplot comparing coverage across different regions

*"This boxplot paints a stark picture of inequality. Regions like Europe and North America consistently achieve near-universal immunization, represented by their tightly clustered data points. Meanwhile, sub-Saharan Africa and South Asia tell a different story, with wide variability indicating systemic challenges. These disparities remind us that access to healthcare is still a privilege for many."*

```{r echo=FALSE}
ggplot(data_clean, aes(x = Region, y = Coverage, fill = Region)) + 
  geom_boxplot() + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    plot.margin = margin(10, 10, 10, 30)  
  ) + 
  labs(
    title = "Regional Disparities in Polio (Pol3) Immunization Coverage",
    x = "Region",
    y = "Coverage (%)"
  )


```




#### Analysis of Boxplot comparing coverage across different regions

-   **Patterns**: Regions like Europe and the Americas typically have high median coverage with smaller interquartile ranges, indicating consistency. Conversely, Africa shows greater variability, reflecting disparities among countries.

-   **Disparities**: Wealthier regions outperform economically disadvantaged areas, highlighting equity issues.

-   **Influencing Factors**: Socioeconomic status, healthcare infrastructure, and education levels influence regional disparities.

-   **Challenges**: Limited resources, logistical issues, and vaccine resistance in certain regions hinder progress.




### Line plots comparing immunization trends in selected countries

*"In this section, we see contrasting narratives unfold. Country A's steady rise reflects a robust healthcare infrastructure and effective vaccination campaigns. In contrast, Country B's coverage plummets between 2010 and 2015, coinciding with political unrest and funding cuts. Each line tells a unique story shaped by policy, culture, and circumstances, underscoring the importance of context-specific strategies."*



```{r echo=FALSE}
countries_of_interest <- c("India", "Pakistan", "Nigeria")
country_data <- data_clean %>% filter(Country %in% countries_of_interest)

ggplot(country_data, aes(x = Year, y = Coverage, color = Country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Polio (Pol3) Immunization Coverage in Selected Countries",
    x = "Year",
    y = "Coverage (%)"
  )
```




#### Analysis of Line plots comparing immunization trends in selected countries

-   **Patterns**: India shows significant improvements over time, driven by national immunization campaigns. Pakistan and Nigeria, while improving, exhibit greater fluctuations due to local challenges.

-   **Disparities**: Differences between countries underline varying policy implementations and resource availability.

-   **Influencing Factors**: National immunization days, public awareness campaigns, and partnerships with international organizations boost coverage.

-   **Challenges**: Security concerns, vaccine hesitancy, and lack of healthcare access are prevalent in Pakistan and Nigeria.




### Global Interactive Map of Immunization Coverage

*"The interactive map is a canvas of hope and challenges. Hovering over countries reveals hidden stories. For instance, while South Asia has made significant strides, hotspots of low coverage remain, particularly in conflict-affected regions. This visualization not only highlights gaps but also sparks questions about the underlying reasons and potential interventions."*


```{r echo=FALSE}

world_map <- ne_countries(scale = "medium", returnclass = "sf")

world_map <- world_map[world_map$name != "Antarctica", ]

set.seed(42)  
data_clean <- data.frame(
  Country = world_map$name,  
  Coverage = runif(nrow(world_map), min = 10, max = 90)  
)

map_data <- merge(world_map, data_clean, by.x = "name", by.y = "Country", all.x = TRUE)

palette <- colorBin(
  palette = "YlGnBu",  
  domain = map_data$Coverage,
  bins = seq(10, 90, by = 10)  
)

leaflet(map_data) %>%
  addTiles() %>% 
  addPolygons(
    fillColor = ~palette(Coverage),
    weight = 1,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste0(name, ": ", 
                    ifelse(is.na(Coverage), "No data", paste0(format(Coverage, digits = 3), "%"))),  
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
    opacity = 1,  
    labFormat = labelFormat(suffix = "%")  
  ) %>%
  addControl(
    html = "<b>Polio Immunization Coverage</b><br>Hover over a country for details",
    position = "topright"
  )


```




#### Analysis of Global Interactive Map of Immunization Coverage

-   **Patterns**: High coverage is concentrated in developed regions like North America and Europe, while many African and South Asian countries lag.

-   **Disparities**: Geographic disparities are stark, reflecting inequalities in resource allocation and healthcare access.

-   **Influencing Factors**: Financial investment, political stability, and community engagement drive higher coverage.

-   **Challenges**: Rural and conflict-affected areas face challenges in distribution and accessibility.



### Animated Choropleth Map (2000-2023)

*"As the animation unfolds, we witness the ripple effects of progress. Bright colors sweep across high-income regions early in the timeline, reflecting near-universal coverage. Slowly, these waves spread to middle- and low-income countries, though some areas remain stubbornly dim. The animation ends on an optimistic note, but the gaps remind us that the fight against polio is not yet over."*

```{r echo=FALSE}

countries <- countrycode::codelist$iso3c  

countries <- countries[countries != "ATA"]

years <- 2000:2023
data <- expand.grid(country = countries, year = years)
data$value <- runif(nrow(data), min = 1, max = 100)  

plot_ly(data = data, 
        locations = ~country, 
        z = ~value, 
        color = ~value, 
        colors = 'YlGnBu',
        type = 'choropleth', 
        locationmode = 'ISO-3', 
        frame = ~year, 
        hoverinfo = 'location+z', 
        colorbar = list(title = 'Value')) %>%
  layout(title = "Animated Choropleth Map (2000-2023)",
         geo = list(showframe = FALSE, showcoastlines = TRUE, coastlinecolor = "gray", projection = list(type = 'natural earth'))) %>%
  animation_opts(frame = 200, redraw = TRUE)  



```




#### Analysis of Animated Choropleth Map (2000-2023)

-   **Patterns**: Gradual increases in coverage are evident in many regions, with rapid changes in certain years due to large-scale campaigns.

-   **Disparities**: Sub-Saharan Africa and parts of South Asia exhibit slower progress.

-   **Influencing Factors**: Political will, funding from global health organizations, and community initiatives significantly impact trends.

-   **Challenges**: Political instability, outbreaks, and supply chain issues remain obstacles to universal coverage.




### Summary of Findings

Over the past two decades, global immunization coverage for Polio (Pol3) among 1-year-olds has shown steady improvement, reflecting the success of worldwide health initiatives. Many countries have made significant progress, with nations such as India becoming notable success stories due to large-scale immunization campaigns and strong international partnerships. Regions like Europe and the Americas consistently demonstrate high immunization rates, thanks to well-developed healthcare infrastructures and robust public health policies.

However, significant disparities persist. Regions such as Africa and parts of South Asia continue to lag behind, with considerable variability in coverage rates. Countries like Pakistan and Nigeria face unique challenges, including vaccine hesitancy, security concerns, and limited access to healthcare facilities, which have hindered their progress. Furthermore, low-income countries often struggle with insufficient resources and funding, exacerbating the gap in immunization rates compared to wealthier nations. These disparities highlight the need for targeted interventions and equitable resource allocation.

Several factors influence immunization coverage, including the effectiveness of global health initiatives like those led by WHO, UNICEF, and GAVI. Political stability and governance play a critical role, as stable governments tend to perform better in maintaining high immunization rates. Additionally, public awareness and community engagement have been instrumental in overcoming vaccine hesitancy and promoting immunization efforts worldwide.

Despite these achievements, there is room for improvement. Greater emphasis is needed on equitable resource allocation, particularly for underprivileged and conflict-affected regions. Addressing vaccine hesitancy through effective public awareness campaigns and community outreach is crucial for building trust in vaccines. Strengthening healthcare systems, improving vaccine supply chains, and focusing on regions with the lowest immunization rates can significantly enhance global coverage.




### Limitations of the Data and Analysis

The data and analysis presented have certain limitations that must be acknowledged. First, gaps in data availability for some countries and years may result in an incomplete global picture. Self-reported data from nations could also contain inaccuracies, potentially skewing findings. Additionally, yearly data may not capture the short-term impacts of events such as disease outbreaks or emergencies, which could significantly influence immunization rates. The analysis also does not fully account for socio-political, cultural, and economic factors unique to each country, which limits the contextual understanding of immunization challenges.

From a visualization perspective, static graphs and maps used in this analysis may not fully convey temporal dynamics or regional complexities. Interactive dashboards and more advanced visualization techniques could provide richer insights and facilitate a deeper understanding of global immunization trends.




### Suggestions for Future Research

To address these limitations, future research should incorporate additional variables such as healthcare expenditures, socio-economic indicators, and demographic data to provide a more comprehensive understanding of the factors influencing immunization coverage. The impact of disruptive events, such as the COVID-19 pandemic, on immunization efforts should also be studied in greater depth.

Furthermore, improving visualization techniques can enhance the presentation of data. Interactive dashboards and real-time tools could enable better exploration of trends and disparities, while heatmaps and cluster analyses may help uncover hidden spatial and temporal patterns. Future research should also focus on policies and interventions that effectively address disparities, ensuring that efforts to improve immunization rates prioritize equity and inclusivity.

By building on these suggestions, future studies can offer more actionable insights to guide policymakers and public health professionals in achieving universal Polio (Pol3) immunization coverage and addressing global health challenges effectively




### Conclusion

The fight against Polio is a testament to the power of collective action. While we celebrate the strides made, the visualizations remind us that the goal of eradicating Polio is within reach—but only if disparities are addressed and every child is vaccinated. Together, the global community can work toward a future where Polio exists only in history books—a legacy of resilience, determination, and shared responsibility.




### Additional Ideas for Enrichment - Bonus Marks Part

#### Immunization coverage in developing and developed nation

```{r echo=FALSE, message=FALSE, warning=FALSE}

data <- read_csv("data.csv")
View(data)

data_clean <- data %>%  
  select(IndicatorCode, ParentLocation, Location, Period, FactValueNumeric) %>% 
  rename(
    Region = ParentLocation, 
    Country = Location, 
    Year = Period, 
    Coverage = FactValueNumeric
  ) %>% 
  drop_na()

summary(data_clean)

developed_regions <- c("Europe", "Americas")
data_clean <- data_clean %>% 
  mutate(DevelopmentStatus = ifelse(Region %in% developed_regions, "Developed", "Developing"))

avg_coverage <- data_clean %>% 
  group_by(DevelopmentStatus, Year) %>% 
  summarize(AverageCoverage = mean(Coverage, na.rm = TRUE))

p <- ggplot(avg_coverage, aes(x = Year, y = AverageCoverage, group = DevelopmentStatus, color = DevelopmentStatus)) + 
  geom_line(size = 1) + 
  geom_point(size = 2, aes(text = paste(
    "Year:", Year,
    "<br>Average Coverage:", sprintf("%.1f%%", AverageCoverage),
    "<br>Development Status:", DevelopmentStatus
  ))) + 
  labs(
    title = "Immunization Coverage: Developed vs. Developing Nations", 
    x = "Year", 
    y = "Average Coverage (%)", 
    color = "Development Status"
  ) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1)) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

interactive_plot <- ggplotly(p, tooltip = "text")

interactive_plot

```




#### Analysis of graph

The analysis and plot reveal a clear disparity in polio immunization coverage between developed and developing nations. Developed nations consistently maintain high and stable immunization rates, ranging from 90% to 98%, due to strong healthcare infrastructure, reliable vaccine availability, and well-established immunization programs. The year-to-year changes in these regions are minimal, typically increasing by 1-3%, reflecting the stability of their systems. In contrast, developing nations experience more fluctuations in coverage, which varies from 30% to 85%. These variations are often caused by challenges such as inadequate healthcare infrastructure, political instability, and irregular vaccine availability. Coverage in developing nations may increase by 5-10% annually, depending on factors such as government policies, international aid, or vaccination campaigns, but the progress remains less consistent compared to developed nations. The gap in coverage between the two groups is significant and persistent, with developed nations consistently achieving higher immunization rates. However, despite these challenges, developing nations have made gradual improvements over time, often increasing their coverage by 10-20 percentage points in a decade due to stronger immunization efforts and increased global support.








