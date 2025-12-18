# FlexDashboard: Exploring National Park Visitation and Air Quality Patterns in the Intermountain Region

## Project Overview
This repository contains the final group project for **ERHS 535**, which explores long-term trends in U.S. National Park visitation and their relationship with air quality patterns, with a focused analysis on the **Intermountain Region**. Using an interactive **FlexDashboard built in R**, we integrate visitation data with environmental and socioeconomic indicators to examine how air quality conditions intersect with park use and regional trends.

---

## Objectives
The primary goals of this project are to:

- Visualize long-term trends in National Park visitation across the United States  
- Explore visitation patterns within the Intermountain Region  
- Examine air quality conditions (AQI) near National Parks and how they vary over time  
- Demonstrate the use of interactive data visualization tools in R  

---

## Data Sources
The dashboard integrates multiple publicly available datasets, including:

- **TidyTuesday National Parks Visitation Dataset** (1904–2016)  
- **U.S. Environmental Protection Agency (EPA)** Air Quality Index (AQI) data (1980–2022)  
- **U.S. Census Bureau** population data  
- **U.S. Energy Information Administration (EIA)** annual average gasoline prices  
- Aggregated and processed datasets from **Kaggle**  

---

## Dashboard Structure
The FlexDashboard is organized into three main panes:

### **Pane 1: All National Parks**
- Interactive **Leaflet map** showing park visitation by year  
- **Dynamic marker sizing** based on visitor counts  
- **Year slider** to animate visitation trends over time  
- Pop-up details including park name, visitor counts, state population, and gas prices  

### **Pane 2: Intermountain Region**
- Bar charts of visitation across Intermountain National Parks  
- **State-level AQI maps** and summary tables  
- Exploration of how air quality conditions vary across parks and states  
- Interactive filtering by year  

### **Pane 3: Denali National Park**
- Detailed **time-series visualization** of visitation trends  
- Interactive **Plotly** chart highlighting long-term changes  

---

## Methods & Tools
This project was developed using the following tools and packages:

- **R** and **RStudio**  
- **flexdashboard** for layout and structure  
- **shiny** for interactivity  
- **leaflet** and **leafletProxy()** for efficient spatial visualization  
- **ggplot2** and **plotly** for dynamic charts  
- **tidyverse** for data cleaning and transformation  
- **sf** and **tigris** for spatial data handling  

---

## Key Files
Key files and directories in this repository include:

- **finalproject_flex.Rmd** – Main FlexDashboard file  
- **EGA explore.R** – Exploratory data analysis  
- **Park visitation by year.Rmd** – Supporting visualizations  
- **/data/** – Cleaned datasets used in the project  
- **/writing/** – Project documentation and notes  
- **r_finalproject.Rproj** – RStudio project file  

---

## Key Takeaways
- Interactive dashboards enable intuitive exploration of complex, long-term datasets  
- Early data validation and consistent data sources are critical for successful integration  
- Performance optimizations (e.g., `leafletProxy()`) significantly improve user experience  
- Air quality conditions vary across regions and may influence park visitation patterns  

---

## Contributors
- **Lina Rettig**  
- **Ely Anneser**  
- **Esther Alorkpa**  
- **Eliud Rivas Hernandez**  

---

## Course Information
**ERHS 535 – Environmental & Radiological Health Sciences**  
Final Group Project
