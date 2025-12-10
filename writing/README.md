Group Project Overview
================
2025-11-17

## Project Overview

This dashboard was created as a final group project for ERHS 535. It allows users to explore:    

- **All National Parks**: Historical visitation trends (1904–2016) mapped against state population and gas prices.    
- **Intermountain Region Focus**: Air quality conditions at IM region parks using EPA AQI data.    
- **Park-level Analysis**: Individual park visitation patterns over time.     


## Contributors

- [Lina] - All National Parks tab
- [Ely] - Intermountain Region visitation plot
- [Eli] - AQI section (map, table, data cleaning)
- [Esther] - Denali tab

## Running Locally

1. Clone the repository:    
   ```bash
   git clone https://github.com/linarettig/r_finalproject.git
   cd r_finalproject
   ```    

2. Open `dashboard.Rmd` in RStudio    

3. Click **Run Document** or use:
   ```r
   rmarkdown::run("dashboard.Rmd")
   ```

## Dependencies

```r
install.packages(c(
  "flexdashboard",
  "shiny",
  "plotly",
  "leaflet",
  "tidyverse",
  "sf",
  "scales"
))
```

## Dashboard Tabs

### Tab 1: All National Parks
- **Interactive Map**: Circle markers sized by visitor count     
- **Year Slider**: Animate through 1929–2015      
- **Popup Info**: Park name, state, visitors, state population, gas price     

### Tab 2: Intermountain Region
- **Park Dropdown**: Select individual IM region parks.
- **Visitation Plot**: Time series of annual visitors by park, uses the dropdown previously mentioned.
- **AQI Map**: Parks colored by air quality category (Good/Moderate/Unhealthy)    
- **Summary Table**: State-level AQI metrics and park visitor totals    

### Tab 3: Denali
- (In development)

## Known Issues & Data Notes

### AQI Data Correction
The raw `Median AQI` column in the EPA data represents the **sum of median AQI values across all reporting counties**, not the true state median. We corrected this by dividing by `Cnty_Rpt` (number of reporting counties):    

```r
median_aqi = raw_median_aqi / counties_reporting
``` 


## License

This project is for educational purposes as part of ERHS 535 at Colorado State University.
