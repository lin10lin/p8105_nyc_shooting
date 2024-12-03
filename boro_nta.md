boro_nta
================
2024-12-03

load packages

``` r
library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ stringr   1.5.1
    ## ✔ forcats   1.0.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

load dataset

``` r
df_descriptive=read_csv("filtered_merged_dataset_sample.csv")
```

    ## Rows: 10000 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (8): BORO, PERP_AGE_GROUP, PERP_SEX, PERP_RACE, VIC_AGE_GROUP, VIC_SEX...
    ## dbl  (10): INCIDENT_KEY, PRECINCT, Latitude, Longitude, Number_poverty, Perc...
    ## date  (1): OCCUR_DATE
    ## time  (1): OCCUR_TIME
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Plot of the number of incidents in each borough for each year

``` r
# Summarize data: count the number of incidents by borough and year
incident_summary <- df_descriptive %>%
  group_by(BORO, Year) %>%
  summarise(Number_of_Incidents = n(), .groups = "drop") %>%
  # Ensure missing years and boroughs are included
  complete(BORO, Year = full_seq(min(df_descriptive$Year):max(df_descriptive$Year), 1), fill = list(Number_of_Incidents = 0))

# Create the bar plot
ggplot(incident_summary, aes(x = Year, y = Number_of_Incidents, fill = BORO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Number of Incidents in Each Borough by Year",
    x = "Year",
    y = "Number of Incidents",
    fill = "Borough"
  ) +
  scale_x_continuous(breaks = seq(min(df_descriptive$Year), max(df_descriptive$Year), by = 1)) +
  theme_minimal()
```

![](boro_nta_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
