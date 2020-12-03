Data Exploration
================

Copy and pasted from ‘group\_eda’ repo on my own github.

``` r
library(tidyverse)
library(rvest)
library(patchwork)
knitr::opts_chunk$set(
  fig.width = 6, 
  fig.asp = .6, 
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot.continuous.colour = "viridis", 
  ggplot.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

Load AWSD and Uppsala data

``` r
aAWSD_df = 
  read_csv("Kailey_data/security_incidents_2020-11-25.csv") %>% 
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Country = col_character(),
    ##   Region = col_character(),
    ##   District = col_character(),
    ##   City = col_character(),
    ##   `Means of attack` = col_character(),
    ##   `Attack context` = col_character(),
    ##   Location = col_character(),
    ##   `Actor type` = col_character(),
    ##   `Actor name` = col_character(),
    ##   Details = col_character(),
    ##   Verified = col_character(),
    ##   Source = col_character()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 3 parsing failures.
    ##  row       col               expected      actual                                            file
    ## 1693 Latitude  no trailing characters , 24.933643 'Kailey_data/security_incidents_2020-11-25.csv'
    ## 1693 Longitude no trailing characters )           'Kailey_data/security_incidents_2020-11-25.csv'
    ## 2804 Latitude  no trailing characters ,           'Kailey_data/security_incidents_2020-11-25.csv'

``` r
uppsala_df =
  read_csv("Kailey_data/ucdp-prio-acd-201.csv") %>% 
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   conflict_id = col_double(),
    ##   incompatibility = col_double(),
    ##   year = col_double(),
    ##   intensity_level = col_double(),
    ##   cumulative_intensity = col_double(),
    ##   type_of_conflict = col_double(),
    ##   start_date = col_date(format = ""),
    ##   start_prec = col_double(),
    ##   start_date2 = col_date(format = ""),
    ##   start_prec2 = col_double(),
    ##   ep_end = col_double(),
    ##   ep_end_date = col_date(format = ""),
    ##   ep_end_prec = col_logical(),
    ##   version = col_double()
    ## )
    ## See spec(...) for full column specifications.

Initial look: these two datasets have a possibility of being merged…

The other data sets have less compatible variables
