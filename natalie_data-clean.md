first cleaning attempt
================
Natalie Boychuk
10/18/2020

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ──────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(p8105.datasets)
library(hexbin)
library(patchwork)
library(leaflet)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
devtools::install_github("benmarwick/wordcountaddin",  type = "source", dependencies = TRUE)
```

    ## Skipping install of 'wordcountaddin' from a github remote, the SHA1 (8c063135) has not changed since last install.
    ##   Use `force = TRUE` to force installation

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
theme_set(theme_minimal() + theme(legend.position = "bottom"))
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
aidworker_df = 
read_csv("./data/security_incidents_2020-11-25.csv") %>%
  janitor::clean_names() %>% 
  pivot_longer(
    un:other,
    names_to = "org_type", 
    values_to = "number_orgs_affected"
  ) %>% 
  select(-number_orgs_affected)
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
    ##  row       col               expected      actual                                       file
    ## 1693 Latitude  no trailing characters , 24.933643 './data/security_incidents_2020-11-25.csv'
    ## 1693 Longitude no trailing characters )           './data/security_incidents_2020-11-25.csv'
    ## 2804 Latitude  no trailing characters ,           './data/security_incidents_2020-11-25.csv'

``` r
aidworker_df$date <- as.Date(with(aidworker_df, paste(year, month, day, sep = "-")), "%Y-%m-%d")

aidworker_df %>% 
  ggplot(aes(x = date, y = total_affected)) + 
  geom_point(aes(color = country))
```

    ## Warning: Removed 1878 rows containing missing values (geom_point).

<img src="natalie_data-clean_files/figure-gfm/basic cleaning and exploring-1.png" width="90%" />
