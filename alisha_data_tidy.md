initial\_tidy\_and\_wrangle
================
Alisha Sarakki

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readxl)
library(p8105.datasets)
library(hexbin)
library(patchwork)
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

\#\#2018 SHCC Dataset

Import Data

``` r
shcc2018_df = 
  read_excel("data/2018-shcc-attacks-data.xlsx", skip = 1) %>% 
  janitor::clean_names() %>% view()
```

total number of hcws affected per country in 2018 (killed, gbc,
arrested, injured, kidnapped)

``` r
shcc2018_df %>% 
  group_by(number_country_name) %>% 
  mutate(tot_hcw_affected = 
           number_affected_healthworker_killed + 
           number_affected_healthworker_sgbv + 
           number_affected_healthworker_arrested + 
           number_affected_healthworker_injured + 
           number_affected_healthworker_kidnapped) %>% 
  count(tot_hcw_affected)
```

    ## # A tibble: 25 x 3
    ## # Groups:   number_country_name [24]
    ##    number_country_name tot_hcw_affected     n
    ##    <chr>                          <dbl> <int>
    ##  1 Afghanistan                       NA    98
    ##  2 Burkina Faso                      NA     7
    ##  3 Cameroon                          NA    14
    ##  4 CAR                               NA    47
    ##  5 DRC                               NA    24
    ##  6 Egypt                             NA     1
    ##  7 Ethiopia                          NA     1
    ##  8 Indonesia                         NA     2
    ##  9 Iraq                              NA    12
    ## 10 Libya                             NA    47
    ## # … with 15 more rows
