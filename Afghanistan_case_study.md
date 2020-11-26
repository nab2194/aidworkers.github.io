Afghanistan Case Study
================

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

Aidworker dataset - cleaning and tailoring

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

afghan_aidw_df = 
aidworker_df %>% 
  filter(country == "Afghanistan") %>% 
  filter(year %in% c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

# tried to write a function, didn't work. Tried to group by and summarize, didn't work. Tried to mutate and create a new column for totals but that somehow didn't work either. Also tried to recode the columns, and you guessed it - that didn't work. Had to do it like this, but it's not even remotely efficient, so if someone has a better way, please feel free to overwrite this. 

afghan_aidw_df %>% 
   filter(year %in% c("2008")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 3 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   150
    ## 2                     1    24
    ## 3                     3     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2008")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 5 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0    78
    ## 2                1    78
    ## 3                2    12
    ## 4                3     6
    ## 5                5     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2009")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 3 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   144
    ## 2                     1     6
    ## 3                     3     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2009")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 4 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0    66
    ## 2                1    78
    ## 3                3     6
    ## 4                4     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2010")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 3 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   330
    ## 2                     1     6
    ## 3                     8     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2010")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 4 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0   228
    ## 2                1    84
    ## 3                2    24
    ## 4                3     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2011")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 2 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   300
    ## 2                     2     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2011")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 5 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0   192
    ## 2                1    84
    ## 3                2    12
    ## 4                3     6
    ## 5                4    12

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2012")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 2 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   330
    ## 2                     1     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2012")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 2 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0   276
    ## 2                1    60

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2013")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 2 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   474
    ## 2                     1    12

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2013")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 7 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0   318
    ## 2                1   126
    ## 3                2    12
    ## 4                3    12
    ## 5                4     6
    ## 6                5     6
    ## 7                6     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2014")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 3 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   294
    ## 2                     1    18
    ## 3                     2    12

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2014")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 6 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0   216
    ## 2                1    72
    ## 3                2    18
    ## 4                3     6
    ## 5                6     6
    ## 6                8     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2015")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 1 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   162

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2015")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 7 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0    90
    ## 2                1    36
    ## 3                2    12
    ## 4                3     6
    ## 5                5     6
    ## 6                9     6
    ## 7               14     6

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2016")) %>% 
  count(internationals_killed)
```

    ## # A tibble: 1 x 2
    ##   internationals_killed     n
    ##                   <dbl> <int>
    ## 1                     0   150

``` r
afghan_aidw_df %>% 
   filter(year %in% c("2016")) %>% 
  count(nationals_killed)
```

    ## # A tibble: 5 x 2
    ##   nationals_killed     n
    ##              <dbl> <int>
    ## 1                0   102
    ## 2                1    30
    ## 3                2     6
    ## 4                3     6
    ## 5                4     6
