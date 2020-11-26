Section 1
================
Natalie Boychuk, E. Brennan Bollman, Emily Bamforth, Alisha Sarakki, and
Kailey Rishovd
11/26/2020

This document is a working draft of section 1 of our final report, which
will focus on global levels of violence against aid workers.

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
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
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)
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
url = "https://aidworkersecurity.org/incidents/search"
aidworker_html = read_html(url)

aidworker_df = 
  aidworker_html %>% 
  html_nodes(css = "table") %>%  
  first() %>% 
  html_table() %>% 
  as_tibble()

aidworker_df = 
distinct(aidworker_df) 

aidworker_df %>% 
  janitor::clean_names() %>% 
  ## need to rethink the strategy for this - a simple pivot longer doesn't work here, creates 6 duplicates for each observation because it doesn't recognize 0s 
  pivot_longer(un:other,
    names_to = "org_type", 
    values_to = "number_orgs_affected") %>% 
  select(-number_orgs_affected) %>%
  rename(year = year_sort_descending, -source, -verified)
```

    ## # A tibble: 18,018 x 28
    ##       id month   day  year country nationals_killed nationals_wound…
    ##    <int> <chr> <int> <int> <chr>              <int>            <int>
    ##  1    35 ""       NA  1997 ""                     1                0
    ##  2    35 ""       NA  1997 ""                     1                0
    ##  3    35 ""       NA  1997 ""                     1                0
    ##  4    35 ""       NA  1997 ""                     1                0
    ##  5    35 ""       NA  1997 ""                     1                0
    ##  6    35 ""       NA  1997 ""                     1                0
    ##  7     1 "Jan"    NA  1997 "Cambo…                1                0
    ##  8     1 "Jan"    NA  1997 "Cambo…                1                0
    ##  9     1 "Jan"    NA  1997 "Cambo…                1                0
    ## 10     1 "Jan"    NA  1997 "Cambo…                1                0
    ## # … with 18,008 more rows, and 21 more variables: nationals_kidnapped <int>,
    ## #   total_national_staff <int>, internationals_killed <int>,
    ## #   internationals_wounded <int>, internationals_kidnapped <int>,
    ## #   total_international_staff <int>, total_victims <int>, gender_male <int>,
    ## #   gender_female <int>, gender_unknown <int>, means_of_attack <chr>,
    ## #   attack_context <chr>, location <chr>, latitude <chr>, longitude <chr>,
    ## #   actor_type <chr>, actor_name <chr>, details <chr>, verified <chr>,
    ## #   source <chr>, org_type <chr>
