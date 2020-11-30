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

    ## ── Attaching packages ─────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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

## Data import and tidying

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

aidworker_tidy_df =
  aidworker_df %>% 
  janitor::clean_names() %>% 
  mutate(intl_org_affected = 
           case_when(
             un != 0 ~ "yes",
             ingo != 0 ~ "yes",
             icrc != 0 ~ "yes",
             ifrc != 0 ~ "yes",
             other != 0 ~ "yes",
             lngo_and_nrcs != 0 ~ "no"
           ),
         intl_org_affected = as.factor(intl_org_affected)) %>% 
  rename(year = year_sort_descending) %>% 
  select(-source, -verified) %>% 
  relocate(id, month, day, year, country, intl_org_affected)
```

Brennan notes:

\*why did we use distinct here?

  - I think this `case_when` approach works to identify whether an
    international org was affected, instead of the pivot idea.
    Otherwise, I agree with Natalie that there’s no way to do it without
    making 6 copies of each incident. The existing data already seems to
    be the clearest way to present the number of people from each type
    of org affected, and we can plot that.

  - if happy with cleaning, we can rename the `aidworker_tidy_df` as
    just `aidworker_df`

## EDA of Attack patterns: International vs National

#### Look at frequency of attacks on various types of organizations

``` r
aidworker_tidy_df %>% 
  ggplot(aes(x = intl_org_affected)) + 
  geom_bar()
```

<img src="section-1_files/figure-gfm/unnamed-chunk-1-1.png" width="90%" />
