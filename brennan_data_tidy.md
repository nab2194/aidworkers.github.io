Reading and tidying data
================
E. Brennan Bollman
20-11-22

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

knitr::opts_chunk$set(
  fig.width = 10,
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_bw() + theme(legend.position = "bottom")) 

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d()
scale_fill_discrete = scale_fill_viridis_d()
```

## Aid Worker Security Database

URL: <https://aidworkersecurity.org/incidents> Select: ‘all’ in all
fields to obtain the most complete form of the data Download csv

Note: not sure if we should try to scrape from web to make this more
reproducible.

### Read in data

``` r
aidworker_df = 
  read_csv("data/security_incidents_2020-11-23.csv") %>% 
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
    ##  row       col               expected      actual                                     file
    ## 1693 Latitude  no trailing characters , 24.933643 'data/security_incidents_2020-11-23.csv'
    ## 1693 Longitude no trailing characters )           'data/security_incidents_2020-11-23.csv'
    ## 2804 Latitude  no trailing characters ,           'data/security_incidents_2020-11-23.csv'

I think this looks fairly tidy? Each incident\_id is row. There are lots
of variables, but I think mostly will be able to manipulate with
`mutate` and `group_by` in further operations on dataset. Lmk what you
all think.

Data dictionary: <https://aidworkersecurity.org/about>

### Quick EDA

Most deadly countries for aid workers.

``` r
aidworker_df %>%
  group_by(country) %>% 
  summarize(tot_affected_per_country = sum(total_affected, na.rm = TRUE)) %>% 
  mutate(rank = min_rank(desc(tot_affected_per_country))) %>% 
  filter(rank < 6) %>% 
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| country              | tot\_affected\_per\_country | rank |
| :------------------- | --------------------------: | ---: |
| Afghanistan          |                        1259 |    1 |
| Somalia              |                         515 |    3 |
| South Sudan          |                         581 |    2 |
| Sudan                |                         487 |    4 |
| Syrian Arab Republic |                         462 |    5 |

Means of attack

``` r
aidworker_df %>%
  count(means_of_attack) %>%
  mutate(rank = min_rank(desc(n))) %>% 
  arrange(rank) %>% 
  knitr::kable()
```

| means\_of\_attack   |   n | rank |
| :------------------ | --: | ---: |
| Shooting            | 868 |    1 |
| Kidnapping          | 623 |    2 |
| Bodily assault      | 520 |    3 |
| Unknown             | 413 |    4 |
| Aerial bombardment  | 150 |    5 |
| Kidnap-killing      |  89 |    6 |
| Shelling            |  82 |    7 |
| Vehicle-born IED    |  68 |    8 |
| Roadside IED        |  50 |    9 |
| Other Explosives    |  36 |   10 |
| Landmine            |  29 |   11 |
| Complex attack      |  26 |   12 |
| Rape/sexual assault |  26 |   12 |
| Body-borne IED      |  17 |   14 |

International workers affected

``` r
aidworker_df %>% 
  group_by(country) %>% 
  summarize(tot_intl_per_country = sum(total_internationals, na.rm = TRUE)) %>% 
  mutate(rank = min_rank(desc(tot_intl_per_country))) %>% 
  filter(rank < 6) %>% 
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| country     | tot\_intl\_per\_country | rank |
| :---------- | ----------------------: | ---: |
| Afghanistan |                     131 |    1 |
| Somalia     |                      90 |    2 |
| South Sudan |                      65 |    3 |
| Sudan       |                      54 |    4 |
| Yemen       |                      35 |    5 |

National workers affected

``` r
aidworker_df %>% 
  group_by(country) %>% 
  summarize(tot_natl_per_country = sum(total_nationals, na.rm = TRUE)) %>% 
  mutate(rank = min_rank(desc(tot_natl_per_country))) %>% 
  filter(rank < 6) %>% 
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| country              | tot\_natl\_per\_country | rank |
| :------------------- | ----------------------: | ---: |
| Afghanistan          |                    1128 |    1 |
| Somalia              |                     425 |    5 |
| South Sudan          |                     516 |    2 |
| Sudan                |                     433 |    3 |
| Syrian Arab Republic |                     433 |    3 |
