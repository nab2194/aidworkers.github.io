Afghanistan Case Study
================

``` r
library(tidyverse)
library(readxl)
library(p8105.datasets)
library(hexbin)
library(patchwork)
library(leaflet)
library(lubridate)
library(xml2)
library(rvest)
library(plotly)

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

\#\#Explain

Why we chose Afghanistan… Which datasets are being used…

Using the years 2008-2016 because…. and supply code

Aidworker dataset - cleaning and tailoring

``` r
#Kailey notes: from section 1 cleaning and filtered for afghanistan/year

url = "https://aidworkersecurity.org/incidents/search"
aidworker_html = read_html(url)

aidworker_df = 
  aidworker_html %>% 
  html_nodes(css = "table") %>%  
  first() %>% 
  html_table() %>% 
  as_tibble()

afghan_aidworker_df =
  aidworker_df %>%
  janitor::clean_names() %>% 
  select(-source, -verified) %>% 
  rename(year = year_sort_descending) %>% 
  mutate(intl_org_affected = 
           case_when(
             un != 0 ~ "yes",
             ingo != 0 ~ "yes",
             icrc != 0 ~ "yes",
             ifrc != 0 ~ "yes",
             other != 0 ~ "yes",
             lngo_and_nrcs != 0 ~ "no"),
         intl_org_affected = as.factor(intl_org_affected)) %>% 
  relocate(id, month, day, year, country, intl_org_affected) %>% 
  filter(country == "Afghanistan") %>% 
  filter(year %in% c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))
```

## Kailey notes: I found it\! This pivot longer is making the duplicates occur when added to the code chunk above.. also, this date code is not working for me\!

pivot\_longer( un:other, names\_to = “org\_type”, values\_to =
“number\_orgs\_affected” ) %\>% select(-number\_orgs\_affected)

aidworker\_df$date \<- as.Date(with(afghan\_aidworker\_df, paste(year,
month, day, sep = “-”)), “%Y-%m-%d”)

## Starting EDA and vis

Internationals vs. nationals killed (Not in the to-do list but has an
interesting spikes and dips; check historical context?)

``` r
#Kailey notes: FINALLY figured this out. got rid of all duplicates and strange number counts.

afghan_aidworker_international_df =
  afghan_aidworker_df %>% 
  group_by(year) %>% 
  summarise(sum(internationals_killed)) %>% 
  mutate(
    internationals_killed_tot = `sum(internationals_killed)`
  ) %>% 
  select(year, internationals_killed_tot)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
afghan_aidworker_national_df = 
  afghan_aidworker_df %>% 
  group_by(year) %>%
  summarise(sum(nationals_killed)) %>% 
  mutate(
    nationals_killed_tot = `sum(nationals_killed)`
  ) %>% 
  select(year, nationals_killed_tot)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
afghan_aidworker_df_new = 
  left_join(afghan_aidworker_international_df, afghan_aidworker_national_df, by = "year") 


afghan_aidworker_df_new %>% 
     ggplot(aes(x = year, y = internationals_killed_tot, col = "International")) +
   geom_line() +
  geom_line(aes(x = year, y = nationals_killed_tot, col = "National")) +
  labs(
    title = "Number of Aidworkers Killed By Origin (National vs. International)",
    x = "Year",
    y = "Number Killed"
  ) 
```

<img src="Aghanistan_case_study_clean_files/figure-gfm/unnamed-chunk-1-1.png" width="90%" />

More visualizations using aidworker df

``` r
# Kailey notes: Not sure if this visualization is 100% complete in terms of making sense/usefulness but I think something like this could be interesting. Subplotted two interactive plots. You can see the means of attack next to the attack context. For example, the aerial bombardment spike in 2015 is tied to combat/crossfire attack type (could look at historical context here). Can also see that in 2009 there were 16 kidnappings due to an ambush. Open to thoughts on this but thought something like this could be cool. 


means_attack_p = 
afghan_aidworker_df %>%
  plot_ly(
    x = ~year, y = ~total_victims, color = ~means_of_attack, 
    type = "scatter") %>% 
    layout(
    title = "Victims per year by attack type")

attack_context_p =
afghan_aidworker_df %>%
  plot_ly(
    x = ~year, y = ~total_victims, color = ~attack_context, 
    type = "scatter") %>%
    layout(
    title = "Victims per year by attack context")

subplot(means_attack_p, attack_context_p)
```

    ## No scatter mode specifed:
    ##   Setting the mode to markers
    ##   Read more about this attribute -> https://plot.ly/r/reference/#scatter-mode

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors
    
    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors

    ## No scatter mode specifed:
    ##   Setting the mode to markers
    ##   Read more about this attribute -> https://plot.ly/r/reference/#scatter-mode

<img src="Aghanistan_case_study_clean_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
afghan_aidworker_df %>% 
     ggplot(aes(x = location, fill = means_of_attack)) +
   geom_bar() +
  labs(
    title = "Attacks per location and by what means of attack",
    x = "Location"
  ) 
```

<img src="Aghanistan_case_study_clean_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

From section 1 vis:

looking at frequency of attacks on staff of various organizations yes/no

``` r
afghan_aidworker_df %>% 
  ggplot(aes(x = intl_org_affected)) + 
  geom_bar()
```

<img src="Aghanistan_case_study_clean_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

International vs National staff attacks?

``` r
af_pct_intl_ntl = 
afghan_aidworker_df %>%
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims),
            pct_intl = (tot_intl/tot_both)*100,
            pct_national = (tot_national/tot_both)*100) %>% 
  ggplot(aes(x = year, y = pct_national, col = 3)) + 
  geom_line() +
   geom_line(aes(x = year, y = pct_intl, col = 2)) + 
   labs(
    x = "Year",
    y = "% Attacked"
   )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
## Would still need to add labels for understanding
```

## Kailey notes: need to scrape uppsala data from web instead of loading in by .csv

# Filter Uppsala dataset for just Afghanistan to examine overall violence overtime alongside violence against aidworkers

# Deaths among all civilians vs. all aidworkers side by side

– are these different questions?

Uppsala data upload and cleaning/tailoring (downloaded data for
Afghanistan only)

``` r
#Looking at deaths among all civilians verses all aidworkers side by side

#Aidworkers: 
aidworker_deaths_p = 
afghan_aidworker_df %>% 
  group_by(year) %>% 
  summarise(sum(total_victims)) %>%
  mutate(
    tot_victims = `sum(total_victims)`
  ) %>% 
  select(-`sum(total_victims)`) %>% 
  ggplot(aes(x = year, y = tot_victims, col = "Aidworkers")) +
   geom_line() +
  labs(
    title = "Total # of Aidworkers Killed per year",
    x = "Year",
    y = "Number Killed"
  ) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
#All civilians:
afghan_upp_df = 
read_csv("./data/afghanistan_uppsala.csv") %>%
  janitor::clean_names() %>% 
  filter(year %in% c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")) %>% 
  select(year, deaths_civilians) %>% 
  group_by(year) %>% 
  summarise(sum(deaths_civilians)) %>% 
  mutate(
    deaths_civilians_tot = `sum(deaths_civilians)`
  ) %>% 
  select(-`sum(deaths_civilians)`)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   relid = col_character(),
    ##   active_year = col_logical(),
    ##   code_status = col_character(),
    ##   conflict_name = col_character(),
    ##   dyad_name = col_character(),
    ##   side_a = col_character(),
    ##   side_b = col_character(),
    ##   source_article = col_character(),
    ##   source_office = col_character(),
    ##   source_date = col_character(),
    ##   source_headline = col_character(),
    ##   source_original = col_character(),
    ##   where_coordinates = col_character(),
    ##   where_description = col_character(),
    ##   adm_1 = col_character(),
    ##   adm_2 = col_character(),
    ##   geom_wkt = col_character(),
    ##   country = col_character(),
    ##   region = col_character(),
    ##   date_start = col_character()
    ##   # ... with 1 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 33612 parsing failures.
    ## row col   expected     actual                             file
    ##   1  -- 47 columns 48 columns './data/afghanistan_uppsala.csv'
    ##   2  -- 47 columns 48 columns './data/afghanistan_uppsala.csv'
    ##   3  -- 47 columns 48 columns './data/afghanistan_uppsala.csv'
    ##   4  -- 47 columns 48 columns './data/afghanistan_uppsala.csv'
    ##   5  -- 47 columns 48 columns './data/afghanistan_uppsala.csv'
    ## ... ... .......... .......... ................................
    ## See problems(...) for more details.

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
civilian_deaths_p =
afghan_upp_df %>% 
  ggplot(aes(x = year, y = deaths_civilians_tot, col = "Civilian")) +
   geom_line() +
  labs(
    title = "Total # of Civilians Killed per year",
    x = "Year",
    y = "Number Killed"
  ) 

aidworker_deaths_p + civilian_deaths_p
```

<img src="Aghanistan_case_study_clean_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
#Kailey notes: Total victims = all aidworkers killed, right? Do these make sense to go side by side or is there no 'side by side' analysis here? 
```

# Kailey notes: Are we using SHCC or complete scrap?

# Discuss confounders (U.S. administration changes? Troop levels in country? Natural disastor?)
