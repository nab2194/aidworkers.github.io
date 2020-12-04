Section 1
================
Natalie Boychuk, E. Brennan Bollman, Emily Bamforth, Alisha Sarakki, and
Kailey Rishovd
11/26/2020

This document is a working draft of section 1 of our final report, which
will focus on global levels of violence against aid workers.

``` r
library(tidyverse)
library(dplyr)
library(patchwork)
library(leaflet)
library(lubridate)
library(rvest)
library(httr)
library(flexdashboard)
library(plotly)
library(viridis)
library(stringr)
library(maps)
library(corrplot)


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
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
    ) %>% 
  relocate(id, month, day, year, country, intl_org_affected)
```

    ## Warning: Problem with `mutate()` input `latitude`.
    ## ℹ NAs introduced by coercion
    ## ℹ Input `latitude` is `as.numeric(latitude)`.

    ## Warning in mask$eval_all_mutate(dots[[i]]): NAs introduced by coercion

    ## Warning: Problem with `mutate()` input `longitude`.
    ## ℹ NAs introduced by coercion
    ## ℹ Input `longitude` is `as.numeric(longitude)`.

    ## Warning in mask$eval_all_mutate(dots[[i]]): NAs introduced by coercion

``` r
## This is a function that should turn strings with empty spaces into NA 
empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) 
    ifelse(as.character(x)!="", x, NA)
}

aidworker_df = 
aidworker_df %>% mutate_each(funs(empty_as_na)) 

## This is one last tidying thing to remove the "total" row in the bottom of our df, which might be skewing results: 

aidworker_df =
  aidworker_df %>% 
  filter(country != "Total") %>% view()

## This removed 7 rows rather than the 1 I intended - upon inspection it seems to have also removed NAs in the country variable. I'm ok with that since the # of NAs is so small and we'd have to drop them from our analysis anyways but open to thoughts. 

##This next step is to create a variable that groups attack types together for simplicity. For our purposes, we aren't interested in the difference between roadside IED and vehicle-born IED, etc. 

## This is extremely clunky. If anyone has an idea for a function to fix this I would greatly appreciate it!! 

aidworker_df %>% 
distinct(means_of_attack)
```

    ## # A tibble: 14 x 1
    ##    means_of_attack    
    ##    <chr>              
    ##  1 Unknown            
    ##  2 Shooting           
    ##  3 Kidnapping         
    ##  4 Kidnap-killing     
    ##  5 Aerial bombardment 
    ##  6 Landmine           
    ##  7 Shelling           
    ##  8 Body-borne IED     
    ##  9 Bodily assault     
    ## 10 Roadside IED       
    ## 11 Vehicle-born IED   
    ## 12 Other Explosives   
    ## 13 Rape/sexual assault
    ## 14 Complex attack

``` r
aidworker_df = 
  mutate(aidworker_df, 
         attack_abr = case_when(
           means_of_attack == "Kidnap-killing" ~ "Kidnapping",
           means_of_attack == "Kidnapping" ~ "Kidnapping",
           means_of_attack == "Body-borne IED" ~ "IED", 
           means_of_attack == "Vehicle-born IED" ~ "IED", 
           means_of_attack == "Roadside IED" ~ "IED",
           means_of_attack == "Landmine" ~ "Explosives", 
           means_of_attack == "Shelling" ~ "Explosives",
           means_of_attack == "Other Explosives" ~ "Explosives",
           means_of_attack == "Aerial bombardment" ~ "Explosives",
           means_of_attack == "Rape/sexual assault" ~ "Rape/sexual assault",
           means_of_attack == "Complex attack" ~ "Complex attack",
           means_of_attack == "Shooting" ~ "Shooting",
           means_of_attack == "Unknown" ~ "Unknown", 
           means_of_attack == "Bodily assault" ~ "Bodily assault"
           ))

aidworker_df %>% 
  distinct(attack_abr)
```

    ## # A tibble: 8 x 1
    ##   attack_abr         
    ##   <chr>              
    ## 1 Unknown            
    ## 2 Shooting           
    ## 3 Kidnapping         
    ## 4 Explosives         
    ## 5 IED                
    ## 6 Bodily assault     
    ## 7 Rape/sexual assault
    ## 8 Complex attack

``` r
## I am creating a variable based on the below table with the 10 most dangerous countries to be an aid worker that we can use a predictor in our regression model
```

## EDA on aidworker\_df

### Most dangerous countries to be an aid worker

``` r
danger_countries = 
aidworker_df %>%
  filter(country != "Total") %>% 
  group_by(country) %>% 
  summarize(tot_affected_per_country = sum(total_victims, na.rm = TRUE)) %>% 
  mutate(rank = min_rank(desc(tot_affected_per_country))) %>% 
  filter(rank < 11) %>% 
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

### Look at frequency of attacks on staff of various types of organizations

``` r
aidworker_df %>% 
  ggplot(aes(x = intl_org_affected)) + 
  geom_bar()
```

<img src="section-1_files/figure-gfm/tbd_on_type_of_org_affected-1.png" width="90%" />

Brennan: I’m stuck on this part. Natalie - you were thinking about it
conceptually a lot, any ideas?

### International vs National staff attacks

``` r
aidworker_df %>%
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims),
            pct_intl = (tot_intl/tot_both)*100,
            pct_national = (tot_national/tot_both)*100) %>%
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| year | tot\_national | tot\_intl | tot\_both | pct\_intl | pct\_national |
| ---: | ------------: | --------: | --------: | --------: | ------------: |
| 1997 |            44 |        29 |        73 | 39.726027 |      60.27397 |
| 1998 |            41 |        21 |        62 | 33.870968 |      66.12903 |
| 1999 |            42 |        24 |        66 | 36.363636 |      63.63636 |
| 2000 |            67 |        20 |        87 | 22.988506 |      77.01149 |
| 2001 |            60 |        26 |        86 | 30.232558 |      69.76744 |
| 2002 |            68 |        17 |        85 | 20.000000 |      80.00000 |
| 2003 |           117 |        25 |       142 | 17.605634 |      82.39437 |
| 2004 |           101 |        24 |       125 | 19.200000 |      80.80000 |
| 2005 |           158 |        14 |       172 |  8.139535 |      91.86047 |
| 2006 |           214 |        26 |       240 | 10.833333 |      89.16667 |
| 2007 |           186 |        35 |       221 | 15.837104 |      84.16290 |
| 2008 |           227 |        51 |       278 | 18.345324 |      81.65468 |
| 2009 |           221 |        74 |       295 | 25.084746 |      74.91525 |
| 2010 |           209 |        41 |       250 | 16.400000 |      83.60000 |
| 2011 |           282 |        29 |       311 |  9.324759 |      90.67524 |
| 2012 |           228 |        49 |       277 | 17.689531 |      82.31047 |
| 2013 |           415 |        60 |       475 | 12.631579 |      87.36842 |
| 2014 |           300 |        32 |       332 |  9.638554 |      90.36145 |
| 2015 |           260 |        29 |       289 | 10.034602 |      89.96540 |
| 2016 |           252 |        43 |       295 | 14.576271 |      85.42373 |
| 2017 |           285 |        28 |       313 |  8.945687 |      91.05431 |
| 2018 |           379 |        29 |       408 |  7.107843 |      92.89216 |
| 2019 |           456 |        27 |       483 |  5.590062 |      94.40994 |
| 2020 |           264 |        17 |       281 |  6.049822 |      93.95018 |

``` r
## If look at last row of data, appears "NA" is the total
## Natalie note: I'm only seeing NA in the year column, not anywhere else - looks like the sums worked fine. I think R just knows not to add up the years. 

## Plot of percentage of nationals affected by attacks over time 
pct_ntl =
aidworker_df %>%
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims),
            pct_intl = (tot_intl/tot_both)*100,
            pct_national = (tot_national/tot_both)*100) %>% 
  ggplot(aes(x = year, y = pct_national)) + 
  geom_line() +
    labs(
     x = "Year",
     y = "% National"
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
## Plot of percentage of internationals affected by attacks over time 
pct_intl = 
aidworker_df %>%
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims),
            pct_intl = (tot_intl/tot_both)*100,
            pct_national = (tot_national/tot_both)*100) %>% 
  ggplot(aes(x = year, y = pct_intl)) + 
  geom_line() +
   labs(
    x = "Year",
    y = "% International"
   )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
pct_ntl + pct_intl +
  plot_annotation(title = "Percent of Aid Workers Attacked Who Were National vs. International")
```

<img src="section-1_files/figure-gfm/international_vs_national_staff_attacks-1.png" width="90%" />

Roughly this shows that proportions of nationals attacked has always
been higher than internationals, with increasing percentage of national
staff attacked over time.

``` r
aidworker_df %>% 
  drop_na(year) %>% 
  group_by(year) %>% 
   summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims)) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = tot_national, color = "National Staff")) + 
  geom_line(aes(y = tot_intl, color = "International Staff")) + 
  labs(title = "Aid Worker Attacks over time",
       x = "Year",
       y = "Number of Aid Workers Attacked")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<img src="section-1_files/figure-gfm/intl_vs_natl_staff_attack_plot-1.png" width="90%" />

Number of aid worker attacks are increasing over time, especially among
national staff.

### Type of Attack

``` r
kidnap_plot = 
aidworker_df %>% 
  filter(means_of_attack == "Kidnapping") %>% 
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims)) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = tot_national, color = "National Staff")) + 
  geom_line(aes(y = tot_intl, color = "International Staff")) + 
  labs(title = "Aid Worker Kidnapping over time",
       x = "Year",
       y = "Number of Aid Workers Kidnapped")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
shoot_plot = 
aidworker_df %>% 
  filter(means_of_attack == "Shooting") %>% 
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims)) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = tot_national, color = "National Staff")) + 
  geom_line(aes(y = tot_intl, color = "International Staff")) + 
  labs(title = "Aid Worker Shooting over time",
       x = "Year",
       y = "Number of Aid Workers Shot")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

Kidnapping also increasing over time, particularly among national staff.
Could put this side-by-side the total attack line plot.

We can see that the same pattern is occurring with shootings among aid
workers. There has been a sharp decrease in shootings among
international staffers in the last 5-10 years. (Brennan - isn’t this
just stable?)

``` r
aidworker_df %>% 
  filter(means_of_attack == "Unknown") %>% 
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims)) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = tot_national, color = "National Staff")) + 
  geom_line(aes(y = tot_intl, color = "International Staff")) + 
  labs(title = "Aid Worker Attacks over time (unknown means)",
       x = "Year",
       y = "Number of Aid Workers Attacked via unknown means")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<img src="section-1_files/figure-gfm/unknown_attacks-1.png" width="90%" />

The above plot might explain part of the pattern we see above, as there
is a gradual decrease in attacks of unknown means among national staff.
This might be reflective of increases in reporting.

``` r
aidworker_df %>% 
  filter(means_of_attack == "Aerial bombardment") %>% 
  group_by(year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_both = sum(total_victims)) %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = tot_national, color = "National Staff")) + 
  geom_line(aes(y = tot_intl, color = "International Staff")) + 
  labs(title = "Aid Worker Aerial Bombardment over time",
       x = "Year",
       y = "Number of Aid Workers Attacked via Aerial Bombardment")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<img src="section-1_files/figure-gfm/aerial_bombardment-1.png" width="90%" />

Natalie: I think it would be cool to have a sort of dashboard of means
of attack with the changes over time. I’m going to try to make this more
interactive\!\!

#### Attempts to visualize type of attack over time

``` r
aidworker_df %>% 
  drop_na(year) %>% 
  group_by(year, means_of_attack) %>% 
  ggplot(aes(x = year, y = total_victims, color = means_of_attack)) + 
  geom_point()
```

<img src="section-1_files/figure-gfm/scatter_type_attack-1.png" width="90%" />

Above is not good.

Note from Emily: What if we tried the following instead of the above,
plotting by number of attacks, instead of by number of victims?
Following plot still needs work (remove unknowns, any that we could
combine?) but could be useful:

Note from Natalie: I like this, but I do think it’s really informative
to see the national/international differences. One option is making two
plots and placing them side by side via patchwork\! Or we could create
an option on Shiny where people can click to see national
vs. international vs. total.

``` r
aidworker_df %>% 
  drop_na(year) %>% 
  group_by(year) %>% 
    summarise(number_attack = n(), means_of_attack) %>% 
    as_tibble() %>% 
  group_by(means_of_attack) %>% 
  ggplot(aes(x = year, y = number_attack, color = means_of_attack)) + 
  geom_line() +
  labs(
    title = "Types of Attack Over Time",
    x = "Year",
    y = "Number of Attacks"
  )
```

    ## `summarise()` regrouping output by 'year' (override with `.groups` argument)

<img src="section-1_files/figure-gfm/plot_attack_type_over_time-1.png" width="90%" />

``` r
aidworker_df %>% 
  drop_na(year) %>% 
  group_by(year, means_of_attack) %>% 
  filter(year == 2019) %>% 
  ggplot(aes(x = means_of_attack, fill = means_of_attack)) + 
  geom_bar()
```

<img src="section-1_files/figure-gfm/bar_plot_of_types_of_attacks_for_both_ntl_and_intl_workers-1.png" width="90%" />

I like this bar chart, but I think ‘count’ is the number of incidents,
not the number of people affected. Would be really cool if this could be
a shiny plot where could change year in a drop down to see how this
changes over time? Or make a plotly so that you can hover over label to
see number of intl/natl affected by each type of attack?

I’m not sure how to make those things work though.

``` r
## This was a failed attempt at summarizing by worker type to see if there are differences in means of attack by international vs. national status 

#aidworker_df %>% 
  #select(id, year, total_national_staff, total_international_staff, means_of_attack) %>% 
  #as.numeric(c(total_international_staff, total_national_staff)) %>% 
  #group_by(means_of_attack) %>% 
  #summarize(across(total_national_staff:total_international_staff, count))
```

# Mapping the data…

Attempt at `leaflet` map with points for attacks. When mapping all the
data points, nothing appears on the map. Need to look into more. I’ve
tried filtering to one or a handful of countries (this works), sampling
the data (this sometimes works), clustering…

``` r
attack_map =
  aidworker_df %>%
  drop_na(longitude, latitude) %>%
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(
    lat = ~latitude,
    lng = ~longitude,
    clusterOptions = markerClusterOptions())
    
attack_map
```

<img src="section-1_files/figure-gfm/attack_map-1.png" width="90%" />

Some of the things I tried included:

Mapping data for one country (or even multiple countries) by using
`filter`. This will be useful for the Afghanistan case study. We can
utilize the following code for the case study portion of the project.
However, we have one outlier (in the Mediterranean) which shows up for
Afghanistan - likely a data entry issue, which we should resolve. Needed
to make a separate data frame with the filtered country data, in order
to pull variables for the popups (otherwise it plots all 3000 points,
but within the country you filtered by).

``` r
afghanistan_df =
  aidworker_df %>%
  drop_na(longitude, latitude) %>% 
  filter(country == "Afghanistan")
  
leaflet(afghanistan_df) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(lat = ~latitude, lng = ~longitude,
  popup = paste("Total Victims:", afghanistan_df$total_victims, "<br>", "Means of Attack:", afghanistan_df$means_of_attack, "<br>", "Year:", afghanistan_df$year, "<br>", "Country:", afghanistan_df$country), clusterOptions = markerClusterOptions())
```

<img src="section-1_files/figure-gfm/afghanistan_map-1.png" width="90%" />

Just some code to check the map. There are 560 attacks in Afghanistan in
which the location was known (from 1259 total, as shown above). That
works\!

``` r
aidworker_df %>% 
  filter(country == "Afghanistan") %>% 
  drop_na(latitude, longitude) %>% 
  n_distinct()
```

    ## [1] 560

Tried using `slice_sample` to see how many points we could plot on the
map. I got up to 1000, and then attempted 1100, which did not work, then
going back down to 1000, then even 500, did not work anymore. I thought
there could be an issue with plotting all 3000 points on the map, and
maybe there was a point where the code would break. However, because it
sometimes lets me pull 500, and sometimes 1000…something strange is
going on here… The following code should pull a sample.

``` r
samp_attack_map =
  aidworker_df %>%
  drop_na(longitude, latitude) %>%
  slice_sample(n = 500) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(
    lat = ~latitude,
    lng = ~longitude,
    clusterOptions = markerClusterOptions())

samp_attack_map
```

<img src="section-1_files/figure-gfm/samp_attack_map-1.png" width="90%" />

Hey team, here’s what happens if we map a random sample, but call values
to include in the popups. I think this is happening because we’re
referring back to the unfiltered dataset when saying what to include in
the popups. It’s capable of plotting all 3000 observations…but it’s
plotting the incidents in the wrong countries\!

``` r
  aidworker_df %>%
  drop_na(longitude, latitude) %>%
  slice_sample(n = 500) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(
    lat = ~latitude,
    lng = ~longitude,
    popup = paste("Total Victims:", aidworker_df$total_victims, "<br>", "Means of Attack:", aidworker_df$means_of_attack, "<br>", "Year:", aidworker_df$year, "<br>", "Country:", aidworker_df$country),
    clusterOptions = markerClusterOptions())
```

<img src="section-1_files/figure-gfm/bad_example_map-1.png" width="90%" />

After attending office hours, we discussed adpating the `leaflet` map to
show top 10 countries impacted, or a subset of years, as we could not
plot all 3000 points.

Note from making the following df and map…some of these country names
are recoded later in this doc to join with the map data for the static
ggplot global map. In our final project, when recoding is done, will
impact some of our code, such as the following code where we filter by
country.

Map still doesn’t work\!\!\!\!\! Will try again later.

``` r
danger_map_df =
  aidworker_df %>%
  drop_na(longitude, latitude) %>% 
  filter(country %in% c("Afghanistan", "Central African Republic", "DR Congo", "Iraq", "Pakistan", "Somalia", "South Sudan", "Sudan", "Syrian Arab Republic", "Yemen"))
  
leaflet(danger_map_df) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(lat = ~latitude, lng = ~longitude,
  popup = paste("Total Victims:", danger_map_df$total_victims, "<br>", "Means of Attack:", danger_map_df$means_of_attack, "<br>", "Year:", danger_map_df$year, "<br>", "Country:", danger_map_df$country), clusterOptions = markerClusterOptions())
```

<img src="section-1_files/figure-gfm/danger_map-1.png" width="90%" />

Map in Shiny? We want to map all of the global data, be able to filter
by at least year, and include info on attack types, number of victims,
national or international staff…

Shiny map does NOT need to be a leaflet (can drop lat/long) if we’re
able to group\_by and summarize the variables of interest by country and
then plot those.

i.e. in ideal map, would have drop down to select year, and then would
be able to hover over each country to see the total victims that year,
total national, total international, total of attack types…. and would
be AMAZING for that to be a heat map where colors darken for increased
total victims as you change year.

What we want is something like this (map in firearms mortality tab):
<https://eshea6.github.io/us_gun_violence.github.io/index.html>

``` r
global_map_df = 
  aidworker_df %>% 
  mutate(
    kidnapping = case_when(means_of_attack %in% c("Kidnapping", "Kidnap-killing") ~ 1),
    shooting = case_when(means_of_attack %in% c("Shooting") ~ 1),
    assault = case_when(means_of_attack %in% c("Bodily assault", "Rape/sexual assault") ~ 1),
    explosive = case_when(means_of_attack %in% c("Aerial bombardment", "Landmine", "Other Explosives", "Roadside IED",
                                                 "Shelling", "Vehicle-born IED") ~ 1)) %>% 
  drop_na(year) %>% 
  group_by(country, year) %>% 
  summarize(tot_national = sum(total_national_staff),
            tot_intl = sum(total_international_staff),
            tot_victims = sum(total_victims),
            tot_kidnappings = sum(kidnapping, na.rm = TRUE),
            tot_shootings = sum(shooting, na.rm = TRUE),
            tot_assault = sum(assault, na.rm = TRUE),
            tot_explosive = sum(explosive, na.rm = TRUE)) %>% 
  drop_na(country)
```

    ## `summarise()` regrouping output by 'country' (override with `.groups` argument)

This df collapses the means\_of\_attack variable into explosives,
shooting, kidnapping, and assault. It sums by country and year the
number of these attack events, and the number national / international /
total victims (total per country/year, not by attack type).

I think this is the df we want for our shiny map with the following
dropdowns:

  - year
  - attack type
  - victims (select total, international staff, national staff)

and ideally the “heatmap” would change for each\!

Practicing with some spatial data here in `ggplot` before moving toward
interactivity and `shiny`\! Thinking about how we will join the above
data frame, of attacks aggregated by country, with a shape file. I think
we’ll need a map with data that we can join our new data frame to.

``` r
map_world = map_data("world")

ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group))
```

<img src="section-1_files/figure-gfm/map_world-1.png" width="90%" />

Looking at `global_map_df` and `map_world`, in order to join them, I
think we will have to iterate and create a df / map for each year…

First, we need to compare country names between the dataframes to make
sure we’ll be able to join by `country` and `region`, and then update
any discrepancies.

``` r
x =
  global_map_df$country %>% 
  unique()

y =
  map_world$region %>% 
  unique()

x %in% y
```

    ##  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## [13]  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## [25]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## [37] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
    ## [49]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## [61]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## [73]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

``` r
x[which(!(x %in% y))]
```

    ## [1] "Chechnya"                         "Congo"                           
    ## [3] "Cote D'Ivoire"                    "DR Congo"                        
    ## [5] "Kashmir"                          "Libyan Arab Jamahiriya"          
    ## [7] "Occupied Palestinian Territories" "Syrian Arab Republic"

I now have a list of 9 countries that are in `global_map_df` but are not
in `map_world`. Need to manually update.

Dropped where `country` = NA from `global_map_df`; we can’t map them, so
if using this df just for the map, we can drop them. Now list of 8. Have
to manually review `region` in `map_world` to consider how these
countries might appear in the df.

Team, please let me know if countries should be renamed otherwise.

Main concern (\!\!\!) Chechnya and Kashmir are not on the map. For the
sake of the project, I think it may most straight forward for this
project to do the following and make clear notes on the final
deliverable. However, if you have an idea of how to better capture this
data in the map, please take a look\!

  - Kashmir has 1 total victim, so we can drop from the dataframe / map
    as long as we have a note in our dashboard.
  - Chechnya has 44 total victims over many years. Technically, it is a
    republic of Russia, so I suppose we can recode as Russia for now.
    Our dataset does not include any attacks in Russia. So everything
    that appears as Russia in our map, will actually be located in
    Chechnya. We can make note in our dashboard.

<!-- end list -->

``` r
map_world$region =
  recode(map_world$region,
         'Ivory Coast' = 'Cote D\'Ivoire',
         'Syria' = 'Syrian Arab Republic'
         )

global_map_df$country =
  recode(global_map_df$country,
         'Occupied Palestinian Territories' = 'Palestine',
         'DR Congo' = 'Democratic Republic of the Congo',
         'Congo' = 'Republic of Congo',
         'Libyan Arab Jamahiriya' = 'Libya',
         'Chechnya' = 'Russia'
         )

global_map_df =
  global_map_df %>% 
  filter(country != "Kashmir")

map_world =
  map_world %>% 
  rename(country = region)
```

Lastly, had to rename the country columns so that we can join.

Then we’ll try just one year and see if it works…

``` r
global_map_df_2019 =
  global_map_df %>% 
  filter(year == "2019")

joined_map_2019 =
  left_join(map_world, global_map_df_2019) %>% 
  mutate(polygon_fill = ifelse(is.na(tot_victims), F, T))
```

    ## Joining, by = "country"

``` r
ggplot() +
  geom_polygon(data = joined_map_2019, aes(x = long, y = lat, group = group, fill = polygon_fill))
```

<img src="section-1_files/figure-gfm/world_map_2019-1.png" width="90%" />

So we can join the two dataframes on the `country` column, but think we
can only do different dataframes year by year. I don’t love this. I feel
I have a better understanding of the data which will be helpful, but I’m
not sure this is leading us quite where we need to go. Going to move
onto shiny now.

\#\#Logistic Regression of Aid Worker Attacks

This is a histogram of the distribution of total aid worker attacks. As
you can see, it is not normally distributed. However, since we have a
sufficient n of observations, the Central Limit Theorum applies and we
should be able to apply a normal approximation to this data.

Our outcome variable is the probability if experiencing one of

``` r
aidworker_df %>% 
  group_by(attack_abr) %>% 
  summarize(total_attack = n()) %>% 
  knitr::kable()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| attack\_abr         | total\_attack |
| :------------------ | ------------: |
| Bodily assault      |           521 |
| Complex attack      |            26 |
| Explosives          |           297 |
| IED                 |           138 |
| Kidnapping          |           712 |
| Rape/sexual assault |            26 |
| Shooting            |           869 |
| Unknown             |           407 |

``` r
## I absolutely could not get this to work, not sure why - if someone has any ideas to get this to a barplot with x axis being categories of attack_abr I might cry 

##aidworker_df %>% 
  #group_by(attack_abr) %>% 
  #summarize(total_attack = n(), 
            #total_victims) %>% 
  #ggplot(aes(x = attack_abr, y = total_attack)) +
  #geom_bar()

## hg <- ggplot(hg_df, aes(x=total_attack, fill = attack_abr)) + geom_histogram(bindwidth = 2)
#hg + xlim(0, 20) + ylim(0, 10)

aidworker_df %>% 
  distinct(actor_type)
```

    ## # A tibble: 15 x 1
    ##    actor_type                        
    ##    <chr>                             
    ##  1 Unknown                           
    ##  2 Non-state armed group: Regional   
    ##  3 Non-state armed group: National   
    ##  4 Non-state armed group: Unknown    
    ##  5 Staff member                      
    ##  6 Non-state armed group: Subnational
    ##  7 Unaffiliated                      
    ##  8 State: unknown                    
    ##  9 Police or paramilitary            
    ## 10 Host state                        
    ## 11 Aid recipient                     
    ## 12 Non-state armed group: Global     
    ## 13 Criminal                          
    ## 14 Foreign or coalition forces       
    ## 15 Host State

Hypothesis:

We hypothesize that the following variables may be relevant in building
a model to predict the log odds of attack based on attack means:

  - Total men affected (gender\_male)
  - Total women affected (gender\_female)
  - Year
  - Attack context
  - Location
  - Country (we will have to limit this - I wonder if we should just
    limit to the 10 most dangerous countries? This would be Afghanistan,
    CAR, DRC, Iraq, Pakistan, Somalia, South Sudan, Sudan, Syrian Arab
    Republic, Yemen)
  - Actor type (non-state armed group, state, policy/paramilitary)
  - International org affected

<!-- end list -->

``` r
#aidworker_df %>% 
  #select(gender_male, gender_female, year, location, attack_context, actor_type, ) %>% 
  
#cor_aidworker <- cor(aidworker_df)

#corrplot_aidworker <- 
```
