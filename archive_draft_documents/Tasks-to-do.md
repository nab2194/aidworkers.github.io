Tasks to do
================
Natalie Boychuk, Alisha Sarakki, Brennan Bollman, Emily Bamforth, Kailey
Rishovd
20-11-25

# P8105 Final Project

## Title: “Attacks on Aid: Quantifying Risk of Violence among Aid Workers in Global Humanitarian Settings”

## Datasets

#### Uppsala

Uppsala GED: all georeferenced data of armed conflict from 1989. An
incident where armed force used by one organized actor against another
organized actor or civilian resulting in at least one death. Codebook:
<https://ucdp.uu.se/downloads/ged/ged201.pdf>

Humanitarian data: operational datasets of how many orgs work in
country?

#### Aid workers

#### SHCC

(Afghanistan only? iterate multiple years)

## Questions

  - Do aid worker violence correlate with higher overall violence?

  - How has pattern of aid worker violence changed over time: nationals
    vs international attacks, type of attack, etc?

### Aidworker dataset - Natalie, Emily, Brennan

  - Over time and countries, are there patterns to show differences in
    aidworker attacks? (maybe shiny plot)

  - Different types of org workers attacked more frequently (UN vs INGO
    vs local NGOs)

  - Are attacks against international aid workers different versus
    national aid workers? Has the proportion of attacks on international
    vs national aid workers changed over time.

  - stat test on this?

  - Is kidnapping more common among international aid workers versus
    national aid workers?

  - Have types of attacks changed over time?

  - Some sort of map with lat/long: leaflet or shiny?

  - EDA: attack context, type of attack, actor type

  - Aid worker interactive map: \*\* Include dropdown / hover over link
    to show major events. \*\* Highlight countries with attacks but no
    major events, or protracted conflicts: CAR, Nigeria, DRC?

  - Logistic regression: probability experiencing attack with IVs of
    type NGO, national/international, context of attack, type of attack,
    actor type

### Focus on afghanistan - explain why we chose this one. Use multiple datasets. - Alisha, Kailey, Brennan

Use years 2008 - 2016 (describe why in report and show code)

1.  Overall violence - Uppsala GED201 (?)
2.  Aid worker violence
3.  Health facility attacks - SHCC

Step 1: read in, tidy and filter datasets

  - Filter Uppsala dataset just for this country to examine overall
    violence overtime alongside violence against aidworkers.

  - Deaths among all civilians versus all aidworkers side by side

  - SHCC Afghanistan dataset - would have to bring in multiple years.

  - Discuss confounders (U.S. administration changes? Troop levels in
    country? Natural disaster?)

### Website

### TABLE – Aidworker joined with Uppsala

  - Join aidworker and Uppsala based on country & date.

## Things to do

Aid-worker dataset \* Collapse type of NGO variable via `pivot_longer`
so one variable is org type and other var is the number affected

## Team members and task assignments

### Natalie

### Emily

### Kailey

### Alisha

### Brennan

## Proposed Timeline

Nov 29 - Each team member initial data visualizations & analyses created

Nov 29 - Team meeting?

Dec 1 - website completed

Dec 2 - Final draft complete - screencast completed and website
finalized

Dec 5 - Final Project Due

## Project Motivation

In recent decades, attacks on aid workers in complex crises have been
steadily increasing, with 483 workers killed, kidnapped, or wounded in
2019. We are interested in understanding changes in the risk associated
with humanitarian action in different geographic contexts and over time.
We plan to focus our analysis around the Aid Worker Security Database
(AWSD, via Humanitarian Outcomes organization): which has collected data
on attacks against humanitarian aid workers globally since 1997. We will
merge this with other data on violent attacks in conflict settings via
the Safeguarding Health in Conflict Coalition (SHCC) and Uppsala
Conflict Data Program (UCDP) to assess the relationship between aid
worker attacks and conflict intensity, conflict actors (state
vs. non-state actors), and aid worker characteristics (national
vs. international, type of organization). We may then focus on one or
more key settings/case study countries and analyze these contexts with
regard to the degree of humanitarian organizations’ operational
presence, and other possible indicators of insecurity (GDP/capita, food
prices, population displacement, natural disaster etc).

## Intended Final Products

  - EDA & visualization on aid worker attacks: increasing over time?
    Proportion of ‘national’ versus international worker attack? Type of
    attacks?

  - Global map of “hotspots” of aid worker attacks by type (kidnapping,
    landmine, etc.) - multiple years, likely using Leaflet

  - Shiny map interactive map for one year of data with dropdown menu
    showing different outcomes per country: aid worker attacks, health
    facility attacks, civilian casualties, displacement, etc.

  - Interactive app with a predictive model for attack risk based on the
    worker’s nationality, the country in which they work, type of
    conflict, number civilians killed in that conflict, and other
    predictive factors (this will involve regression analysis)
    
      - We are open to suggestions from the teaching team as to whether
        this task would be feasible/valid, given that it may be
        difficult to determine all relevant predictors in the project
        time period.

## Anticipated Data Sources

  - Aid worker security database:
    <https://aidworkersecurity.org/incidents> (for data on all attacks
    on aid workers since 1997)

  - Humanitarian Outcomes data:
    <https://www.humanitarianoutcomes.org/projects(for> potentially
    relevant predictors, such as quantity of humanitarian actors in a
    given context)

  - Uppsala Conflict dataset: <https://ucdp.uu.se> (for data on conflict
    intensity/number of civilian/non-civilian deaths)

  - Humanitarian Data Exchange: <https://data.humdata.org/> (for
    demographic and health data from conflict-affected countries)

  - UNOCHA Attacks on Healthcare in Countries in Conflict datasets:
    <https://data.humdata.org/dataset/shcchealthcare-dataset> (for
    attacks on hospitals in conflcit settings)

## Anticipated Challenges

*Visualizations*: potential challenges in mapping attack data given
different geographic variables across datasets (latitutde vs. longitude,
city or neighborhood level)

*Analyses*: there may be insufficient number of attacks in each country
context over time to meaningfully assess differences between countries.
\* Any predictive models would not be able to take into consideration
behavioral factors of aid workers that may influence violence, but will
still allow us to examine risk factors. Similarly, the predictive model
cannot predict political/civil unrest, natural disasters, resource
shortages, etc. but will be able to calculate risk using these as risk
factors.

*Coding*:

  - Worldwide data may be large and unwieldy and also have multiple
    years of data (though aid worker attacks relatively few per country
    per year) - will need to do EDA to decide whether to subset certain
    \# of years.
  - Multiple datasets: we will at least need to combine aid worker
    security database with Uppsala datasets on total / civilian
    casualties by country etc which might be tedious.
  - Regression analysis may require literature review and other
    considerations to determine if statistically valid, and also would
    combine indicators from multiple datasets.
  - Consideration of doing both global analysis and ‘zero-in’ on one or
    two key contexts to do more granular analysis. Decision about which
    portion of analysis should be global and which context specific
    pending EDA.
