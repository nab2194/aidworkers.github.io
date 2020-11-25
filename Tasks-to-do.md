Tasks to do
================
Natalie Boychuk, Alisha Sarakki, Brennan Bollman, Emily Bamforth, Kailey
Rishovd
20-11-25

# P8105 Final Project

## Title: “Attacks on Aid: Quantifying Risk of Violence among Aid Workers in Global Humanitarian Settings”

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
