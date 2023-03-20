# U.S. COVID Trends

# Load the tidyverse package
library(tidyverse)

# Load data at the national, state, and county level. 
# Load the *national level* data into a variable.
national <- read.csv ("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
View(national)

# Load the *state level* data into a variable.
states <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
View(states)

# Load the *county level* data into a variable.
counties <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# How many observations (rows) are in each dataset?
obs_national <- nrow(national)

obs_states <- nrow(states)

obs_counties <- nrow(counties)

# How many columns are there in each dataset?
num_features_national <- ncol(national)

num_features_states <- ncol(states)

num_features_counties <- ncol(counties)

# Exploratory analysis ----------------------------------------------------

# Load dyplr package
library(dplyr)

# How many total cases have there been in the U.S. by the most recent date
# in the dataset?
total_us_cases <- national %>% 
  filter(cases == max(cases),date == max(date)) %>% 
  pull(cases)

# How many total deaths have there been in the U.S. by the most recent date
# in the dataset?
total_us_deaths <- national %>% 
  filter(deaths == max(deaths),date == max(date)) %>% 
  pull(deaths)

# Which state has had the highest number of cases?
state_highest_cases <- states %>% 
  filter(cases == max(cases)) %>% 
  pull(state)

# What is the highest number of cases in a state?
num_highest_state <- states %>% 
  filter(cases == max(cases)) %>% 
  pull(cases)

# Which state has the highest ratio of deaths to cases (deaths/cases), as of the
# most recent date? 
# (Create a new column to do this!)

state_highest_ratio <- states %>% 
  mutate(ratio = deaths / cases) %>% 
  filter(ratio == max(ratio)) %>%
  filter(date == max(date)) %>% 
  pull(state)

# Which state has had the lowest number of cases *as of the most recent date*?
state_lowest_cases <- states %>% 
  filter(date == max(date)) %>%
  filter(cases == min(cases)) %>% 
  pull(state)

# Which county has had the highest number of cases?
county_highest_cases <- counties %>% 
  filter(cases == max(cases)) %>% 
  pull(county)

# What is the highest number of cases that have happened in a single county?
num_highest_cases_county <- counties %>%
  filter(cases == max(cases)) %>% 
  pull(cases)

# Because there are multiple counties with the same name across states, it
# will be helpful to have a column that stores the county and state together
# (in the form "COUNTY, STATE").
# Add a new column to `counties` data frame called `location`
# storing the county and state (separated by a comma and space).
counties <- counties  %>% 
  mutate(location = paste0(county,", ", state))

# What is the name of the location (county, state) with the highest number
# of deaths? 
location_most_deaths <- counties %>% 
  filter(deaths == max(deaths, na.rm = T)) %>% 
  pull(location)

# Add (mutate) a new column to `national` data frame called `new_cases`
# that has the nubmer of *new* cases each day 
national <- national %>% 
  mutate(new_cases = cases - lag(cases, default = 0))
View(national)

# Add (mutate) a new column on your `national` data frame called `new_deaths`
# that has the number of new deaths each day
national <- national %>% 
  mutate(new_deaths = deaths - lag(deaths, default = 0))

# What was the date when the most new cases occured?
date_most_cases <- national %>%
  filter(new_cases == max(new_cases, na.rm = T)) %>% 
  pull(date)

# What was the date when the most new deaths occured?
date_most_deaths <- national %>% 
  filter(new_deaths == max(new_deaths)) %>% 
  pull(date)

# How many people died on the date when the most deaths occured?
most_deaths <- national %>% 
  filter(date == date_most_deaths) %>% 
  pull(new_deaths)


# Create basic plot by passing `national$new_cases` column to the
# `plot()` function.
new_cases_plot <- plot(national$new_cases)

# Create basic plot by passing the `new_deaths` column to the
# `plot()` function.
new_deaths_plot <- plot(national$new_deaths)

# Grouped analysis --------------------------------------------------------

# What is the county with the *current* (on the most recent date)
# highest number of cases in each state? Should be a *vector* of
# `location` names (the column with COUNTY, STATE).
highest_in_each_state <- counties %>% 
  group_by(state) %>% 
  filter(date == max(date, na.rm = T)) %>% 
  filter(cases == max(cases, na.rm = T)) %>% 
  pull(location)

# Which locaiton (COUNTY, STATE) has had the highest number of cases
# in Washington?
highest_in_wa <- counties %>% 
  filter(state == "Washington") %>% 
  filter(cases == max(cases)) %>% 
  select(location)

# What is the county with the *current* (e.g., on the most recent date)
# lowest number of deaths in each state? Should be a *vector* of
# `location` names (the column with COUNTY, STATE).
lowest_in_each_state <- counties %>%
  group_by(state) %>% 
  filter(date == max(date, na.rm = T)) %>% 
  filter(deaths == min(deaths, na.rm = T)) %>% 
  pull(location)

# What *proportion* of counties have had zero deaths in each state? In other
# words, in each state, how many counties have had zero deaths, divided by
# the total number of counties in the state? Return *dataframe*
# with both the state name, and the proportion in a variable

prop_no_deaths <- counties %>% 
  filter(date == max(date)) %>% 
  group_by(state) %>% 
  mutate(zero_deaths = if_else(deaths > 0, 0, 1)) %>% 
  summarize(total_county = n(), zero_deaths = sum(zero_deaths)) %>% 
  mutate(prop = zero_deaths / total_county) %>% 
  select(state, prop)

# What proportion of counties in Washington have had zero deaths?
wa_prop_no_deaths <- counties %>% 
  filter(date == max(date))%>% 
  mutate(zero_deaths = if_else(deaths > 0, 0, 1)) %>% 
  group_by(state) %>% 
  filter(state == "Washington") %>% 
  summarize(total_county = n(), zero_deaths = sum(zero_deaths)) %>% 
  mutate(prop = zero_deaths / total_county) %>% 
  pull(prop)

# Adding up all of the cases on each day in the
# `states` or `counties` dataset SHOULD add up to the number at the
# `national` level.

# Add up the cases on each day in the
# `states` dataframe. 
# This will be a dataframe with the columns `date` and `state_total`.
state_by_day <- states %>% 
  group_by(date) %>% 
  summarise(state_total = sum(cases)) %>% 
  select(date, state_total)

# Create `county_by_day` by adding up the cases on each day in the
# `counties` dataframe. Call column with the total cases
# `county_total`. This will also be a dataframe
county_by_day <- counties %>%
  group_by(date)%>% 
  summarise(county_total = sum(cases))%>% 
  select(date, county_total)

# *join* those two dataframes into one called `totals_by_day`
totals_by_day <- state_by_day %>% 
  left_join(county_by_day, by = 'date')

# Create `all_totals` by joining `totals_by_day`
# to the `national` dataframe
all_totals <- totals_by_day %>% 
  left_join(national, by = 'date') 

# How many rows are there where the state total *doesn't equal* the national
# cases reported?
num_state_diff <- all_totals %>% 
  filter(cases != state_total) %>% 
  nrow()

# How many rows are there where the county total *doesn't equal* the natinal
# cases reported? 
num_county_diff <- all_totals %>% 
  filter(cases != county_total) %>% 
  nrow()

# An inconsistency -- find out where inconsistency lies. Take the county level data,
# and add up all of the cases to the state level on each day (e.g.,
# aggregating to the state level). Store this dataframe with three columns
# (state, date, county_totals)
# To avoid DPLYR automatically grouping results,
# `.groups = "drop"` in `summarize()` statement. 

sum_county_to_state <- counties %>% 
  group_by(state, date) %>% 
  summarize(county_total = sum(cases, na.rm = T), .groups = "drop")

# Join together the `sum_county_to_state` dataframe with the
# `states` dataframe into the variable `joined_states`.
joined_states <- sum_county_to_state %>% 
  left_join(states, by = "state", "date")

# Find out where (and when) there is a discrepancy in the number of cases,
# which has *only* the observations
# where the sum of the county cases in each state and the state values are
# different. Will be a *dataframe*.
has_discrepancy <- joined_states %>% 
  filter(county_total != cases) 


# Find the *state* where there is the *highest absolute difference*
# between the sum of the county cases and the reported state cases.

state_highest_difference <- joined_states %>% 
  mutate(case_difference = county_total - cases) %>% 
  filter(case_difference == max(abs(case_difference))) %>% 
  pull(state)

# Further exploration -------------------------------------------------

#1.Which state had the lowest absolute difference between sum of county cases
# and the reported state cases? `state_lowest_difference`
state_lowest_difference <- joined_states %>% 
  mutate(case_difference = county_total - cases) %>% 
  filter(case_difference == min(case_difference)) %>% 
  pull(state)

#2. Is there any counties in washington that hasnt had any deaths from covid in 
# the most recent date? What are they? `no_washington_deaths`
no_washington_deaths <- counties %>% 
  filter(state == "Washington") %>% 
  filter(date == max(date), deaths == 0) %>% 
  select(location)

#3. What is the sum of new_cases 2 weeks after christmas? `after_christmas_cases`
after_christmas_cases <- national %>% 
  filter(date > "2020-12-25" & date < "2021-01-08") %>% 
  summarise(after_cases = sum(new_cases)) %>% 
  pull(after_cases)


#4. What is the difference in the sum of 'new cases' 2 weeks after the christmas 
# holiday to 2 weeks before the christmas holiday? (2020-12-11 : 2020-12-25,
# 2020-12-25 : 2021-01-08)`diff_holiday_new_cases`

cases_before <- national %>% 
  filter(date > "2020-12-11" & date <"2020-12-25") %>% 
  summarise(before_cases = sum(new_cases)) %>% 
  pull(before_cases)

case_after <- national %>% 
  filter(date > "2020-12-25" & date< "2021-01-08") %>% 
  summarise(after_cases = sum(new_cases)) %>% 
  pull(after_cases)

diff_holiday_new_cases <- case_after - cases_before
