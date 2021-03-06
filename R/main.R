# Set up =======================================================================

## Load packages ####
library(tidyverse)
library(skimr)
library(readxl)
library(ggtext)
library(extrafont)
library(forecast)

## Initialize containers ####
Plots <- list()

## Source files ####
source("R/AuxObjects.R")
source("R/functions.R")
source("R/themes.R")

# Load data ====================================================================

## Covid data -------------------------
# This dataset was downloaded from Our World in Data.
# It contains basic COVID-19 statistics by country, like vaccination data, COVID cases,
# COVID deaths, ICU patients, etc.
# Source: https://ourworldindata.org/covid-vaccinations
# Please note that in Bin/ there's a bash script to download the data (it's updated daily)

COVID_DATA <- read.csv("Data/covid-data.csv")

# As tibble
COVID_DATA %>%
  as_tibble() -> COVID_DATA

## World Bank Data --------------------
# This dataset was downloaded from the World Bank.
# It contains the World Bank income and lending groups.
# Source: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

WORLD_BANK_CLASS <- read_xls("Data/CLASS.xls", sheet = 3, col_names = T)

# As tibble
WORLD_BANK_CLASS %>%
  as_tibble() -> WORLD_BANK_CLASS

## Population Data --------------------
# This dataset was downloaded from the UN's Department of Economic and Social Affairs.
# It contains population estimates by country, age, and year

POPULATION <- read_csv("Data/Population.csv")

## UN country codes -------------------
# This dataset was downloaded from the UN's Statistics Division
# It contains a "dictionary" of alphanumeric codes assigned to countries and regions.
# It'll be used to match the UN's population data with the COVID dataset
UN_CODES <- read.csv("Data/UNSD_codes.csv")

# Data cleaning ================================================================

## Covid data -------------------------

# Parse dates
COVID_DATA %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) -> COVID_DATA

# Write data
COVID_DATA %>%
  write_rds("Out/Data/covid.rds")

## World Bank Data --------------------

# Rename vars
WORLD_BANK_CLASS %>%
  rename(group_code = GroupCode,
         group_name = GroupName,
         iso_code = CountryCode,
         country = CountryName) -> WORLD_BANK_CLASS

# Keep only income classification
WORLD_BANK_CLASS %>%
  filter(group_name %in% c("Low income", 
                           "Lower middle income", 
                           "Upper middle income", 
                           "High income")) -> WORLD_BANK_CLASS

WORLD_BANK_CLASS %>%
  write_rds("Out/Data/world_bank.rds")

## Population Data --------------------

# Clean names
pop_names <- names(POPULATION)
# White spaces
pop_names <- str_replace_all(pop_names, "\\s", "_")
# To lower
pop_names <- str_to_lower(pop_names)
# Non word
pop_names <- str_replace_all(pop_names, "\\W", "")
# Trailing under score
pop_names <- str_remove_all(pop_names, "_$")
# Add prefix
pop_names <- str_replace_all(pop_names, "^(\\d)", "Age_\\1")

names(POPULATION) <- pop_names

# Remove unused cols
POPULATION %>%
  select(-any_of(c("index", "variant", "type", "notes"))) -> POPULATION

# Rename cols
POPULATION %>%
  rename(date = reference_date_as_of_1_july) -> POPULATION

# Keep only 2020 population
POPULATION %>%
  filter(date == 2020) -> POPULATION

# Coerce ages as integer
POPULATION %>%
  mutate(across(contains("Age_"), ~ str_remove_all(.x, "\\s"))) %>%
  mutate(across(contains("Age_"), ~ as.integer(.x))) -> POPULATION

# Get population 16 or older, 18 or older, and total
POPULATION %>%
  rowwise() %>%
  mutate(Total = sum(c_across(contains("Age_")), na.rm = T) * 1000,
         Age_16_more = sum(c_across(matches("1[6-9]|[2-9][0-9]|100")), na.rm = T) * 1000,
         Age_18_more = sum(c_across(matches("1[8-9]|[2-9][0-9]|100")), na.rm = T) * 1000) %>%
  ungroup() %>%
  select(region_subregion_country_or_area, 
         country_code,
         Total,
         Age_16_more,
         Age_18_more) -> POPULATION

# Write data
POPULATION %>%
  write_rds(file = "Out/Data/population_estimates.rds")

rm(pop_names)

## Country codes ----------------------

# Clean names
un_names <- names(UN_CODES)
# Dots as under scores
un_names <- str_replace_all(un_names, "\\.+", "_")
# To lower
un_names <- str_to_lower(un_names)
# Non word
un_names <- str_replace_all(un_names, "\\W", "")
# Trailing under score
un_names <- str_remove_all(un_names, "_$")

names(UN_CODES) <- un_names

rm(un_names)

## Merge datasets ---------------------

# Merge covid data, codes, and population data
DATA <- COVID_DATA %>%
  left_join(UN_CODES %>% select(iso_alpha3_code, m49_code),
            by = c("iso_code" = "iso_alpha3_code")) %>%
  left_join(WORLD_BANK_CLASS %>% select(-country), by = "iso_code")

# Make sure m49_code is numeric
DATA %>%
  mutate(m49_code = as.integer(m49_code)) -> DATA

# Merge data with population
DATA %>%
  left_join(POPULATION, by = c("m49_code" = "country_code")) -> DATA

## Merged dataset cleaning ------------

# Get countries that are not in the World Bank classification
DATA %>%
  filter(is.na(group_code)) %>%
  pull(location) %>% 
  unique() -> countries2remove

# Get countries without vaccination record
DATA %>%
  group_by(iso_code, location) %>%
  filter(all(is.na(people_fully_vaccinated_per_hundred))) %>%
  pull(location) %>% 
  unique() %>%
  c(countries2remove) -> countries2remove

# Save table with countries that are going to be removed
countries2remove %>%
  unique() %>%
  sort() %>%
  tibble(
    country_or_region = .
  ) %>%
  write_csv(file = "Out/Summaries/removed_countries.csv", col_names = F)

# Removed countries or regions
DATA %>%
  filter(!(location %in% countries2remove)) -> DATA

# Compute new rates (using UN population)
DATA %>%
  mutate(people_16_fully_vaccinated_per_hundred = people_fully_vaccinated / Age_16_more * 100,
         people_16_vaccinated_per_hundred = people_vaccinated / Age_16_more * 100,
         people_18_fully_vaccinated_per_hundred = people_fully_vaccinated / Age_18_more * 100,
         people_18_vaccinated_per_hundred = people_vaccinated / Age_18_more * 100) -> DATA

# Write merged data
DATA %>%
  write_rds(file = "Out/Data/merged_data.rds")

rm(COVID_DATA, WORLD_BANK_CLASS)

# Analysis =====================================================================

## Descriptors ------------------------

### Countries per income group --------

# Get countries per income group
income_groups <- unique(DATA$group_name) %>% as.list()
income_groups_with_countries <- lapply(income_groups, get_countries_in_group, x = DATA)
names(income_groups_with_countries) <- income_groups

# Write tables
for (group in income_groups) {
  # Get clean name (without spaces)
  group_name_clean <- str_replace_all(group, "\\s", "_") %>% str_to_lower()
  
  # Write table
  write_csv(income_groups_with_countries[[group]] %>% as_tibble(), 
            file = str_c("Out/Summaries/", group_name_clean, "_countries.csv"),
            col_names = F)
}

# Clean workspace
rm(group, group_name_clean)

### Vaccination summary ---------------

DATA %>%
  group_by(iso_code, location) %>%
  filter(!is.na(people_fully_vaccinated), !is.na(total_vaccinations)) %>%
  summarise(days_of_vaccination = ( max(date) - min(date) ) %>% as.numeric(),
            # Number of people fully vaccinated
            people_fully_vaccinated = max(people_fully_vaccinated, na.rm = T),
            # Number of people fully vaccinated per 100
            people_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred, na.rm = T),
            # Number of people fully vaccinated per 100 (base: aged 16+)
            people_16_fully_vaccinated_per_hundred = max(people_16_fully_vaccinated_per_hundred, na.rm = T),
            # Number of people fully vaccinated per 100 (base: aged 18+)
            people_18_fully_vaccinated_per_hundred = max(people_18_fully_vaccinated_per_hundred, na.rm = T),
            # Naive rate of vaccination (vaccinated / days)
            naive_rate_vaccination = people_fully_vaccinated / days_of_vaccination,
            # Number of people vaccinated
            people_vaccinated = max(people_vaccinated, na.rm = T),
            # Number of people vaccinated per 100
            people_vaccinated_per_hundred = max(people_vaccinated_per_hundred, na.rm = T),
            # Population (Our World in Data)
            population = max(population),
            # Population (16+, UN estimates)
            population_16 = max(Age_16_more),
            # Population (18+, UN estimates)
            population_18 = max(Age_18_more),
            # Income group
            group = max(group_name)) -> Vaccination_summary

# If days of vaccination is 0, then rate is number of vaccinated
Vaccination_summary %>%
  mutate(naive_rate_vaccination = if_else(days_of_vaccination < 1, people_fully_vaccinated, naive_rate_vaccination)) -> Vaccination_summary

# Get countries to remove because of missing population estimates
Vaccination_summary %>%
  filter(is.infinite(people_16_fully_vaccinated_per_hundred) |
         is.infinite(people_18_fully_vaccinated_per_hundred)) %>%
  pull(location) %>%
  c(countries2remove) %>%
  unique() -> countries2remove

# Remove countries
Vaccination_summary %>%
  filter( !(location %in% countries2remove) ) -> Vaccination_summary

# Write merged data and remove countries
DATA %>%
  filter( !(location %in% countries2remove) ) %>%
  write_rds(file = "Out/Data/merged_data.rds")

# Write
Vaccination_summary %>%
  write_csv(file = "Out/Summaries/vaccination_summary.csv")

#### Descriptive statistics ####

# Get fully vaccinated summary by group
Vaccination_summary %>%
  group_by(group) %>%
  skim(people_16_fully_vaccinated_per_hundred) -> Vaccination_summary_descriptors

# Write summary
Vaccination_summary_descriptors %>%
  write_csv(file = "Out/Summaries/fully_16_vaccinated_summary_by_income.csv")

# Get fully vaccinated summary by group
Vaccination_summary %>%
  group_by(group) %>%
  skim(people_18_fully_vaccinated_per_hundred) -> Vaccination_summary_descriptors

# Write
Vaccination_summary_descriptors %>%
  write_csv(file = "Out/Summaries/fully_18_vaccinated_summary_by_income.csv")

### Share of population that lives in a country where vaccination started ----

# Load (again hehe) covid data
COVID_DATA <- read_rds("Out/Data/covid.rds")

# Get countries without vaccination data
COVID_DATA %>%
  group_by(iso_code, location) %>%
  filter(all(is.na(people_vaccinated_per_hundred))) %>%
  pull(location) %>% 
  unique() -> countries_no_vaccination

# Get countries without (fully) vaccinated data
COVID_DATA %>%
  group_by(iso_code, location) %>%
  filter(all(is.na(people_fully_vaccinated_per_hundred))) %>%
  pull(location) %>% 
  unique() -> countries_no_vaccination_fully

# Get summary
COVID_DATA %>%
  mutate(vaccination_started = if_else(location %in% countries_no_vaccination,
                                       FALSE,
                                       TRUE),
         fully_vaccinated_available = if_else(location %in% countries_no_vaccination_fully,
                                              FALSE,
                                              TRUE)) %>%
  select(iso_code, continent, location, date, population,
         vaccination_started, fully_vaccinated_available) -> Countries_vaccination

# Load World Bank Classification
WORLD_BANK_CLASS <- read_rds("Out/Data/world_bank.rds")

# Merge with classification
Countries_vaccination %>%
  left_join(WORLD_BANK_CLASS %>% select(-country), by = "iso_code") -> Countries_vaccination

Countries_vaccination %>%
  group_by(group_name) %>%
  summarise(Vac = mean(!vaccination_started))
  
# Summary
Countries_vaccination %>%
  group_by(location) %>%
  summarise(population = max(population),
            vaccination_started = max(vaccination_started) %>% as.logical(),
            group_name = max(group_name)) %>%
  filter(!is.na(group_name)) %>%
  group_by(group_name, vaccination_started) %>%
  summarise(population = sum(population)) %>%
  mutate(total_population = sum(population),
         prop_population = population/total_population) -> Countries_vaccination_started

Countries_vaccination %>%
  group_by(location) %>%
  summarise(population = max(population),
            fully_vaccinated_available = max(fully_vaccinated_available) %>% as.logical(),
            group_name = max(group_name)) %>%
  filter(!is.na(group_name)) %>%
  group_by(group_name, fully_vaccinated_available) %>%
  summarise(population = sum(population)) %>%
  mutate(total_population = sum(population),
         prop_population = population/total_population) -> Countries_vaccination_available

# Write data
Countries_vaccination_started %>%
  write_csv("Out/Summaries/groups_vaccination_started.csv")
Countries_vaccination_available %>%
  write_csv("Out/Summaries/groups_vaccination_data_available.csv")

#### Plots ####

Countries_vaccination_started %>%
  filter(vaccination_started) %>%
  ggplot(aes(x = reorder(group_name, -prop_population), y = prop_population)) +
  geom_bar(stat = "identity",
           colour = "#00bfc4",
           fill = "#00bfc4",
           alpha = 0.75,
           width = 0.75) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(x = "Income group",
       y = "Share of population (%)",
       title = "Share of Population that Lives in a Country Where Vaccination Against Covid-19 Started",
       subtitle = "Countries by income group",
       caption = str_c(caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$share_population_started

### Vaccination summary plots ---------

# Init container
Plots$Vaccination_summary <- list()

# As factor
Vaccination_summary %>%
  mutate(group = factor(group, levels = c("Low income",
                                         "Lower middle income",
                                         "Upper middle income",
                                         "High income"))) -> Vaccination_summary

# Per hundred as ration
Vaccination_summary %>%
  mutate(across(contains("per_hundred"), ~ . / 100)) -> Vaccination_summary

#### Violin: share of fully vaccinated ####

Vaccination_summary %>%
  ggplot(aes(x = group, y = people_16_fully_vaccinated_per_hundred,
             colour = group, fill = group)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(guide = F) +
  scale_colour_discrete(guide = F) +
  labs(title = "Share of People Aged 16+ Fully Vaccinated Against COVID-19",
       subtitle = "Countries grouped by income",
       x = "Income group",
       y = "Share of people fully vaccinated (%)",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Only countries that have started vaccination against COVID-19.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$violin_share_fully_vaccinated

#### Violin: rate of vaccination ####
  
Vaccination_summary %>%
  ggplot(aes(x = group, y = naive_rate_vaccination,
             colour = group, fill = group)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_fill_discrete(guide = F) +
  scale_colour_discrete(guide = F) +
  labs(title = "Naive Rate of Vaccination Against COVID-19",
       subtitle = "Countries grouped by income",
       x = "Income group",
       y = "Rate of vaccination (pple / day)",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Only countries that have started vaccination against COVID-19.", "<br/>",
                       "Naive rate: people aged 16+ fully vaccinated per day.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$violin_rate

#### Violin: days of vaccination ####

Vaccination_summary %>%
  ggplot(aes(x = group, y = days_of_vaccination,
             colour = group, fill = group)) +
  geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_fill_discrete(guide = F) +
  scale_colour_discrete(guide = F) +
  labs(title = "Days Since Vaccination Against COVID-19 Started",
       subtitle = "Countries grouped by income",
       x = "Income group",
       y = "Days",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Only countries that have started vaccination against COVID-19.", "<br/>",
                       caption_data, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$violin_days

#### Bar: top countries by vaccination share ####

Vaccination_summary %>%
  ungroup() %>%
  arrange(-people_16_fully_vaccinated_per_hundred) %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(location, people_16_fully_vaccinated_per_hundred),
             y = people_16_fully_vaccinated_per_hundred,
             colour = group, fill = group)) + 
  geom_bar(stat = "identity", width = 0.75,
           alpha = 0.75) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Share of People Aged 16+ Fully Vaccinated Against COVID-19",
       subtitle = "Top 25 countries",
       x = "Country",
       y = "Share of people fully vaccinated (%)",
       color = "Income group",
       fill = "Income group",
       caption = str_c(caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$bar_top_fully

#### Bar: top countries by naive rate ####

Vaccination_summary %>%
  ungroup() %>%
  arrange(-naive_rate_vaccination) %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(location, naive_rate_vaccination),
             y = naive_rate_vaccination,
             colour = group, fill = group)) + 
  geom_bar(stat = "identity", width = 0.75,
           alpha = 0.75) +
  scale_y_continuous(labels = scales::label_number()) +
  coord_flip() +
  labs(title = "Naive Rate of Vaccination Against COVID-19",
       subtitle = "Top 25 countries",
       x = "Country",
       y = "Rate of vaccination (pple / day)",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Naive rate: people aged 16+ fully vaccinated per day.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$bar_top_rate

#### Bar: top countries by days of vaccination ####

Vaccination_summary %>%
  ungroup() %>%
  arrange(-days_of_vaccination) %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(location, days_of_vaccination),
             y = days_of_vaccination,
             colour = group, fill = group)) + 
  geom_bar(stat = "identity", width = 0.75,
           alpha = 0.75) +
  scale_y_continuous(labels = scales::label_number()) +
  coord_flip() +
  labs(title = "Days Since Vaccination Against COVID-19 Started",
       subtitle = "Top 25 countries",
       x = "Country",
       y = "Days",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Only countries that have started vaccination against COVID-19.", "<br/>",
                       caption_data, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$bar_top_days

### Naive prediction ------------------

Vaccination_summary %>%
  mutate(remaining_16 = population_16 - people_fully_vaccinated,
         naive_remaining_days_16 = remaining_16 / naive_rate_vaccination,
         remaining_18 = population_18 - people_fully_vaccinated,
         naive_remaining_days_18 = remaining_18 / naive_rate_vaccination,
         remaining_all = population - people_fully_vaccinated,
         naive_remaining_days_all = remaining_all / naive_rate_vaccination) -> Vaccination_summary

### Naive prediction plots ------------------

Vaccination_summary %>%
  filter( !(location %in% c("Ukraine", "Guatemala", "Albania")) ) %>%
  ggplot(aes(x = group,
             y = naive_remaining_days_16,
             colour = group, fill = group)) + 
  geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_fill_discrete(guide = F) +
  scale_colour_discrete(guide = F) +
  labs(title = "Days Until All the Population Aged 16+ is Fully Vaccinated (Naive Forecast)",
       subtitle = "Countries grouped by income",
       x = "Country",
       y = "Days",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Only countries that have started vaccination against COVID-19.", "<br/>",
                       "Naive forecast: population aged 16+ not vaccinated divided by current rate of vaccination.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$violin_naive_prediction

## HOLT's forecast --------------------

# Get countries
countries <- unique(DATA$location)

# Compute days until vaccination
holts_remaining_days_16 <- map_dbl(countries, function(country, .progress = T) {
  compute_days_until_100(x = DATA, country = country, population = "16+")
})
holts_remaining_days_18 <- map_dbl(countries, function(country, .progress = T) {
  compute_days_until_100(x = DATA, country = country, population = "18+")
})
holts_remaining_days_all <- map_dbl(countries, function(country, .progress = T) {
  compute_days_until_100(x = DATA, country = country, population = "all")
})

# Creat tibble
holts_forecast <- tibble(location = countries,
                         holts_remaining_days_16 = holts_remaining_days_16,
                         holts_remaining_days_18 = holts_remaining_days_18,
                         holts_remaining_days_all = holts_remaining_days_all,
                         diff_18_16 = holts_remaining_days_18 - holts_remaining_days_16)

# Merge
Vaccination_summary %>%
  left_join(holts_forecast, by = "location") -> Vaccination_summary

# Write
Vaccination_summary %>%
  write_csv("Out/Summaries/vaccination_summary.csv")

Vaccination_summary %>%
  filter(!is.infinite(holts_remaining_days_16),
         !is.infinite(holts_remaining_days_18),
         !is.infinite(holts_remaining_days_all)) %>%
  group_by(group) %>%
  skim(holts_remaining_days_all, 
       holts_remaining_days_18, 
       holts_remaining_days_16,
       naive_remaining_days_16,
       naive_remaining_days_18,
       naive_remaining_days_all) %>%
  write.csv("Out/Summaries/vaccination_forecast_summary.csv")

#### Plot ####

Vaccination_summary %>%
  filter( !is.infinite(holts_remaining_days_16) ) %>%
  ggplot(aes(x = group,
             y = holts_remaining_days_16,
             colour = group, fill = group)) + 
  geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_fill_discrete(guide = F) +
  scale_colour_discrete(guide = F) +
  labs(title = "Days Until All the Population Aged 16+ is Fully Vaccinated",
       subtitle = "Countries grouped by income",
       x = "Country",
       y = "Days",
       color = "Income group",
       fill = "Income group",
       caption = str_c("Remaining days computed using Holt's linear trend method.",
                       "Only countries that have started vaccination against COVID-19.", "<br/>",
                       "Naive forecast: population aged 16+ not vaccinated divided by current rate of vaccination.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$violin_holts_prediction

Vaccination_summary %>%
  filter(group == "Upper middle income") %>%
  ggplot(aes(x = reorder(location, holts_remaining_days_16),
             y = holts_remaining_days_16,
             fill = holts_remaining_days_16,
             colour = holts_remaining_days_16)) +
  geom_bar(stat = "identity") +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Country",
       y = "Days",
       fill = "Days",
       colour = "Days",
       title = "Days Untill the Adult Population (16+) is Fully Vaccinated",
       subtitle = "Holt's linear trend (Upper middle income countries)",
       caption = str_c("Countries in gray = Inf (time estimation out of bounds).", "<br/>",
                       "Naive forecast: population aged 16+ not vaccinated divided by current rate of vaccination.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$bar_remaining_upper_middle

Vaccination_summary %>%
  filter(group == "Lower middle income") %>%
  ggplot(aes(x = reorder(location, holts_remaining_days_16),
             y = holts_remaining_days_16,
             fill = holts_remaining_days_16,
             colour = holts_remaining_days_16)) +
  geom_bar(stat = "identity") +
  scale_colour_viridis_c() +
  scale_fill_viridis_c() +
  coord_flip() +
  labs(x = "Country",
       y = "Days",
       fill = "Days",
       colour = "Days",
       title = "Days Untill the Adult Population (16+) is Fully Vaccinated",
       subtitle = "Holt's linear trend (Lower middle income countries)",
       caption = str_c("Countries in gray = Inf (time estimation out of bounds).", "<br/>",
                       "Naive forecast: population aged 16+ not vaccinated divided by current rate of vaccination.", "<br/>",
                       caption_data, "<br/>",
                       caption_population, "<br/>",
                       caption_world_bank)) +
  theme_DataInt() -> Plots$Vaccination_summary$bar_remaining_lower_middle

# Write plots ==================================================================

save_plots(Plots$Vaccination_summary)

Vaccination_summary %>%
  filter( !( is.na(naive_remaining_days_all) | is.infinite(holts_remaining_days_all) ) ) %>%
  mutate(diff = naive_remaining_days_all - holts_remaining_days_all) %>%
  pull(diff) %>%
  mean(na.rm = T)

