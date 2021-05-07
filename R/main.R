# Set up =======================================================================

# Load packages
library(tidyverse)
library(skimr)
library(readxl)
library(ggtext)
library(extrafont)
library(forecast)

# Init plot container
Plots <- list()

# Define caption
caption_data <- str_c("Vaccination Data: Hannah Ritchie _et al._, (2021), _Coronavirus (COVID-19) Vaccinations_. Published online at OurWorldInData.org. Retrieved ", Sys.Date(), ".")
caption_world_bank <- str_c("Income Grouping: World Bank (2021), _World Bank Country and Lending Groups_. Published online at DataHelpDesk.WorldBank.org. Retrieved ", Sys.Date(), ".")
caption_population <- str_c("Population Data: United Nations (2021), _Standard country or area codes for statistical use_. Published online at UnStats.un.org. Retrieved ", Sys.Date(), ".")


# Load data ====================================================================

## Covid data -------------------------
# This dataset was downloaded from Our World in Data.
# It contains basic COVID-19 statistics by country, like vaccination data, COVID cases,
# COVID deaths, ICU patients, etc.
# Source: https://ourworldindata.org/covid-vaccinations

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
  write_csv(file = "Out/Data/population_estimates.csv")

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
  mutate(people_fully_vaccinated_per_hundred = people_fully_vaccinated / Age_16_more * 100,
         people_vaccinated_per_hundred = people_vaccinated / Age_16_more * 100) -> DATA

# Write merged data
DATA %>%
  write_csv(file = "Out/Data/merged_data.csv")

rm(COVID_DATA, WORLD_BANK_CLASS, countries2remove, POPULATION)

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
            people_fully_vaccinated = max(people_fully_vaccinated, na.rm = T),
            people_fully_vaccinated_per_hundred = max(people_fully_vaccinated_per_hundred, na.rm = T),
            naive_rate_vaccination = people_fully_vaccinated / days_of_vaccination,
            people_vaccinated = max(people_vaccinated, na.rm = T),
            people_vaccinated_per_hundred = max(people_vaccinated_per_hundred, na.rm = T),
            population = max(population),
            group = max(group_name)) -> Vaccination_summary

# If days of vaccination is 0, then rate is number of vaccinated
Vaccination_summary %>%
  mutate(naive_rate_vaccination = if_else(days_of_vaccination < 1, people_fully_vaccinated, naive_rate_vaccination)) -> Vaccination_summary

# Write
Vaccination_summary %>%
  write_csv(file = "Out/Summaries/vaccination_summary.csv")

#### Descriptive statistics ####

# Get fully vaccinated summary by group
Vaccination_summary %>%
  group_by(group) %>%
  skim(people_fully_vaccinated_per_hundred) -> Vaccination_summary_descriptors

# Write
Vaccination_summary_descriptors %>%
  write_csv(file = "Out/Summaries/fully_vaccinated_summary_by_income.csv")

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
  ggplot(aes(x = group, y = people_fully_vaccinated_per_hundred,
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
  arrange(-people_fully_vaccinated_per_hundred) %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = reorder(location, people_fully_vaccinated_per_hundred),
             y = people_fully_vaccinated_per_hundred,
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
  mutate(remaining = population - people_fully_vaccinated,
         naive_remaining_days = remaining / naive_rate_vaccination) -> Vaccination_summary

### Naive prediction plots ------------------

Vaccination_summary %>%
  filter( !(location %in% c("Ukraine", "Guatemala", "Albania")) ) %>%
  ggplot(aes(x = group,
             y = naive_remaining_days,
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

# Function that computes how many days until ALL population is vaccinated
# This function uses Holt’s linear trend method (https://otexts.com/fpp2/holt.html)
compute_days_until_100 <- function(x, country, max_days = 365 * 100) {
  # Create time series
  TS <- ts(x$people_fully_vaccinated_per_hundred[x$location == country],
           start = c(2021, 1), frequency = 365)
  
  # Compute HOLT's forecast
  # If there's an error, assume it's because if too much NA.
  # Set value to Inf
  forecast <- tryCatch(holt(TS, h = max_days),
                       error = function(e) return(Inf))
  
  # If it's not a list, then it's Inf
  if (!is.list(forecast)) {
    return(Inf)
  }
  
  # Get first day when 100.0 is reached
  min_100 <- which(forecast$mean >= 100) %>% min()
  
  return(min_100)
}

# Get countries
countries <- unique(DATA$location)

# Compute days until vaccination
holts_remaining_days <- sapply(countries, compute_days_until_100, x = DATA)

# Creat tibble
holts_forecast <- tibble(location = countries,
                         holts_remaining_days = holts_remaining_days)

# Merge
Vaccination_summary %>%
  left_join(holts_forecast, by = "location") -> Vaccination_summary

# Write
Vaccination_summary %>%
  write_csv("Out/Summaries/vaccination_summary.csv")

#### Descriptive statistics ####

# Get fully vaccinated summary by group
Vaccination_summary %>%
  group_by(group) %>%
  skim(people_fully_vaccinated_per_hundred) -> Vaccination_summary_descriptors

# Write
Vaccination_summary_descriptors %>%
  write_csv(file = "Out/Summaries/fully_vaccinated_summary_by_income.csv")

#### Plot ####

Vaccination_summary %>%
  filter( !is.infinite(holts_remaining_days) ) %>%
  ggplot(aes(x = group,
             y = holts_remaining_days,
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

# Write plots ==================================================================

save_plots <- function(plot_container) {
  # Get names
  plot_names <- names(plot_container)
  
  # Write plots
  lapply(plot_names, function(plot_container, plot_name) {
    # Get plot
    p <- plot_container[[plot_name]]
    
    # Save plot
    ggsave(str_c("Out/Plots/", plot_name, ".png"), p,
           width = 14, height = 10, units = "cm",
           scale = 2, dpi = 400)
  }, plot_container = plot_container)
  
}

save_plots(Plots$Vaccination_summary)
