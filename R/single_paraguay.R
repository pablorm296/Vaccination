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

Data <- read_rds("Out/Data/merged_data.rds")

# Filter data ==================================================================

Data %>%
  filter(location == "Paraguay") -> Data

# Plots ========================================================================

Data %>%
  ggplot(aes(x = date, y = fu))
