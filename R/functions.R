
# Function to get countries in income group
get_countries_in_group <- function(x, group_name) {
  x %>%
    filter(group_name == {{ group_name }}) %>%
    pull(location) %>%
    unique() %>%
    sort() -> countries
  
  return(countries)
}

# Function that computes how many days until ALL population is vaccinated
# This function uses Holt’s linear trend method (https://otexts.com/fpp2/holt.html)
compute_days_until_100 <- function(x, country, 
                                   population = "16+", 
                                   max_days = 365 * 100) {
  
  cat(country, "\n")
  
  # Validate population arg
  if ( !(population %in% c("16+", "18+", "all")) ) {
    stop("Population must be 16+, 18+ or all.")
  }
  
  # Get name of population var
  if (population == "16+") {
    pop_var <- "people_16_fully_vaccinated_per_hundred"
  } else if (population == "18+") {
    pop_var <- "people_18_fully_vaccinated_per_hundred"
  } else if (population == "all") {
    pop_var <- "people_fully_vaccinated_per_hundred"
  }
  
  # Get data
  y <- x %>%
    filter(location == country) %>%
    pull( pop_var )
  
  # Create time series
  if (all(is.na(y))) {
    return(NA)
  } else if (sum(!is.na(y)) < 2) {
    return(NA)
  } else {
    TS <- ts(y, start = c(2021, 1), frequency = 365)
  }
  
  # Compute HOLT's forecast
  # If there's an error, assume it's because if too much NA.
  # Set value to Inf
  forecast <- holt(TS, h = max_days, biasadj = T)
  
  # Get first day when 100.0 is reached
  min_100 <- which(forecast$mean >= 100) %>% min()
  
  return(min_100)
}

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