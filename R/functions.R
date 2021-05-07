get_countries_in_group <- function(x, group_name) {
  x %>%
    filter(group_name == {{ group_name }}) %>%
    pull(location) %>%
    unique() %>%
    sort() -> countries
  
  return(countries)
}
