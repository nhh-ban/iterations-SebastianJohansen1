########################
# Problem 2

# Creating a function that takes the Vegvesenet data and turns it into a df
# I solve this by utilising the code from the iterations lesson
transform_metadata_to_df <- function(stations_metadata) {
  transformed_data <- stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(
      latestData = map_chr(latestData, 1, .default = NA_character_),
      latestData = as_datetime(latestData, tz = "UTC") # Changed to UTC
    ) %>% 
    mutate(location = map(location, unlist)) %>% 
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
  return(transformed_data)
}







