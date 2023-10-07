
### Problem 2

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


### Problem 4
to_iso8601 <- function(datetime, offset_days) {
  
  # Converting the string to a datetime object
  datetime_var <- anytime(datetime)
  
  # Adding the offset to the original time
  adjusted_date_time <- datetime_var + days(offset_days)
  
  # Converting to UTC and adding ISO8601 formatting + Z
  iso8601_z <- format(adjusted_date_time,
                      format = "%Y-%m-%dT%H:%M:%SZ",
                      tz = "UTC")
  
  return(iso8601_z)
}


### Problem 5
transform_volumes <- function(json_data) {
  # Converting the JSON string to a list if necessary
  if(is.character(json_data)) {
    json_data <- fromJSON(json_data)
  }
  
  # Extracting the nested data
  edges <- json_data$trafficData$volume$byHour$edges
  
  # Converting the list into a df
  volumes_df <- tibble(
    from = map_chr(edges, ~ .x$node$from) %>% ymd_hms(),
    to = map_chr(edges, ~ .x$node$to) %>% ymd_hms(),
    volume = map_dbl(edges, ~ .x$node$total$volumeNumbers$volume)
  )
  
  return(volumes_df)
}
