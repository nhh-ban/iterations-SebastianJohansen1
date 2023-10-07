library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)
library(glue)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 
configs <- 
  read_yaml("vegvesen_configs.yml")

gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 
stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata
source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)
head(stations_metadata_df)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 
source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()


### 6: Final volume query with station name:

# First we save the selected station to extract the station name
selected_station <- stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1)

# Extracting station name
station_name <- selected_station$name

# Extracting the volume data
vol_data <- vol_qry(
  id = selected_station$id,
  from = to_iso8601(selected_station$latestData, -4),
  to = to_iso8601(selected_station$latestData, 0)
)

# Creating the plot  
vol_data %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x = from, y = volume)) + 
  geom_line(linewidth = 1, colour = "darkblue") + 
  labs(
    title = "Traffic Volume Over Time",
    subtitle = paste("Name of Traffic Station: ", station_name),
    x = "Date",
    y = "Volume of Cars"
  ) +
  theme_classic()



