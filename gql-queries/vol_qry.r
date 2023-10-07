
### Problem 4

vol_qry <- function(id, from, to) {
  query <- glue('{{
    trafficData(trafficRegistrationPointId: "{id}") {{
      volume {{
        byHour(from: "{from}", to: "{to}") {{
          edges {{
            node {{
              from
              to
              total {{
                volumeNumbers {{
                  volume
                }}
              }}
            }}
          }}
        }}
      }}
    }}
  }}', id = id, from = from, to = to)
  
  return(query)
}

# Checking if the qry works as intended. It does
GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)






