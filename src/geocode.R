library(ggmap)
library(dplyr)
library(purrr)
library(tidyr)

df <- data_frame(place = c("Stockholm", "Göteborg"))

# geocode any batch of locality text strings
geocoded <- mutate_geocode(data = df, place)

# now what if we have just coordinate pairs?
# can we do the opposite - reverse geocoding?

mutate_revgeocode <- function (data, ...) {

  coords <- 
    data %>% 
    select(lon, lat) %>% 
    # don't lookup dupes
    distinct()
  
  rgc2df <- function(lon, lat) {
    df <- revgeocode(c(lon, lat), output = "more")
    df %>% mutate_each(funs(as.character))
  }
  
  geography <- 
    map2(coords$lon, coords$lat, rgc2df) %>%
    bind_rows() %>%
    bind_cols(coords) %>%
    select(lon, lat, everything())

  data %>% 
    left_join(geography, by = c("lon", "lat"))
  
}

mutate_revgeocode(geocoded)
