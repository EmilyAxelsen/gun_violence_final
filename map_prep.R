
library(sf)
library(fs)
library(tidyverse)

shape <- read_sf("data/states_21basic/states.shp", quiet = TRUE)

write_rds(shape, "map_us.rds")


united_states <- readRDS("final data/final_data.rds") %>%
  drop_na()



locations <- st_as_sf(united_states, coords = c("longitude", "latitude"), crs = 4326)

write_rds(locations, "locations.rds")