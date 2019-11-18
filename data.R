
library(tidyverse)
library(dplyr)
library(tidyr)
library(car)

simple_background <- background_checks %>%
  dplyr::select(month, state, permit) %>%
  mutate(year = substr(month, 1, 4),
         month = substr(month, 6, 7)) %>%
  dplyr::select(state, year, month, permit)



simple_gun <- gun_violence %>% 
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7)) %>%
  dplyr::select(state, year, month, state, city_or_county, n_killed, 
                n_injured, latitude, longitude, n_guns_involved) 

compiled1 <- inner_join(simple_background, simple_gun, by = c("state", "year", "month"))

# Renaming the state column from "...1" to "State"

names(population)[1:3] <- c("state", "year", "population")

# Getting state names wihtout the period in front

simple_population <- population %>%
  filter(!state %in% c(".District of Columbia", "United States", "Midwest", "Northeast", "South", "West")) %>%
  gather(year, population, c(4:12)) %>%
  dplyr::select(state, year, population)

proper_names <- mutate(simple_population, state= recode(state, "'.Alabama'='Alabama'")) %>%
  mutate(population, state= recode(state, "'.Alaska'='Alaska'")) %>%
  mutate(population, state= recode(state, "'.Arizona'='Arizona'")) %>%
  mutate(population, state= recode(state, "'.Arkansas'='Arkansas'")) %>%
  mutate(population, state= recode(state, "'.California'='California'")) %>%
  mutate(population, state= recode(state, "'.Connecticut'='Connecticut'")) %>%
  mutate(population, state= recode(state, "'.Colorado'='Colorado'")) %>%
  mutate(population, state= recode(state, "'.Delaware'='Delaware'")) %>%
  mutate(population, state= recode(state, "'.Florida'='Florida'")) %>%
  mutate(population, state= recode(state, "'.Georgia'='Georgia'")) %>%
  mutate(population, state= recode(state, "'.Hawaii'='Hawaii'")) %>%
  mutate(population, state= recode(state, "'.Idaho'='Idaho'")) %>%
  mutate(population, state= recode(state, "'.Illinois'='Illinois'")) %>%
  mutate(population, state= recode(state, "'.Indiana'='Indiana'")) %>%
  mutate(population, state= recode(state, "'.Iowa'='Iowa'")) %>%
  mutate(population, state= recode(state, "'.Kansas'='Kansas'")) %>%
  mutate(population, state= recode(state, "'.Kentucky'='Kentucky'")) %>%
  mutate(population, state= recode(state, "'.Louisiana'='Louisana'")) %>%
  mutate(population, state= recode(state, "'.Massachusetts'='Massachusetts'")) %>%
  mutate(population, state= recode(state, "'.Maine'='Maine'")) %>%
  mutate(population, state= recode(state, "'.Maryland'='Maryland'")) %>%
  mutate(population, state= recode(state, "'.Michigan'='Michigan'")) %>%
  mutate(population, state= recode(state, "'.Minnesota'='Minnesota'")) %>%
  mutate(population, state= recode(state, "'.Mississippi'='Mississippi'")) %>%
  mutate(population, state= recode(state, "'.Missouri'='Missouri'")) %>%
  mutate(population, state= recode(state, "'.Montana'='Montana'")) %>%
  mutate(population, state= recode(state, "'.Nebraska'='Nebraska'")) %>%
  mutate(population, state= recode(state, "'.Nevada'='Nevada'")) %>%
  mutate(population, state= recode(state, "'.New Hampshire'='New Hampshire'")) %>%
  mutate(population, state= recode(state, "'.New Jersey'='New Jersey'")) %>%
  mutate(population, state= recode(state, "'.New Mexico'='New Mexico'")) %>%
  mutate(population, state= recode(state, "'.New York'='New York'")) %>%
  mutate(population, state= recode(state, "'.North Carolina'='North Carolina'")) %>%
  mutate(population, state= recode(state, "'.North Dakota'='North Dakota'")) %>%
  mutate(population, state= recode(state, "'.Ohio'='Ohio'")) %>%
  mutate(population, state= recode(state, "'.Oklahoma'='Oklahoma'")) %>%
  mutate(population, state= recode(state, "'.Oregon'='Oregon'")) %>%
  mutate(population, state= recode(state, "'.Pennsylvania'='Pennsylvania'")) %>%
  mutate(population, state= recode(state, "'.Rhode Island'='Rhode Island'")) %>%
  mutate(population, state= recode(state, "'.South Carolina'='South Carolina'")) %>%
  mutate(population, state= recode(state, "'.South Dakota'='South Dakota'")) %>%
  mutate(population, state= recode(state, "'.Tennessee'='Tennessee'")) %>%
  mutate(population, state= recode(state, "'.Texas'='Texas'")) %>%
  mutate(population, state= recode(state, "'.Utah'='Utah'")) %>%
  mutate(population, state= recode(state, "'.Vermont'='Vermont'")) %>%
  mutate(population, state= recode(state, "'.Virginia'='Virginia'")) %>%
  mutate(population, state= recode(state, "'.Washington'='Washington'")) %>%
  mutate(population, state= recode(state, "'.West Virginia'='West Virginia'")) %>%
  mutate(population, state= recode(state, "'.Wisconsin'='Wisconsin'")) 



final_data <- inner_join(compiled1, proper_names, by = c("state", "year")) 

saveRDS(final_data, "final_data.rds")


state <- read_xlsx("data/correlatesofstatepolicyprojectv2_1 NEED.xlsx")


new_state <- state %>%
  filter(year %in% c(2013, 2014, 2015, 2016, 2017, 2018)) %>%
  dplyr::select(state, year, guncontrol_assaultweapon_ban, w_guncontrol_waitingperiod, 
                guncontrol_stand_your_ground, w_guncontrol_registration_requir, 
                guncontrol_opencarry)

saveRDS(new_state, "state_policy.rds")

