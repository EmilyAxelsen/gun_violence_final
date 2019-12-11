
# Leading in libraries

library(tidyverse)
library(dplyr)
library(tidyr)
library(car)

# Selecting the specific columns that I want from the background_checks daaset.
# Remember to specify that you want to dplyr select with "dplyr::select" rather
# than a select from a different package. 

simple_background <- background_checks %>%
  dplyr::select(month, state, permit) %>%
  
# Within the mutate, I separate out the year from the month column to get the 
# year in one column adn the month in another state. Therefore, in order to 
# separate out the year I call the month column and say to separate out the 
# 1st to the 4th character. I do the same thing for the new month column but
# specify the 6th and 7th characters since the month is a two characters.
  
  mutate(year = substr(month, 1, 4),
         month = substr(month, 6, 7)) %>%
  
# Here I select the specific columns I want to keep in my dataset as the 
# state, year, month, and permit. 
  
  dplyr::select(state, year, month, permit)

# First, I make a new column that has the year separate from the month with
# the same process as I did for the background_checks dataset. I use the
# substr function to specify the year from the date column as characters
# one to four and the month as characters 6 and 7. 

simple_gun <- gun_violence %>% 
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7)) %>%
  
# I specified the select from the dplyr package and chose which columns
# I wanted to keep in my dataset. 
  
  dplyr::select(state, year, month, state, city_or_county, n_killed, 
                n_injured, latitude, longitude, n_guns_involved) 

# Next, I use the inner_join function to join the simple_background and
# simple_gun datasets by state, year, and month and name the joined 
# dataset compiled1.

compiled1 <- inner_join(simple_background, simple_gun, by = c("state", "year", "month"))

# I rename the state column from "...1" to "state," "...2" to "year,"
# and "...3" to "population."

names(population)[1:3] <- c("state", "year", "population")

# The big picture of what I'm doing in the next few steps is getting 
# state names wihtout the period in front. 

# First, I filter to remove the regions that I don't want in my final
# dataset because the other joined set does not have the region 
# specifications.

simple_population <- population %>%
  filter(!state %in% c(".District of Columbia", "United States", "Midwest", "Northeast", "South", "West")) %>%
  
# Here, I use the gather function to make a year column rather than 
# having columns for each separate year.
  
  gather(year, population, c(4:12)) %>%
  
# Finally, I select the columns that I want. 
  
  dplyr::select(state, year, population)

# Here, I manually recode each of the 50 states from having a period 
# in front of their name to just being their name.
# Without recoding each state to not have a period in front of their
# name, I would not have been able to join proper_names with another
# dataset because the names that you're joining by must match 
# exactly. 
# In order to actually recode, I call the dataset I want to edit 
# within the mutate function and specify a new column called state.
# I set state equal to a recode of state with the old name first 
# and the new name second. 

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

# After renaming all the states so they will match the state names in
# proper_names, I can then call the inner_join function and join
# compiled1 with proper_names by state and year. 

final_data <- inner_join(compiled1, proper_names, by = c("state", "year")) 

# I then use the write_rds function to create the final_data data set
# I just created into an RDS file that I can then read in to another
# file with read_rds. 

write_rds(final_data, "gun_violence/final_data.rds")

# Here, I'm working with my state policy data that I read in with 
# the read_xlsx function.

state <- read_xlsx("data/correlatesofstatepolicyprojectv2_1 NEED.xlsx")

# I specify the year column as a character so I am able to call
# filters and other functions on the column. 

state <- state %>%
  mutate(year = as.character(year))

# Next, I select the specific columns I want to work with by making
# sure to call dplyr::select to avoid any problems with other select 
# functions.

new_state <- state %>%
  dplyr::select(state, year, guncontrol_assaultweapon_ban, w_guncontrol_waitingperiod, 
                guncontrol_stand_your_ground, w_guncontrol_registration_requir, 
                guncontrol_opencarry) 

# Finally, I save my new dataset as an RDS file to be able to easily 
# call it later.

saveRDS(new_state, "state_policy.rds")

# I use the left join function to join the two datasets by state and 
# year. 

data_gun_violence <- left_join(final_data, new_state, by = c("state", "year"))

# Again, I save the new dataset as an RDS file to be able to call it 
# easily later in my project. 

saveRDS(data_gun_violence, "policy_and_checks.rds")

