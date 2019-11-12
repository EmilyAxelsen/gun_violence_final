library(tidyverse)
library(dplyr)




simple_background <- background_checks %>%
  dplyr::select(month, state, permit) %>%
  mutate(year = substr(month, 1, 4),
         month = substr(month, 6, 7)) %>%
  select(state, year, month, permit)



simple_gun <- gun_violence %>% 
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7)) %>%
  group_by(state, year, month) %>%
  summarise(n_incidents = n()) 

compiled1 <- inner_join(simple_background, simple_gun, by = c("state", "year", "month"))




saveRDS(compiled1, "compiled.rds")

