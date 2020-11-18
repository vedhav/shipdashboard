# Before running this script make sure to populate the constants in the app_constants.R inside the R directory
library(tidyverse)
library(dbx)
library(RMySQL)
# Make sure to run this script from the same directory where this script is present Or change the source() functions accordingly
source("../R/app_constants.R")
source("../R/database_functions.R")

ships <- readRDS("../data/ships.RDS")
data.table::setnames(ships, old = c('PORT','port'), new = c('vessel_port','location_port'))
ships_data <- ships %>% select(SHIP_ID,SHIPNAME,WIDTH,LENGTH,ship_type, FLAG, DWT) %>% distinct() %>% arrange(SHIP_ID)
# This check let's us know that the data sebset has multiple SHIP_IDS, Which is not what we want if we consider SHIP_ID to be primary identified for this data
length(unique(ships_data$SHIP_ID)) == nrow(ships_data)
# Taking the subset of the data with this case to try to fix it
duplicate_ships_data <- ships_data %>%
    filter(SHIP_ID %in% (ships_data %>% count(SHIP_ID) %>% arrange(desc(n)) %>% filter(n > 1) %>% pull(SHIP_ID))) %>%
    arrange(SHIP_ID)
# Looks like some of the ship ids have multiple ship name and ship dimensions
# Upon inspection they mostly look like name typos but there are certain special cases.

# Case 1: ship_id 4666609 has 6 different names and the naming convention suggest some kind of version so we might not want to truncate this by renaming the name.
# # A tibble: 6 x 7
#   SHIP_ID SHIPNAME        WIDTH LENGTH ship_type   FLAG    DWT
#     <dbl> <chr>           <dbl>  <dbl> <chr>       <chr> <dbl>
# 1 4666609 BLACKPEARL 7.7V   100   1000 Unspecified SO       NA
# 2 4666609 BLACKPEARL 7.6V   100   1000 Unspecified SO       NA
# 3 4666609 BLACKPEARL 7.8V   100   1000 Unspecified SO       NA
# 4 4666609 BLACKPEARL 7.5V   100   1000 Unspecified SO       NA
# 5 4666609 BLACKPEARL 7.4V   100   1000 Unspecified SO       NA
# 6 4666609 BLACKPEARL 7.3V   100   1000 Unspecified SO       NA

# Case 2: ship_id 4190565 has two different ship_types potentially indicating that a ship might convert it's type, So this information also could not be unique to the ship_id
# We are going to ignore this special case because it occured only once in your dataset. Considering this an data collection error.
# # A tibble: 2 x 7
#   SHIP_ID SHIPNAME   WIDTH LENGTH ship_type FLAG    DWT
#     <dbl> <chr>      <dbl>  <dbl> <chr>     <chr> <dbl>
# 1 4190565 OCEANOGRAF    14     50 Tug       PL       NA
# 2 4190565 OCEANOGRAF    14     50 Fishing   PL       NA

# Case 3: ship_id 3653787 has different flags and it's highly likely that it's a typo
# We are going to ignore this special case because it occured only once in your dataset. Considering this an data collection error.
# # A tibble: 2 x 7
#   SHIP_ID SHIPNAME WIDTH LENGTH ship_type FLAG    DWT
#     <dbl> <chr>    <dbl>  <dbl> <chr>     <chr> <dbl>
# 1 3653787 ARGO        12     33 Tug       NL        0
# 2 3653787 C           12     33 Tug       RU        0

# Case 4: Same ship with multiple Deadweights, This could mean that the ship might have gone through some modification therby changing it's capacity.
# But for the sake of our shiny app let's assume that this is not the case and every ship has a unique DWT attribute
# # A tibble: 2 x 7
#   SHIP_ID SHIPNAME WIDTH LENGTH ship_type FLAG    DWT
#     <dbl> <chr>    <dbl>  <dbl> <chr>     <chr> <dbl>
# 1  297208 MARIEKE     21     97 Tug       LU     8327
# 2  297208 MARIEKE     21     97 Tug       LU     5850

# Case 5: ship_id 347195 Some names do not seem to be typos. Maybe they have different names based on their travel destination.
# # A tibble: 2 x 7
#   SHIP_ID SHIPNAME         WIDTH LENGTH ship_type FLAG    DWT
#     <dbl> <chr>            <dbl>  <dbl> <chr>     <chr> <dbl>
# 1  347195 GAZPROMNEFT WEST    13     79 Tanker    RU     2778
# 2  347195 VOVAN               13     79 Tanker    RU     2778

# For the sake of this shiny app we are going to clean this by combining the SHIPNAME together and take the median of the SHIPWIDTH AND LENGTH
duplicate_ships_data <- duplicate_ships_data %>% group_by(SHIP_ID) %>%
    summarise(SHIPNAME = paste(SHIPNAME %>% str_remove("^\\. *") %>% unique %>% sort(), collapse = " | "),
        WIDTH = median(WIDTH),
        LENGTH = median(LENGTH),
        DWT = median(DWT),
        ship_type = ship_type[1],
        FLAG = FLAG[1])
# Now joining the original data with this data
ships_data <- ships_data %>% filter(!SHIP_ID %in% duplicate_ships_data$SHIP_ID) %>%
    rbind(duplicate_ships_data) %>% arrange(SHIP_ID)
insert_into_db("ships_data", ships_data)
# 0.101 sec elapsed
ais_data <- ships %>% select(SHIP_ID,LAT,LON,SPEED,COURSE,HEADING,DESTINATION,DATETIME,vessel_port,location_port,is_parked)
insert_into_db("ais_data", ais_data)
