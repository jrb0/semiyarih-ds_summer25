install.packages("pacman")
library(tidyverse)
library(lubridate)


pacman::p_load(pacman, tidyverse, nycflights13)

(log(32) * log(32)) / (11 ^ (1/3))

27^(1/3)

dates <- seq(as.Date("2015/1/1"), as.Date("2025/1/1"), "2 months") #creates a vector with dates

dates_df <- tibble(date = dates) #converts vector into date frame

dates_df <- dates_df %>% #adds new colums to the date frame for year, quarter, and isoweek
  mutate(year = year(date),
         quarter = quarter(date),
         week = isoweek(date))

dates_df

?possibly
?parse_date_time
?diff
