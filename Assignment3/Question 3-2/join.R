library(tidyverse)
library(jsonlite)
library(tidyr)

cum <- read_tsv("cumulative-stats.tsv")

json_data <- RJSONIO::fromJSON("./player-lookup.json")

raw <- enframe(unlist(json_data))

rgx_split <- "\\."
n_cols_max <-
  raw %>%
  pull(name) %>% 
  str_split(rgx_split) %>% 
  map_dbl(~length(.)) %>% 
  max()
n_cols_max

nms_sep <- paste0("name", 1:n_cols_max)

data_sep <-
  raw %>%
  separate(name, into = nms_sep, sep = rgx_split, fill = "right") %>%
  select(name1, name2, value) %>%
  filter(name2 == "player_name" | name2 == "year_end" | name2 == "age_start" | name2 == "year_start")

data_sep2 <- data_sep %>% 
  pivot_wider(names_from = name2,
              values_from = value) %>%
  mutate("player_id" = name1) %>%
  select("player_id", "player_name", "year_end", "age_start", "year_start")

joined3 <- left_join(cum, data_sep2, by = "player_id") %>%
  select(1, 10, 2:9, 11, 12, 13) %>%
  mutate(age_start = as.numeric(age_start)) %>%
  mutate(year_start = as.numeric(year_start)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(age = age_start + year - year_start) %>%
  filter(year_end = 202playoff_wins != 0)

write_csv(joined3, "/Users/jinyounghur/Documents/2022-winter-dataviz/hw3-3/joined3.csv")
