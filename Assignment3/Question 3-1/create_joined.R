library(tidyverse)
library(jsonlite)
library(tidyr)

cum <- read_tsv("cumulative-stats.tsv")
json_data <- RJSONIO::fromJSON(file="https://static01.nyt.com/newsgraphics/2022/01/29/brady-career/216fe5204c6d41cf312f933f886cc16d35e148e9/player-lookup.json")

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
  filter(name2 == "player_name" | name2 == "year_end")

data_sep2 <- data_sep %>% 
  pivot_wider(names_from = name2,
              values_from = value) %>%
  mutate("player_id" = name1) %>%
  select("player_id", "player_name", "year_end")
 
joined <- left_join(cum, data_sep2, by = "player_id") %>%
  select(1, 10, 2:9, 11)
  
write_csv(joined, "/Users/jinyounghur/Documents/2022-winter-dataviz/hw3-3/joined.csv")
