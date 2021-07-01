library(foreign)
library(dplyr)
library(glue)
library(purrr)
library(stringr)
library(tidyr)

# Load counties population dataset ---------------------------------------------

dir_population <- here::here("raw_data", "pop_mun_br")

dir_population_files <- list.files(dir_population)

path_population_files <- glue("{dir_population}/{dir_population_files}")

df_brazil_counties_population <- map_dfr(path_population_files, read.dbf)

# Clean counties population dataset ---------------------------------------------

df_brazil_counties_population %<>%
  janitor::clean_names() %>%
  select(-starts_with('X')) %>%
  mutate(ano = as.integer(as.character(ano)),
         munic_res = stringr::str_sub(munic_res, start = 1, end = 6))

# Write CSV Brasil counties population dataset ---------------------------------------------

data_dir <- here::here("data")

path_output <- glue('{data_dir}/df_brazil_counties_population.csv')

readr::write_csv(df_brazil_counties_population,
                 path_output)

# Write CSV Ceará counties population dataset ---------------------------------

ceara_counties <- df_brazil_counties_population %>%
  purrr::pluck("munic_res") %>%
  str_subset('^23') %>%
  unique()

ceara_counties_population_by_year <- df_brazil_counties_population %>%
  filter(munic_res %in% ceara_counties) %>%
  group_by(ano, munic_res) %>%
  summarise(total_population = sum(populacao)) %>%
  mutate(munic_res = as.character(munic_res)) %>%
  left_join(df_counties_info,
            by = c("munic_res" = "codigo_municipio")) %>%
  select(ano, nome_municipio, nome_mesorregiao, nome_microrregiao,
         total_population)

path_output <- str_glue('{data_dir}/df_ceara_counties_population_by_year.csv')
