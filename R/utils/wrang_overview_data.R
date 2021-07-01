library(dplyr)
library(stringr)
library(reactable)
library(tidyr)

df_counties_info <- here::here("raw_data", "codigo_municipios.xls") %>%
  readxl::read_excel() %>%
  janitor::clean_names() %>%
  mutate(codigo_municipio = str_sub(codigo_municipio_completo,
                                    start = 1,
                                    end = 6))

dengue_ce <- here::here("data", "dengue_ce.csv") %>%
  readr::read_csv()

df_brazil_counties_population <- here::here("data", "df_brazil_counties_population.csv") %>%
  readr::read_csv()

ceara_counties <- df_brazil_counties_population %>%
  purrr::pluck("munic_res") %>%
  str_subset('^23') %>%
  unique()

ceara_population_by_year <- df_brazil_counties_population %>%
  filter(munic_res %in% ceara_counties) %>%
  group_by(ano) %>%
  summarise(total_population = sum(populacao))

ceara_counties_population_by_year <- df_brazil_counties_population %>%
  filter(munic_res %in% ceara_counties) %>%
  group_by(ano, munic_res) %>%
  summarise(total_population = sum(populacao)) %>%
  mutate(munic_res = as.character(munic_res)) %>%
  left_join(df_counties_info,
            by = c("munic_res" = "codigo_municipio")) %>%
  select(ano, nome_municipio, nome_mesorregiao, nome_microrregiao,
         total_population)

dengue_measurements <- dengue_ce %>%
  group_by(reported_year) %>%
  summarise(total_confirmed_cases = sum(confirmed_cases, na.rm = T),
            total_reported_deaths = sum(reported_deaths, na.rm = T)) %>%
  left_join(ceara_population_by_year,
            by = c("reported_year" = "ano")) %>%
  mutate(case_incidence = total_confirmed_cases*10^5/total_population,
         mortality = total_reported_deaths*10^5/total_population,
         letality = total_reported_deaths/total_confirmed_cases) %>%
  summarise(across(c(case_incidence, mortality, letality), mean)) %>%
  pivot_longer(cols = everything(), names_to = "measurement", values_to = "mean")

dengue_summary_by_region <- dengue_ce %>%
  group_by(nome_municipio,
           nome_microrregiao,
           nome_mesorregiao,
           id_municip) %>%
  summarise(total_confirmed_cases = sum(confirmed_cases, na.rm = T),
            total_reported_deaths = sum(reported_deaths, na.rm = T))

dengue_measurements_by_region <- dengue_ce %>%
  group_by(reported_year,
           nome_municipio,
           nome_microrregiao,
           nome_mesorregiao,
           id_municip) %>%
  summarise(total_confirmed_cases = sum(confirmed_cases, na.rm = T),
            total_reported_deaths = sum(reported_deaths, na.rm = T)) %>%
  left_join(ceara_counties_population_by_year,
            by = c("reported_year" = "ano",
                   "nome_municipio")) %>%
  mutate(case_incidence = total_confirmed_cases*10^5/total_population,
         mortality = total_reported_deaths*10^5/total_population) %>%
  group_by(nome_municipio,
           nome_microrregiao.x,
           nome_mesorregiao.x) %>%
  rename(nome_microrregiao = nome_microrregiao.x,
         nome_mesorregiao = nome_mesorregiao.x) %>%
  summarise(across(c(case_incidence, mortality), 
                   mean,
                   .names = "{.col}_mean")) %>%
  mutate(case_incidence_mean = scales::comma(case_incidence_mean, accuracy = 0.01),
         mortality_mean = scales::comma(mortality_mean, accuracy = 0.01)) %>%
  left_join(dengue_summary_by_region, by = "nome_municipio") %>%
  select(nome_mesorregiao.x, nome_municipio, nome_microrregiao.x, 
         case_incidence_mean, mortality_mean, total_confirmed_cases,
         total_reported_deaths)