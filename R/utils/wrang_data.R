here() %>% setwd()

aux_df <- df %>%
  select(
    dt_notific,
    sg_uf_not,
    id_municip,
    classi_fin,
    criterio,
    evolucao,
  ) %>%
  filter(
    sg_uf_not == 23
  )

# Compute measuraments ---------------------------------------------------------

confirmed_cases_df <- aux_df %>%
  filter(
    classi_fin == 10 |
      classi_fin == 11 |
      classi_fin == 12,
    criterio != 3
  ) %>%
  group_by(dt_notific, id_municip) %>%
  dplyr::summarise(confirmed_cases = n())

discarted_cases_df <- aux_df %>%
  filter(
    classi_fin == 5,
    criterio != 3
  ) %>%
  group_by(dt_notific, id_municip) %>%
  dplyr::summarise(discarted_cases = n())

reported_cases_df <- aux_df %>%
  group_by(dt_notific, id_municip) %>%
  dplyr::summarise(reported_cases = n())

reported_deaths_df <- aux_df %>%
  filter(
    evolucao == 2
  ) %>%
  group_by(dt_notific, id_municip) %>%
  dplyr::summarise(reported_deaths = n())

recovered_cases_df <- aux_df %>%
  filter(
    evolucao == 1
  ) %>%
  group_by(dt_notific, id_municip) %>%
  dplyr::summarise(recovered_cases = n())

dengue_ce <- plyr::join_all(list(
  confirmed_cases_df,
  discarted_cases_df,
  reported_cases_df,
  reported_deaths_df,
  recovered_cases_df
),
by = c('dt_notific', 'id_municip'),
type='left') %>%
  mutate(suspected_cases = reported_cases - (confirmed_cases + discarted_cases))

# Add county names -------------------------------------------------------------

counties_df <- readxl::read_excel('codigo_municipios.xls')

counties_df %<>%
  janitor::clean_names() %>%
  mutate(codigo_municipio = str_sub(codigo_municipio_completo,
                                    start = 1,
                                    end = 6))

dengue_ce %<>%
  left_join(
    counties_df,
    by=c("id_municip" = "codigo_municipio")
  ) 

