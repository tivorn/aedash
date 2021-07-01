if(!require(pacman))install.packages('pacman')

pacman::p_load("curl", "tidyverse", "stringr", "read.dbc", "magrittr",
               "janitor", "plyr", "foreign", "lubridate", "plotly",
               "rjson", "here")

# Connect to FTP server --------------------------------------------------------

url <- 'ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/'
connection <- curl(url)
dir_files <- readLines(connection)

# Limit results to Dengue reports from Ceará -----------------------------------

files_name <- dir_files %>% 
  str_subset("DENGCE*") %>%
  str_squish() %>% 
  str_split(" ", n=4) %>%
  unlist() %>%
  str_subset("DENGCE*")
  

# Download all files -----------------------------------------------------------

dir.create('raw_data')

for (file in files_name){
  url_download <- str_glue(url, file)
  
  curl_download(url_download,destfile=str_glue('raw_data/{file}'))
}

# Read and bind all files ------------------------------------------------------

here('raw_data') %>%
  setwd()

data_dir <- list.files()

df <- data_dir %>% 
  map_dfr(read.dbc)

df %<>% clean_names()
