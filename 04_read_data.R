##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Read data
##########################################################################

# library(readr)
library(tidyverse)

#built-in R dataset 
msleep
glimpse(msleep)


# Reading files -----------------------------------------------------------

# Read from an URL
msleep_df <- read_csv('https://raw.githubusercontent.com/joseramoncajide/master_data_science_online/main/data/msleep.csv')

# Save a copy locally
url <- "https://raw.githubusercontent.com/joseramoncajide/master_data_science_online/main/data/msleep.csv"

# Working with directories
.Platform$file.sep
file_name <- basename(url)
file_name

local_path <- 'tmp'
dir.create(local_path)

destination_file_name <- file.path(local_path, 
                                   file_name, 
                                   fsep = .Platform$file.sep)

# Downlaad
download.file(url, destination_file_name)

# load data
msleep_df <- read_csv(destination_file_name)



# Read data from html documuent -------------------------------------------


library(rvest)

url <- "https://www.comuniazo.com/comunio-apuestas/jugadores"

jugadores_df <- url %>% 
  read_html() %>% 
  html_element(css = '.page-1') %>% 
  html_table(convert = T)

# Exercise
# Change column names according to https://www.comuniazo.com/comunio-apuestas/jugadores

# Sol:
names(jugadores_df) <- c("Jugador","Puntos","Media","Puntos_Casa","Media_Casa","Puntos_Fuera","Media_fuera", "Valor", "Error")

# jugadores_df %>% 
#   rename_if(is.character, ~ c("Jugador","Puntos","Media","Puntos_Casa","Media_Casa","Puntos_Fuera","Media_fuera", "Valor", "Error")) %>% 
#   select(-Error) %>% 
#   mutate_all(~str_replace_all(., "\\.", "")) %>% 
#   mutate_all(~str_replace_all(., ",", ".")) %>% 
#   mutate_at(2:8, as.numeric) %>% 
#   drop_na() %>% View()


# Reada data from public API ----------------------------------------------

library(httr)
meso_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py/"
denver <- GET(url = meso_url,
              query = list(station = "DEN",
                           data = "sped",
                           year1 = "2022",
                           month1 = "1",
                           day1 = "1",
                           year2 = "2022",
                           month2 = "1",
                           day2 = "31",
                           tz = "America/Denver",
                           format = "comma")) %>%
  content() %>% 
  read_csv(skip = 5, na = "M") 

