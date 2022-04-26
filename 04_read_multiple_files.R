library(tidyverse)
library(rvest)

html_doc<- read_html("http://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=33cb30c367e78410VgnVCM1000000b205a0aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD")

files_links <- html_doc %>%
  html_nodes("a") %>% 
  html_attr("href") %>%  
  str_subset("\\.zip") %>% 
  head(2)

# Let's start creating a local directory for all the files
dir.create('datos_accidentes')

# Download 1 file only
i <- 1
file_link <-  files_links[i]
url <- paste("http://datos.madrid.es", file_link, sep = "")
cat(url)
file <- basename(url)
download.file(url, file)
unzip(file, list=TRUE)
csv_file_name <-  unzip(file, list=TRUE)$Name
unzip(file, files=csv_file_name, exdir="./datos_accidentes", overwrite=TRUE)
unlink(file)

# Downloading all files

# Do you remember?
my_vector <- files_links
for (i in 1:(length(my_vector))) {
  print(i)
  print(my_vector[i])
}

# Exercise: apply the same to the previous code in order to download all the files in files_links

# Sol:







# Exercise. 
# Modify the previous code to avoid download a file if already present on the download folder

for (i in 1:(length(files_links))) {
 #...
  
  
  
  
}








# Using a custom function

download_traffic_data <- function(file_link) {
  url <- paste("http://datos.madrid.es", file_link, sep = "")
  cat(url)
  file <- basename(url)
  download.file(url, file)
  unzip(file, list=TRUE)
  csv_file_name <-  unzip(file, list=TRUE)$Name
  unzip(file, files=csv_file_name, exdir="./datos_accidentes", overwrite=TRUE)
}

download_traffic_data(file_link = files_links[1])

s1 <- system.time({
  lapply(files_links, download_traffic_data)
})
s1

# Speed it up
library(parallel)
detectCores()
detectCores(logical = FALSE)

s2 <- system.time({
  mclapply(files_links, download_traffic_data, mc.cores = 2)
})
s2

# Time difference
s1 - s2

# read data ---------------------------------------------------------------
list_of_files <- list.files(path = "./datos_accidentes",
                            recursive = FALSE,
                            pattern = "\\.csv$",
                            full.names = TRUE)


trafico_df <- read_delim(list_of_files, 
                 id = "file_name", delim = ';')

# Intensidad   del   Punto   de   Medida   enel   periodo   de   15   minutos (vehículos/hora)
# Ocupación: Tiempo  de  Ocupación  del  Punto  de  Medida  en  el  periodo  de  15  minutos (%). 
# Carga: Carga  de  vehículos  en  el  periodo  de  15 minutos.  Parámetro  que  tiene  en  cuenta intensidad, ocupación y capacidad de la vía y establece el grado de uso de la vía de 0 a 100. 
# vmed: Velocidad  media  de  los  vehículos  en  el periodo  de  15  minutos  (Km./h). Sólo para puntos de medida interurbanos M30. 





