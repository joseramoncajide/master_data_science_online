##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Datawrangling with Spark
##########################################################################

library(sparklyr)
library(dplyr)
library(ggplot2)
spark_install(version = '2.4.3')

# Connect to Spark
sc <- spark_connect(master="local")

# Clusters
# sc <- spark_connect(master = "yarn") # Hadoop
# sc <- spark_connect(master = "k8s://https://server") #Kubernetes ...

flights_tbl <- copy_to(sc, nycflights13::flights, "flights")

airlines_tbl <- copy_to(sc, nycflights13::airlines, "airlines")

flights_tbl %>% 
  select(year:day, arr_delay, dep_delay)

flights_tbl %>% 
  filter(dep_delay > 1000)

# Carriers stats
flights_tbl %>% 
  group_by(carrier) %>%
  summarize(
    count = n(), 
    mean_dep_delay = mean(dep_delay, na.rm = TRUE),
    max_dep_delay = max(dep_delay, na.rm = TRUE)
  ) -> carrier_stats 

# Collect results
carrier_stats_df <- 
  carrier_stats %>% 
  collect()

# Peforming Joins

flights_tbl %>% 
  left_join(airlines_tbl, by = "carrier") %>% 
  select(name, flight, dep_time)


spark_disconnect(sc)  
