##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Databases
##########################################################################

library(tidyverse)
msleep


library(DBI)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")


dbListTables(con)
dbWriteTable(con, "msleep", msleep)
dbListTables(con)


dbListFields(con, "msleep")
dbReadTable(con, "msleep")

# You can fetch all results:
res <- dbSendQuery(con, "SELECT * FROM msleep WHERE vore = 'carni'")
df <- dbFetch(res)
dbClearResult(res)

df %>% 
  as_tibble()


dbGetQuery(con, "SELECT * FROM msleep", vore == 'carni')

dbGetQuery(
  con,
  "SELECT COUNT(*) FROM msleep WHERE vore = ?",
  params = list(c('carni', 'herbi'))
)

# Reading SQL statement from a file
df <- dbGetQuery(con, statement = readr::read_file('sql/sample_query.sql')) %>% 
  as_tibble()
df



# Warking in remote
msleep <- tbl(con, "msleep")
msleep

msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18))


msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18)) %>% 
  show_query()

# Collecting the data
sample_df <- msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18)) %>% 
  collect()


dbDisconnect(con)
