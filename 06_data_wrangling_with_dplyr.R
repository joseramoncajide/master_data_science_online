##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Datawrangling
##########################################################################

library(tidyverse)

#built-in R dataset 
glimpse(msleep)

head(msleep)
tail(msleep)
dim(msleep)
str(msleep)
names(msleep)



# Useful functions when working with data frames --------------------------

mean(msleep$sleep_total)      # Mean
median(msleep$sleep_total)    # Median
max(msleep$sleep_total)       # Max
min(msleep$sleep_total)       # Min
sd(msleep$sleep_total)        # Standard deviation
var(msleep$sleep_total)       # Variance
quantile(msleep$sleep_total)  # Various quantiles

boxplot(msleep$sleep_total)

hist(msleep$sleep_total)

table(msleep$vore)
barplot(table(msleep$vore))

proportions(table(msleep$vore))
# Counts:
table(msleep$vore, msleep$conservation)
# Proportions, per row:
proportions(table(msleep$vore, msleep$conservation), margin = 1)
# Proportions, per column:
proportions(table(msleep$vore, msleep$conservation), margin = 2)

plot(msleep$sleep_total, msleep$sleep_rem, pch = 1)
grid()

# NAs

is.na(msleep$sleep_rem[1])
is.na(msleep$sleep_rem)

# count the number of missing values for each variable
colSums(is.na(msleep))

# Total missing values
sum(is.na(msleep))

#install.packages("naniar")
library(naniar)

## visualize 
vis_miss(msleep)

# Summarizing Your Data
#install.packages("skimr")
library(skimr)
skim(msleep)

# Selecting columns -------------------------------------------------------


# Selecting columns
# Selecting columns: the basics

msleep %>%
  select(name, genus, sleep_total, awake)

msleep %>%
  select(name:order, sleep_total:sleep_cycle)

#  deselect columns 
msleep %>% 
  select(-conservation, -(sleep_total:awake))

# deselect a whole chunk, and then re-add a column again
msleep %>%
  select(-(name:awake), conservation)

# Selecting columns based on partial column names
msleep %>%
  select(name, starts_with("sleep"))

msleep %>%
  select(contains("eep"), ends_with("wt"))

# Selecting columns based on regex
msleep %>%
  select(matches("o.+er"))

# Selecting columns based pre-identified columns
classification <- c("name", "genus", "vore", "order", "conservation")

msleep %>%
  select(!!classification)

# Selecting columns by their data type
msleep %>%
  select_if(is.numeric)

# Negation:  add a tilde to indicate a function
msleep %>%
  select_if(~!is.numeric(.))

# Selecting columns by logical expressions
msleep %>%
  select_if(is.numeric) %>%
  select_if(~mean(., na.rm=TRUE) > 10)

# Or shorter:
msleep %>%
  select_if(~is.numeric(.) & mean(., na.rm=TRUE) > 10)

# return the columns that have less than 10 distinct answers for instance
length(unique(msleep$vore)) < 10
length(unique(msleep$genus)) < 10

msleep %>%
  select_if(~n_distinct(.) < 10)

# Re-ordering columns
msleep %>%
  select(conservation, sleep_total, name)

# moving a few columns to the front
msleep %>%
  select(conservation, sleep_total, everything())


# Column names
# Renaming columns

msleep %>%
  select(animal = name, sleep_total, extinction_threat = conservation)

# to retain all columns
msleep %>% 
  rename(animal = name, extinction_threat = conservation)

# Reformatting all column names
msleep %>%
  select_all(toupper)

# replace all white spaces with an underscore
# Example
msleep2 <- select(msleep, name, sleep_total, brainwt)
colnames(msleep2) <- c("name", "sleep total", "brain weight")
msleep2

msleep2 %>%
  select_all(~str_replace(., " ", "_"))

# Row names to column
mtcars %>% 
  head() 

mtcars %>%
  rownames_to_column("car_model") %>% 
  head()

# Convert dataframe to tibble

mtcars %>% 
  as_tibble()

mtcars %>% 
  as_tibble(rownames = 'car_model')


# manipulate --------------------------------------------------------------

msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_min = sleep_total * 60)

# New columns can be made with aggregate functions such as average, median, max, min, sd, ..
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_vs_AVG = sleep_total - round(mean(sleep_total), 1),
         sleep_total_vs_MIN = sleep_total - min(sleep_total))


msleep %>%
  select(name, brainwt) %>%
  mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
  arrange(desc(brainwt))


msleep %>%
  select(name) %>%
  mutate(name_last_word = tolower(str_extract(name, pattern = "\\w+$")))

# Mutating several columns at once

msleep %>%
  mutate_all(toupper)

msleep_ohno <- msleep %>%
  mutate_all(~paste(., "  /n  "))

msleep_ohno


msleep_corr <- msleep_ohno %>%
  mutate_all(~str_replace_all(., "/n", "")) %>%
  mutate_all(str_trim)

msleep_corr[,1:4]

# Mutate if
msleep %>%
  select(name, sleep_total:bodywt) %>%
  mutate_if(is.numeric, round)

# Mutate at to change specific columns
msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60))


msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60)) %>%
  rename_at(vars(contains("sleep")), ~paste0(.,"_min"))


# Working with discrete columns
# Recoding discrete columns

msleep %>% 
  distinct(conservation)

msleep %>% 
  count(conservation)


msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)

# change NA into something other than NA by adding a .missing argument 
msleep %>%
  mutate(conservation2 = recode_factor(conservation,
                                       "en" = "Endangered",
                                       "lc" = "Least_Concern",
                                       "domesticated" = "Least_Concern",
                                       .default = "other",
                                       .missing = "no data",
                                       .ordered = TRUE)) %>%
  count(conservation2)


# Creating new discrete column (two levels)
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_time = ifelse(sleep_total > 10, "long", "short")) 

# Creating new discrete column (multiple levels)
msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr = case_when(
    sleep_total > 13 ~ "very long",
    sleep_total > 10 ~ "long",
    sleep_total > 7 ~ "limited",
    TRUE ~ "short")) 


# case_when: can be used for grouping across columns

msleep %>%
  mutate(silly_groups = case_when(
    brainwt < 0.001 ~ "light_headed",
    sleep_total > 10 ~ "lazy_sleeper",
    is.na(sleep_rem) ~ "absent_rem",
    TRUE ~ "other")) %>%
  count(silly_groups)

# merging and splitting columns
msleep %>% 
  unite(united_col, genus, order, sep=": ")  -> msleep2

msleep2 %>% 
  separate(united_col, into = c("a", "b"), sep = ": ")



# Long Vs Wide ------------------------------------------------------------

msleep %>%
  select(name, contains("sleep")) %>%
  gather(key = "sleep_measure", value = "time", -name)


msleep %>%
  select(name, contains("sleep")) %>%
  pivot_longer(cols  = contains('sleep'), names_to = "sleep") -> msleep_long

# Exercise: remove "sleep_" from sleep variable/column

msleep %>%
  select(name, contains("sleep")) %>%
  pivot_longer(cols  = contains('sleep'), names_to = "sleep") %>% 
  mutate(sleep = str_replace_all(sleep, "sleep_", "")) -> msleep_long


# Long to wide

msleep_long %>% 
  pivot_wider(names_from = sleep, values_from = value, names_prefix = 'sleep_')


# Turning data into NA ----------------------------------------------------

msleep %>%
  select(name:order) %>%
  na_if("Cheetah")

msleep %>% 
  select(name:order) %>% 
  replace_na(list(vore = 'Unknown'))

msleep %>% 
  select(name:order) %>% 
  replace_na(list(vore = 'Unknown')) %>% 
  na_if("Unknown")



# Filtering rows ----------------------------------------------------------

# Basic row filters

# Filtering rows based on a numeric variable

msleep %>% 
  select(name, sleep_total) %>% 
  filter(sleep_total > 18)

# Range
msleep %>% 
  select(name, sleep_total) %>% 
  filter(between(sleep_total, 16, 18))

# select all code that is nearly a given value with a tolerance
msleep %>% 
  select(name, sleep_total) %>% 
  filter(near(sleep_total, 17, tol = sd(sleep_total)))

# Filtering based on a exact character variable matches

msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order == "Didelphimorphia")

msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order != "Rodentia")

# Select the rows with a name in the alphabet after the letter v.
msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(name > "v")

msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(order %in% c("Didelphimorphia", "Diprotodontia"))

remove <- c("Rodentia", "Carnivora", "Primates")
msleep %>% 
  select(order, name, sleep_total) %>% 
  filter(!order %in% remove)

# Filtering rows based on regex

msleep %>% 
  select(name, sleep_total) %>% 
  filter(str_detect(tolower(name), pattern = "mouse"))

# Filtering based on multiple conditions

msleep %>% 
  select(name, order, sleep_total:bodywt) %>% 
  filter(bodywt > 100, (sleep_total > 15 | order != "Carnivora"))

msleep %>% 
  select(name, sleep_total, brainwt, bodywt) %>% 
  filter(brainwt > 1, !bodywt > 100)

# Filtering out empty rows

msleep %>% 
  select(name, conservation:sleep_cycle) %>% 
  filter(!is.na(conservation))

msleep %>% 
  select(name, conservation:sleep_cycle) %>% 
  drop_na(conservation) # tidyr

# Filtering across multiple columns

msleep %>% 
  select(name:order, sleep_total, -vore) %>% 
  filter_all(any_vars(str_detect(., pattern = "Ca")))

# any_vars = OR
msleep %>%  
  select(name, sleep_total:bodywt) %>% 
  filter_all(any_vars(. < 0.1))

# all_vars = AND
msleep %>%  
  select(name, sleep_total:bodywt, -awake) %>% 
  filter_all(all_vars(. > 1))

msleep %>% 
  select(name:order, sleep_total:sleep_rem) %>% 
  filter_if(is.character, any_vars(is.na(.)))


msleep %>% 
  select(name, sleep_total:sleep_rem, brainwt:bodywt) %>% 
  filter_at(vars(sleep_total, sleep_rem), all_vars(.>5))



# Summarizing and slicing data --------------------------------------------

# Counting the number of observations
msleep %>%
  count(order, sort = TRUE)

msleep %>%
  count(order, vore, sort = TRUE)

# Exercise
msleep %>%
  count(order, vore, sort = TRUE) %>% 
  pivot_wider(names_from = 'vore', values_from = 'n',values_fill = 0)


# Summarising data

msleep %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

# group_by

msleep %>%
  group_by(vore) %>%
  summarise(n = n(), average = mean(sleep_total), maximum = max(sleep_total))

# Exercise: amount of sleep as a fraction of a day.
msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep_day = mean(sleep_total)/24)


msleep %>%
  group_by(vore) %>%
  summarise_all(mean, na.rm=TRUE)

# Better:
msleep %>%
  group_by(vore) %>%
  summarise_if(is.numeric, mean, na.rm=TRUE)



# Arranging rows
msleep %>%
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total)) %>%
  arrange(desc(avg_sleep))

msleep %>%
  select(order, name, sleep_total) %>%
  group_by(order) %>%
  arrange(desc(sleep_total), .by_group = TRUE)


# Showing only part of the data -------------------------------------------

# The 5 lowest and highest values

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(5)

msleep %>%
  group_by(order) %>%
  summarise(average = mean(sleep_total)) %>%
  top_n(-5)

# A random selection of rows
msleep %>%
  sample_frac(.1)

# user-defined slice of rows
msleep %>%
  slice(50:55)



# Duplicated rows ---------------------------------------------------------
msleep %>% 
  distinct()

