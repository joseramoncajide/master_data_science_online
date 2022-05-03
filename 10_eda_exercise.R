##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: highcharter
##########################################################################

library(tidyverse)
library(nycflights13)
library(lares)
library(hrbrthemes)
library(ggridges)
theme_set(theme_ipsum())

airlines_data <- airlines
airports_data <- airports
flights_data <- flights
planes_data <- planes
weather_data <- weather


df_str(flights_data, return = "skimr", subtitle = NA, quiet = FALSE)
df_str(flights_data, return = "plot", subtitle = NA, quiet = FALSE)
# Exploratory data analysis -----------------------------------------------

# Ex.1
# Number of airline companies and flights per company


# Ex.2
# Number of different origins, different destinations and flights


# Ex.3
# Number of distinct routes: origin and dest combinations


# Ex.4
# Number of flights by departure airport


# Ex.5
# Add to the previous result the name of the airport




# Is there any particular trend of delays at all the airports or is it randomized?

# Ex.6
# Monthly number of delayed flights with origin in EWR


# Ex.7
# Add a column with the percentage number of delayed flights


# Ex.8
# Add a line plot with the absolute number of delayed flights


# Ex.10
# Plot the monthly number of delayed flights for all the origins in the same plot




# Are the delays related with an increase in the number of carriers flying those months?

# Ex.11
# Get the number of different carriers operating from EWR


# Ex.12
# Get the number of different carriers operating from EWR per month and show them using a bar plot: geom_col()


# Ex.13
# Using the previous resulta, split it by origin


# Which carriers have been the top and the bottom performers in 2013?
# The performance of the carrier can be gauged by (1) what percentage of flights of a particular carrier are
# delayed in departure and also delayed in arrival and (2) what is the average delay in arrival time for each of
# the carrier over the year of 2013.

# Ex.14
# Delayed: Filter delayed flights and get the number of flights per carrier


# Ex.15
# All flights: Repeat the same but filtering out flights with no information about  dep_delay

# Ex.16
# Join the results in Ex.14 and Ex.15 and calculate the ratio of delayed Vs all flights


# Ex.17
# Replace carrier with it's full name

# Ex.18
# Calculate and plot the Average Arrival Delay for each Carrier: geom_col

# Ex.19
# Calculate and plot the distribution of departure delays per origin: geom_boxplot


# Ex.20
# Plot a histogram of wind speed for all flights



# Ex.21
# Plot a histogram of wind speed for all flights without the outliers: you can use the quantile() function


# Ex.22
# Make a boxplot of the distribution of wind_speed without the outliers. Per hour and facets by origin


# Ex.23
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
# Using the ggridges package and the geom_density_ridges plot the temperature distribution by month



# Ex.24
# Find all the not delayed flights departing from 'JFK' and 'LGA' to 'ORD', 'ATL', 'LAX'
# For each route calculate the total delay, remove the outliers by total delay 
# Plot the distribution using geom_density_ridges() by route




# How many flights were there from JFK airports to Seattle in 2013

# Ex.25
# Search the IATA code in the airports_data using R and save it in a vector


# Ex.26
# How many unique air planes fly from JFK to Seattle?
# What is the average arrival delay for flights from NYC to Seattle? (Only for flights with arrival delay > 0)


# Ex.27
# What proportion of flights to Seattle come from each NYC airport? In what proportion



# Study Flight Delays with Weather data
# Does visibility affected flight delays?

# Ex.28
# Filter to get only flights with departure delay and get, buy origin and time_hour, the number of delayed flights and the average delay.
# Join the results with weather_data to get the weather conditions for that flights


# Ex.29
# Exclude perfect visibility conditions and calculate the average delay and the average number of flights


# Ex.29
# Visibility vs. Average Departure Delay
# Plot the average delay Vs visibility into a scatter plot geom_point() to check if there is a relationship between those variables.
# Add a geom_smooth layer to describe what is happening.


# Ex.30
# Visibility vs. Average Number of Delays
# Plot the Average Number of Delays Vs visibility into a scatter plot geom_point() to check if there is a relationship between those variables.
# Add a geom_smooth layer to describe what is happening.


# Ex.31
# Find the origin airport most sensible to visibility conditions
# Find the origin airport with the biggest correlation between visibility and average departure delay 
# Exclude perfect visibility conditions



# Maps --------------------------------------------------------------------

# USA map
map_data("state") %>% 
  # filter(long > -140) %>% 
  ggplot() + 
  geom_polygon(mapping=aes(x=long,y=lat,group=group),color="white",fill="grey")


# Add the 3 worst performing destination airports to the previous plot

# Ex.31
# Get all delayed flights, group by dest and calculate de average arr_delay
# Add data with their geolocation


# Ex.32
# Filter the 3 top delayed flights


# Ex.33
# Add the points to the map plot


# Ex.34
# How many pax could be affected by delays in those three airports.
# Make a final data frame for a report, as easy to understand as possible with information of:
# The maximum number of passangers carried by each airline to the previous airports


  
