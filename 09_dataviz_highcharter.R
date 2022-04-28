##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: highcharter
##########################################################################

library(tidyverse)
library("highcharter")

# scatter -----------------------------------------------------------------


hchart(mpg, "point", hcaes(x = displ, y = cty))
#  ó
highchart() %>% 
  hc_add_series(mpg, "point", hcaes(x = displ, y = cty))


# heatmap -----------------------------------------------------------------

dfdiam <- diamonds %>% 
  group_by(cut, clarity) %>%
  summarize(price = median(price))

hchart(dfdiam, "heatmap", hcaes(x = cut, y = clarity, value = price)) 


# treemap -----------------------------------------------------------------

mpgman <- mpg %>% 
  group_by(manufacturer) %>% 
  summarise(n = n(),
            unique = length(unique(model))) %>% 
  arrange(-n, -unique)

hchart(mpgman, "treemap", hcaes(x = manufacturer, value = n, color = unique))


# trend line --------------------------------------------------------------



highchart() %>% 
  hc_chart(type = "line") %>% 
  hc_title(text = "Monthly Average Temperature") %>% 
  hc_subtitle(text = "Source: WorldClimate.com") %>% 
  hc_xAxis(categories = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>% 
  hc_yAxis(title = list(text = "Temperature (C)")) %>% 
  hc_plotOptions(line = list(
    dataLabels = list(enabled = TRUE),
    enableMouseTracking = FALSE)
  ) %>% 
  hc_series(
    list(
      name = "Tokyo",
      data = c(7.0, 6.9, 9.5, 14.5, 18.4, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6)
    ),
    list(
      name = "London",
      data = c(3.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8)
    )
  )



# splider -----------------------------------------------------------------

highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "Budget vs Spending") %>% 
  hc_xAxis(categories = c("Sales", "Marketing", "Development",
                          "Customer Support",  "Information Technology",
                          "Administration"),
           tickmarkPlacement = "on",
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0) %>% 
  hc_series(
    list(
      name = "Allocated Budget",
      data = c(43000, 19000, 60000, 35000, 17000, 10000),
      pointPlacement = "on"
    ),
    list(
      name = "Actual Spending",
      data = c(50000, 39000, 42000, 31000, 26000, 14000),
      pointPlacement = "on"
    )
  )



# map ---------------------------------------------------------------------

getContent <- function(url) {
  library(httr)
  content(GET(url))
}

world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
world <- jsonlite::fromJSON(world, simplifyVector = FALSE)

volcano <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_volcanes&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")

highchart(type = "map") %>%
  hc_chart(backgroundColor = "#fff") %>%  
  hc_add_series(
    mapData = world,
    showInLegend = FALSE,
    nullColor = "#424242",
    borderWidth = 0) %>%   
  hc_add_series(
    data = volcano,
    type = "mappoint",
    color = hex_to_rgba("#f1c40f", 0.4),
    geojson = TRUE,
    name = "Volcanos",
    tooltip = list(pointFormat = "{point.properties.NOMBRE}"),
    marker = list(lineWidth = 0, radius = 2)
  )

