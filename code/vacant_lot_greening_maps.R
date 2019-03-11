library(tidyverse)
library(sf)
library(mapview)
library(leaflet)
library(htmltools)

setwd("C:/Users/ajjit/Google Drive/Documents/vacant_lots_final/vacant-lots-project")

lots = readRDS("final_data/lots_balanced.rds") %>% 
  st_as_sf() %>%  
  st_transform(4326) %>%
  mutate(year = lubridate::year(date_season_begin))


lots_greened = lots %>% filter(greened)  
lots_ungreened = lots %>% filter(!greened)
phl_city_bounds = st_read("http://data.phl.opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson") %>% st_transform(4326)

#weird leaflet issue where if geometry calumned is named, it won't plot
names(phl_city_bounds$geometry) = NULL

#hacking our way to titles on Leaflet plots
g_title <- tags$span(tags$style("span {color: black; font-size:22px}"),
                     tags$b("Greened Lots"))

#setting a color palette for years spanned in the data
pal <- colorNumeric(
  palette = "viridis",
  domain = c(2008:2017))



greened_lots_map = leaflet(lots_greened, options = leafletOptions(zoomControl = FALSE)) %>%
 addProviderTiles('CartoDB.Positron') %>%
 addCircles(
            opacity = 0.5,
            fill =T,
            radius = 200,
            stroke = F,
            color = ~pal(year)) %>%
  addPolygons(data = phl_city_bounds,
            weight = 2.5,
            fill= F, 
            color = "black") %>%
  addLegend("bottomright", pal=pal, values = c(2008:2017), title = "Year", 
            labFormat = labelFormat(big.mark= ""), opacity = 1) %>%
  addControl(g_title, position = "topleft") 



g_title <- tags$span(tags$style("span {color: black; font-size:22px}"),
                  tags$b("Ungreened Lots"))

ungreened_lots_map = leaflet(lots_ungreened, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(
             opacity = 0.4,
             fill =T,
             radius = 150,
             stroke = F,
             color = ~pal(year)) %>%
  addPolygons(data = phl_city_bounds,
              weight = 2.5,
              fill= F, 
              color = "black") %>%
  addLegend("bottomright", pal=pal, values = c(2008:2017), title = "Year", 
            labFormat = labelFormat(big.mark= ""), opacity = 1) %>%
  addControl(g_title, position = "topleft") #%>%
  # addMiniMap(
  #   tiles = providers$CartoDB.Positron,
  #   toggleDisplay = TRUE,
  #   position = "topright")




## use lattice veiw to get side by side leaflet maps, then take screenshot
latticeview(greened_lots_map, ungreened_lots_map)



