#This is a simple script to pull learning poverty data from the World Bank API and map the data using leaflet
#links to the shape file used is embedded in the code.
# Author: Brian Stacy 10/17/2019

library(tidyverse)
library(WDI)
library(leaflet)
library(htmlwidgets)

#Set directory to save work
save_dir<-"C:/Users/wb469649/Documents/Github/learning_poverty_map"

#list of indicators
ind_list <- c( "SE.LPV.PRIM", "SE.LPV.PRIM.FE", "SE.LPV.PRIM.MA", "SE.LPV.PRIM.OOS",  "SE.LPV.PRIM.OOS.FE", "SE.LPV.PRIM.OOS.MA",
               "SE.LPV.PRIM.BMP", "SE.LPV.PRIM.BMP.FE", "SE.LPV.PRIM.BMP.MA")
#read in data from wbopendata
dat<-WDI(indicator=ind_list, start=2011, end=2019, extra=T) %>%
  filter(!is.na(SE.LPV.PRIM) & !is.na(country)) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  filter(row_number()==n())

#do some processing on lat/long
dat <- dat %>%
  mutate(ISO_A3=iso3c) 



#read in TopoJSON polygon file 
#downloaded https://datahub.io/core/geo-countries#data

shape_dir<-"C:/Users/wb469649/Documents/Github/learning_poverty_map"

countries <- geojsonio::geojson_read(paste(shape_dir,"countries.geojson", sep="/"),
                                     what = "sp")

countries@data <- countries@data %>%
  left_join(dat) %>%
  mutate(prof=100-SE.LPV.PRIM)




#create pallete
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
pal <- colorBin("RdYlGn", domain = countries@data$SE.LPV.PRIM, bins = bins, reverse=TRUE)



#create labels
labels <- sprintf(
  "<strong>%s</strong><br/> <hr size=2>
  <strong> %g%% </strong> Overall Learning Poverty <br/> 
  <strong> %g%% </strong> Male Learning Poverty <br/> 
  <strong> %g%% </strong> Female Learning Poverty <br/> <hr size=1>
  <strong> %g%% </strong> Overall Children Out of School <br/> 
  <strong> %g%% </strong> Male Children Out of School <br/> 
  <strong> %g%% </strong> Female Children Out of School <br/> <hr size=1>
  <strong> %g%% </strong> Overall Pupils below minimum reading proficiency    <br/> 
  <strong> %g%% </strong> Male Pupils below minimum reading proficiency                             <br/>
  <strong> %g%% </strong> Female Pupils below minimum reading proficiency ",
  countries@data$ADMIN, round(countries@data$SE.LPV.PRIM, digits = 1), round(countries@data$SE.LPV.PRIM.MA, digits = 1), round(countries@data$SE.LPV.PRIM.FE, digits = 1),
                            round(countries@data$SE.LPV.PRIM.OOS, digits = 1), round(countries@data$SE.LPV.PRIM.OOS.MA, digits = 1), round(countries@data$SE.LPV.PRIM.OOS.FE, digits = 1),
                            round(countries@data$SE.LPV.PRIM.BMP, digits = 1), round(countries@data$SE.LPV.PRIM.BMP.MA, digits = 1), round(countries@data$SE.LPV.PRIM.BMP.FE, digits = 1)
  
                            ) %>% 
  lapply(htmltools::HTML)


m <- leaflet(countries) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~pal(SE.LPV.PRIM),
              popup=labels) %>%
  addLegend(pal=pal, values=~SE.LPV.PRIM, opacity=0.7, title="Learning Poverty", position="bottomright")

m
saveWidget(m, file=paste(save_dir,"learning_poverty_map.html", sep="/"))

