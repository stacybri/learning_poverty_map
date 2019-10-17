library(tidyverse)
library(WDI)
library(leaflet)


#read in data from wbopendata
dat<-WDI(indicator=c( "SE.LPV.PRIM", "SE.LPV.PRIM.FE", "SE.LPV.PRIM.MA" ), start=2011, end=2019, extra=T) %>%
  filter(!is.na(SE.LPV.PRIM) & !is.na(Country_Co)) %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  filter(row_number()==n())

#do some processing on lat/long
dat <- dat %>%
  mutate(Country_Co=iso3c) 



#read in TopoJSON from World Bank
countries <- geojsonio::geojson_read("C:/Users/wb469649/OneDrive - WBG/GEAK/GeoSpatial/20160921_GAUL_GeoJSON_TopoJSON/TopoJSON/Default_Quantization/g2015_2014_0.json",
                                     what = "sp")

countries@data <- countries@data %>%
  left_join(dat) %>%
  mutate(prof=100-SE.LPV.PRIM)




#create pallete
bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
pal <- colorBin("RdYlGn", domain = countries@data$SE.LPV.PRIM, bins = bins, reverse=TRUE)



#create labels
labels <- sprintf(
  "<strong>%s</strong><br/> %g Overall Learning Poverty <br/> %g Male Learning Poverty <br/> %g Female Learning Poverty",
  countries@data$ADM0_NAME, round(countries@data$SE.LPV.PRIM, digits = 1), round(countries@data$SE.LPV.PRIM.MA, digits = 1), round(countries@data$SE.LPV.PRIM.FE, digits = 1)) %>% 
  lapply(htmltools::HTML)


m <- leaflet(countries) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~pal(SE.LPV.PRIM),
              popup=labels) %>%
  addLegend(pal=pal, values=~SE.LPV.PRIM, opacity=0.7, title="Learning Poverty", position="bottomright")

m

