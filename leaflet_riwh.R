
library(leaflet)
library(htmltools)

#read in survey data
setwd("C:/Users/tim.gowan/Documents/Working/WhaleMap")
dat <- read.csv("Data_Review_Table_f618051.csv", header=TRUE)

#Convert date format
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")

# extract right whale sightings
riwh <- dat[dat$SPECCODE=='RIWH',]

# extract fin whale sightings (to test days without sightings)
fiwh <- dat[dat$SPECCODE=='FIWH',]

# start basemap
map <- leaflet() %>% 
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -80, lat = 30, zoom = 7) %>%
  
  # add layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add layers control
  addLayersControl(overlayGroups = c('Place names',
                                     'Points',
                                     'Lines',
                                     'Polygons'),
                   options = layersControlOptions(collapsed = FALSE),
                   position = 'topright')

map

# add scalebar and measurement tool
map <- map %>% 
  addScaleBar(position = 'topright') %>% 

  addMeasure(
    primaryLengthUnit = "kilometers",
    secondaryLengthUnit = 'miles', 
    primaryAreaUnit = "hectares",
    secondaryAreaUnit="acres", 
    position = 'topleft')


# add tracklines
map <- map %>%
  addPolylines(data=dat, ~Longitude, ~Latitude,
               weight = 2,
               color = 'red',
               opacity = 0.5,
               #popup = dat$Trackline, #this slows performance
               smoothFactor = 3,
               group = 'Lines')
map

#add right whale sightings

#label for sightings
labs <- lapply(seq(nrow(riwh)), function(i) {
  paste0('Date: ',  riwh[i, "Date"], '<br/>', 
         'Time: ',  substr(riwh[i, "Time"], start=12, stop=16), '<br/>', 
         'Lat, Long: ',  riwh[i, "Latitude"], ', ', riwh[i, "Longitude"],'<br/>', 
         'Species: ',  riwh[i, "SPECCODE"], '<br/>',
         'Number: ',  riwh[i, "NUMBER"], '<br/>',
         'Calves: ',  riwh[i, "NUMCALF"]) 
})

map <- map %>% 
  addCircles(data=riwh, ~Longitude, ~Latitude,
                   weight = 4,
                   col = 'black', 
                   fillColor = 'darkslategrey',
                   radius = 5, 
                   fillOpacity = 0.9, 
                   stroke = T, 
                   label = lapply(labs, HTML),
                   group = 'Points')

#add fin whale sightings

#label for sightings
labs <- lapply(seq(nrow(fiwh)), function(i) {
  paste0('Date: ',  fiwh[i, "Date"], '<br/>', 
         'Time: ',  substr(fiwh[i, "Time"], start=12, stop=16), '<br/>', 
         'Lat, Long: ',  fiwh[i, "Latitude"], ', ', fiwh[i, "Longitude"],'<br/>', 
         'Species: ',  fiwh[i, "SPECCODE"], '<br/>',
         'Number: ',  fiwh[i, "NUMBER"], '<br/>',
         'Calves: ',  fiwh[i, "NUMCALF"]) 
})

map <- map %>% 
  addCircles(data=fiwh, ~Longitude, ~Latitude,
             weight = 4,
             col = 'black', 
             fillColor = 'green',
             radius = 5, 
             fillOpacity = 0.9, 
             stroke = T, 
             label = lapply(labs, HTML),
             group = 'Points')

map
  


