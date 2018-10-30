## Leaflet infografia ABREC 

library(leaflet)
library(ggmap)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(htmltools)
# Load data 

responses =  read.csv("data/Responses13AugRefined.csv")
head(responses)
names(responses)

# Columns with geografical data 
geoDat = c(7,8,15,16)
geoDat = responses[geoDat]
names(geoDat) = c("provinciaResidencia", "ciudadResidencia", 
                  "paisEstudios","ciudadEstudios")


geoDat$paisEstudios[geoDat$paisEstudios == "Inglaterra"] = "Reino Unido"


## Geocode with Google Maps API


geocodeFuntion = function(geoDat, column){ 

  
  loc = aggregate(geoDat, list(geoDat[,column]), FUN = length)[,1:2]
  
  loc1 = geocode(as.character(loc$Group.1))
  
  location = cbind(loc, loc1)

  names(location) =  c(names(geoDat)[column],"count", "lon", "lat")
  return(location)
}

provinciaResidencia = geocodeFuntion(geoDat, 1)
ciudadResidencia = geocodeFuntion(geoDat, 2)
paisEstudios = geocodeFuntion(geoDat, 3)
ciudadEstudios = geocodeFuntion(geoDat, 4)

## Arrange missing data 

### Fix manually (du'h)

which(is.na(provinciaResidencia$lon)) ## Ok 

# Fix Machala 
ciudadResidencia[which(is.na(ciudadResidencia$lon)),][c("lon","lat")] = c(-79.9960488,-3.2568589)

ciudadEstudios[which(is.na(ciudadEstudios$lon)),]
ciudadEstudios[ciudadEstudios$ciudadEstudios == "Austin",][c("lon","lat")] = c(-97.733527,30.274260)
ciudadEstudios[ciudadEstudios$ciudadEstudios == "Boston",][c("lon","lat")] = c(-71.077441,42.328223)
ciudadEstudios[ciudadEstudios$ciudadEstudios == "Budapest",][c("lon","lat")] = c(19.0991301,47.4985891)
ciudadEstudios[ciudadEstudios$ciudadEstudios == "Montpellier",][c("lon","lat")] = c(43.6100005,3.8039495)
ciudadEstudios[ciudadEstudios$ciudadEstudios == "Munich",][c("lon","lat")] = c(48.1548252,11.4014076)
ciudadEstudios[ciudadEstudios$ciudadEstudios == "Palencia",][c("lon","lat")] = c(42.0088108,-4.5620592)


# leaflet construction 






m = leaflet(options = leafletOptions(minZoom = 6, maxZoom = 9)) %>% setView(lng = -77, lat = -1.5, zoom = 7 ) 

map1 = m %>% addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    group = "Provincia Residencia",
    radius = ~(sqrt(count) * 3), 
    lat = ~lat,
    lng = ~lon,
    color = "#de2d26",
    popup = ~htmlEscape(paste(provinciaResidencia, "=", count)),
    data = provinciaResidencia) %>%
  addCircleMarkers(
    group = "Ciudad Residencia",
    radius = ~(sqrt(count) * 3), 
    lat = ~lat,
    lng = ~lon,
    color = "#2c7fb8",
    popup = ~htmlEscape(paste(ciudadResidencia, "=", count)),
    data = ciudadResidencia) %>%
addLayersControl(overlayGroups = c("Provincia Residencia", "Ciudad Residencia"),
                 options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Provincia Residencia") 

saveWidget(map1, file="residencia.html")

m1 = leaflet(options = leafletOptions(minZoom = 1, maxZoom = 12))  
map2 = m1 %>% addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addCircleMarkers(
    group = "Pais Estudios",
    radius = ~(sqrt(count) * 3), 
    lat = ~lat,
    lng = ~lon,
    color = "#de2d26",
    popup = ~htmlEscape(paste(paisEstudios, "=", count)),
    data = paisEstudios) %>%
  addCircleMarkers(
    group = "Ciudad Estudios",
    radius = ~(sqrt(count) * 3), 
    lat = ~lat,
    lng = ~lon,
    color = "#2c7fb8",
    popup = ~htmlEscape(paste(ciudadEstudios, "=", count)),
    data = ciudadEstudios) %>%
  addLayersControl(overlayGroups = c("Pais Estudios", "Ciudad Estudios"),
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Pais Estudios") 


saveWidget(map1, file="estudis.html")
