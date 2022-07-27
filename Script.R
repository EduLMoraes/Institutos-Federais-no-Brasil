library("geobr")
library("leaflet")
library("rgdal")
library("dplyr")
library("tidygraph")
library("sf")

#pegando os dados
institutos <- read_schools()
estados <- read_state(year = 2020)



#trabalhando os dados
institutosF <- subset(institutos, institutos$government_level == "Federal")
institutosF$abrev <- substr(institutosF$name_school, 1,2)

campusIF <- subset(institutosF, institutosF$abrev == "IF")
campusIF <- campusIF[,-c(2, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18)]
campusIF$code_school <- substr(campusIF$code_school, 1,2)

head(campusIF)

## Coletando a longitude e latitude do geom
## Usando o sf
CampusIF <- st_as_sf(campusIF, coords = c("long","lat")) %>% 
  st_set_crs(4326)
CampusIF_coords <- unlist(st_geometry(CampusIF)) %>% 
  matrix(ncol=2,byrow=TRUE) %>% 
  as_tibble() %>% 
  setNames(c("long","lat"))
campusIF <- data.frame(campusIF, CampusIF_coords)
campusIF <- subset(campusIF, campusIF$long != "NaN")

#preparando o que irá aparecer ao gerar o popup
label <- sprintf(
  "<strong>%s</strong></br>
  <strong>Endereço:</strong>%s</br>
  <strong>Phone:</strong>%s</br>
  <strong>x:</strong>%s</br>
  <strong>y:</strong>%s",
  campusIF$name_school,
  campusIF$address,
  campusIF$phone_number,
  campusIF$long,
  campusIF$lat
) %>% lapply(htmltools::HTML)



#criando o mapa cru
mapa <- leaflet(estados) %>% addTiles()

#adicionando os pontos e customizando o mapa
mapa %>% addPolygons(
              weight = 0.5,
              opacity = 1,
              color = "green",
              dashArray = "1",
              fillOpacity = 0.2,
          ) %>% 
          addMarkers(
              lng = campusIF$long,
              lat = campusIF$lat,
              popup = label,
              popupOptions = popupOptions(
                                          maxWidth = 200,
                                          minWidth = 50,
                                          style = list(
                                                        "font-weight" = "normal",
                                                        padding = "3px 8px"),
                                                        direction = "auto"
                                          ),
              clusterOptions = markerClusterOptions(),
              clusterId = ~campusIF$abbrev_state,
              group = "addMarkers"
          ) %>%
          addCircleMarkers(
              lng = campusIF$long,
              lat = campusIF$lat,
              label = campusIF$name_school,
              popup = label,
              popupOptions = popupOptions(
                                  maxWidth = 200,
                                  minWidth = 50,
                                          style = list(
                                            "font-weight" = "normal",
                                            padding = "3px 8px"),
                                  direction = "auto"
                              ),
              clusterOptions = markerClusterOptions(),
              group = "addCircleMarkers"
          ) %>%
        # dando opções de markadores
          addLayersControl(
              baseGroups = c("addMarkers","addCircleMarkers"), 
              options = layersControlOptions(collapsed = T)
          )