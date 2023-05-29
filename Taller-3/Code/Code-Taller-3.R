#Juan Camilo Cifuentes Mogollon 202011853
#Santiago Rodrigues 202013402
# R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid"
#Taller 3 
rm(list = ls())
library(tmaptools)
library(lmtest)
library(sandwich)
library(stargazer)
library(flextable)
library(readxl)
library(ggplot2)
library(officer)
require(pacman)
require(rio)
require(tidyverse)
require(dplyr)
p_load(pacman,
       rio,
       tidyverse, 
       dplyr,
       arrow,
       broom,
       mfx,
       margins,
       estimatr,
       lmtest,
       fixest,
       modelsummary,
       stargazer,
       skimr,
       leaflet,
       tmaptools,
       ggsn,
       ggmap, 
       osmdata
       )
getwd()
list.files()
require(sf)
require(leaflet)
require(rvest)
require(xml2)
require(osmdata)
require(ggsn)
require(openxlsx)
library(tm)
library(wordcloud2)
#-----------------------------------------------------------------
#1) Regresiones
#1.1) Estimaciones

datos_vivienda <- read_rds("input/data_regresiones.rds")
datos_vivienda_limpios <- datos_vivienda[complete.cases(datos_vivienda),]

#Todas las observaciones estan completas, se continua con la estimación depues de verificar

modelo_1 <- lm_robust(data = datos_vivienda, price ~ rooms + bathrooms + surface_total)
summary(modelo_1)
summary(modelo_1)$r.squared
summary(modelo_1)$adj.r.squared
#Se crea un nuevo data frame con las variables categoricas en forma de dummys para la nueva regresión
datos_vivienda_dummy <- datos_vivienda
valores_property_type <- unique(datos_vivienda_dummy$property_type)
print(valores_property_type)
#Se confirma que solo hay Casas y Apartamentos
datos_vivienda_dummy <- datos_vivienda
datos_vivienda_dummy <- within(datos_vivienda_dummy,{
  casa <- ifelse(property_type == "Casa",1,0)
})
datos_vivienda_dummy <- within(datos_vivienda_dummy,{
  apartamento <- ifelse(property_type == "Apartamento",1,0)
})

modelo_2 <- lm_robust(data = datos_vivienda_dummy, price ~ rooms + bathrooms + surface_total + apartamento)
summary(modelo_2)

#Se crea un nuevo modelo para las elasticidades y la distancia al centro de negocios
datos_vivienda_dummy$log_price <- log(datos_vivienda_dummy$price)
datos_vivienda_dummy$log_rooms <- log(datos_vivienda_dummy$rooms)
datos_vivienda_dummy$log_bathrooms <- log(datos_vivienda_dummy$bathrooms)
datos_vivienda_dummy$log_distanceCBD <- log(datos_vivienda_dummy$dist_cbd)
datos_vivienda_dummy$log_area <- log(datos_vivienda_dummy$surface_total)

any(!is.finite(datos_vivienda_dummy$log_price))
any(!is.finite(datos_vivienda_dummy$log_rooms))
any(!is.finite(datos_vivienda_dummy$log_bathrooms))
any(!is.finite(datos_vivienda_dummy$log_area))
any(!is.finite(datos_vivienda_dummy$log_distanceCBD))
#Se eliminan los datos atipicos de la variable dependiente (Log_price)

datos_vivienda_dummy_2 <- datos_vivienda_dummy[is.finite(datos_vivienda_dummy$log_price),]

#Sigue siendo relevante la regresión debido a que solo se eliminaron 3 observaciones

modelo_3 <- lm_robust(data = datos_vivienda_dummy_2, log_price ~ log_rooms + log_bathrooms + log_area + log_distanceCBD + apartamento)
summary(modelo_3)

#1.2) Presentar resultados

modelo_consolidado <- msummary(list(modelo_1, modelo_2, modelo_3))

modelos <- list("Modelo_Int-Int" = modelo_1, "Modelo_Int-Int/dummy" = modelo_2,
                "Modelo Log-log" = modelo_3)
modelplot(modelos) + coord_flip()+
  labs(title = "Precio de la casa", subtitle = "Comparando 3 modelos")

#1.3) Exportar resultados 

plot_modelos <- modelplot(modelos) + coord_flip()+
  labs(title = "Precio de la casa", subtitle = "Comparando 3 modelos")

ggsave(file = "Output/plot_regresiones.png", plot = plot_modelos, 
       device = "png",
       width = 10,
       height = 8)


coeficientes_consolidados <- data.frame(regresion_1 = ifelse(length(coef(modelo_1)), coef(modelo_1), NA),
                                        regresion_2 = ifelse(length(coef(modelo_2)), coef(modelo_2), NA),
                                        regresion_3 = ifelse(length(coef(modelo_3)), coef(modelo_3), NA))

openxlsx::write.xlsx(coeficientes_consolidados, file = "Output/resultados_regresiones.xlsx", row.names = TRUE)

#---------------------------------------------------------------------------------------
#2) Datos Espaciales 

#2.1)Descargar datos

amenities_bogota <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "amenity", value = "restaurant" )
class(amenities_bogota)

amenities_bogota_sf <- amenities_bogota %>% osmdata_sf()

restaurantes <- amenities_bogota_sf$osm_points
restaurantes <- dplyr::select(restaurantes, amenity, osm_id)

amenities_bogota_2 <- opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key = "leisure", value = "park" )

amenities_bogota_2_sf <- amenities_bogota_2 %>% osmdata_sf()

parques <- amenities_bogota_2_sf$osm_polygons
parques <- dplyr::select(parques, leisure, osm_id)

#2.2) Visualización 
leaflet() %>% addTiles() %>% addCircleMarkers(data = restaurantes, color = "blue") %>%
  addPolygons(data = parques, color = "red")


#2.3) Geocodificar Direcciones

uniandes <- geocode_OSM("Carrera 1 %23% 18A-12, Bogotá", as.sf = T)

#2.4) Exportar Mapa

map <- ggplot()+geom_sf(data = parques, color = "red") + geom_sf(data = restaurantes, color = "blue") +
  geom_sf(data = uniandes, color = "black")+theme_linedraw() + scalebar(data = parques, dist = 5, transform = T, dist_unit = "km") + 
  north(data = parques, location = "topright") 

ggsave(file = "Output/mapa_amenities.png", plot = map, device = "png")

#---------------------------------------------------------------------------------

#3) WEB-SCRAPPING Y PROCESAMIENTO DE TEXTO 

#3.1) 

url_trabajo <- "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"

objeto_html <- read_html(url_trabajo)

#3.2)

titulo <- objeto_html %>% html_nodes(xpath = '//*[@id="firstHeading"]/span') %>%
  html_text()
titulo

#3.3)
tabla_departamentos <- objeto_html %>%
  html_table()

openxlsx::write.xlsx(tabla_departamentos, file = "Output/tabla_departamentos.xlsx", rowNames = TRUE)

#3.4)

parrafos <- objeto_html %>% html_elements("p")

nps_corpus <- Corpus(VectorSource(parrafos))
inspect(nps_corpus)

nps_corpus_limpio <- nps_corpus %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeNumbers)
  
inspect(nps_corpus_limpio)
inspect(nps_corpus)

nps_matris <- TermDocumentMatrix(nps_corpus)

nps_matrix <- as.matrix(nps_matris)
nps_freq <- sort(rowSums(nps_matrix), decreasing = T)
nps_data <- data.frame(words = names(nps_freq), nps_freq)

wordcloud2(nps_data)










