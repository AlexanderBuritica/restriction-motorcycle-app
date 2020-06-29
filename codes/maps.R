
### Setup
cat("\f")
rm(list = ls())
setwd("~/Dropbox/Michael Weintraub/Restriccion Motos/")
listpackages <- c("tidyverse","rgeos","rgdal","maptools","geosphere")
sapply(listpackages,require,character.only=TRUE)

### Functions
read_shape = function(path,name_layer,ID){
             city = readOGR(dsn = paste0("Data/Processed/Maps/",path), layer = name_layer) %>% 
             spTransform(.,CRSobj = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
             slot(slot(city,"polygons")[[1]], "ID") = ID
             row.names(city@data) <- ID
return(city)
}
atributtes = function(df,city_rest,name_rest,date_rest,area_rest,percent_rest){
             df <- mutate(df,city_restric = city_rest, 
                             name_restric = name_rest, 
                             date_restric = date_rest,
                             area_restric = area_rest,
                             percent_restric = percent_rest)  
} 

### Load shape city
barranquilla = read_shape("Barranquilla/Base Maps","barranquilla_habited","1")
soledad = read_shape("Barranquilla/Base Maps","soledad_habited","2")
bogota = read_shape("Bogota/Base Maps","bogota_habited","3")
cartagena = read_shape("Cartagena/Base Maps","cartagena_habited","4")
neiva = read_shape("Neiva/Base Maps","neiva_habited","5")

### Rbind shapes citys
citys <- spRbind(barranquilla,soledad) %>% spRbind(.,bogota) %>% spRbind(.,cartagena) %>% spRbind(.,neiva)

### Load Restrictions
r_baq_male = read_shape("Barranquilla/restriction","restric_man","1") 
r_baq_male@data = atributtes(df = r_baq_male@data,city_rest = "Male passenger - Barranquilla",
                             name_rest = "Male passenger restriction", date_rest = '', 
                             area_rest = '', percent_rest = '')
r_bog_male = read_shape("Bogota/restriction","restric","2")
r_bog_male@data = atributtes(df = r_bog_male@data,city_rest = "Male passenger - Bogotá",
                             name_rest = "Male passenger restriction", date_rest = '', 
                             area_rest = '', percent_rest = '')
r_car_pass = read_shape("Cartagena/restriction","restric","3")
r_car_pass@data = atributtes(df = r_car_pass@data,city_rest = "No passengers - Cartagena",
                             name_rest = "No passengers restriction", date_rest = '', 
                             area_rest = '', percent_rest = '')
r_neiva_pass = read_shape("Neiva/restriction","restric","4")
r_neiva_pass@data = atributtes(df = r_neiva_pass@data,city_rest = "No passengers - Neiva",
                             name_rest = "No passengers restriction", date_rest = '', 
                             area_rest = '', percent_rest = '')
r_sol_moto = read_shape("Barranquilla/Base Maps","soledad_habited","5")
r_sol_moto@data = atributtes(df = r_sol_moto@data,city_rest = "No motorcycles - Soledad",
                             name_rest = "No motorcycles restriction", date_rest = '', 
                             area_rest = '', percent_rest = '')
r_baq_moto = read_shape("Barranquilla/restriction","without_motorcycle_2011",as.character(6:11))
r_baq_moto@data = atributtes(df = r_baq_moto@data,city_rest = "No motorcycles - Barranquilla",
                             name_rest = "No motorcycles restriction", date_rest = '', 
                             area_rest = '', percent_rest = '')
lapply(1:6, function(x) slot(slot(r_baq_moto,"polygons")[[x]], "ID") = as.character(x+6))
row.names(r_baq_moto@data) <- as.character(6:11)

### Rbind shapes restrictions
restric <- spRbind(r_baq_male,r_bog_male) %>% spRbind(.,r_car_pass) %>% 
           spRbind(.,r_neiva_pass) %>% spRbind(.,r_sol_moto) %>% spRbind(.,r_baq_moto)

### Save
maps <- list(citys,restric)
saveRDS(maps,file = 'github-app/data/maps.rds')
