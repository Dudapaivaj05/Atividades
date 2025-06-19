#GBIF

#baixar pacotes
install.packages("rgbif")
library(rgbif)
library(dplyr)

# checar funcoes
?occ_data

# baixar ocorrencias
dori_gbif <- occ_data(scientificName = "Natator depressus", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(dori_gbif) #lista ou um objeto complexo (como o resultado de uma função do pacote rgbif
dim(dori_gbif$data) - #quantidade de um data frame (500 linhas e 114 colunas)

 
  #Cada linha é uma ocorrência e variáveis
  #list que contém um conjunto de data frames
  # checar campos
   dori_gbif$data %>% names
gbif_issues()

# checar problemas reportados
issues_gbif <- dori_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  dplyr::filter(code %in% issues_gbif)
names(data.frame(gbif_issues()))

dori_gbif1 <- dori_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locationID)
dori_gbif1 <- dori_gbif1 %>% 
  distinct() 
# checar niveis dos fatores
lapply(dori_gbif1, unique)

library(bdc)
library(CoordinateCleaner)

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = dori_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")
# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- dori_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))
library(ggplot2)
# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()

# pontos no mar
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()

# filtrar todas do dataset suspeito
dori_gbif_noDiveboard <- dori_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))
# filtrar todas do dataset suspeito
dori_gbif_noDiveboard %>% 
  filter(decimalLatitude > 0) %>% 
  arrange(-decimalLatitude) %>% 
  data.frame()
dori_gbif_ok <- dori_gbif_noDiveboard %>% 
  filter(decimalLatitude < 31) 

install.packages("ggmap")
library(ggmap)
library(maps)
install.packages("mapdata")
library(mapdata)

world <- map_data('world')

# checar pontos

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "blue") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))


#OBIS
## OBIS
dori_obis <- robis::occurrence("Natator depressus")
# checar dados
names(dori_obis)
dori_obis1 <- dori_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude,flags, 
                basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locality)%>% 
  distinct()

# check problemas reportados (flags)
dori_obis1 %>% 
  distinct(flags)
# check NA em datasetName
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName))

# freq x tipo de observação
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName)) %>%
  group_by(datasetName, basisOfRecord) %>% 
  reframe(freq = n()) %>%
  ggplot(aes(x = freq, fill = basisOfRecord)) +
  geom_histogram()

# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName))%>%
  lapply(., unique)

# aplicar filtros
world <- map_data('world')
dori_obis <- dori_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName))%>%
  
  # plot
  ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis1, aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))


dori_obis <- dori_obis1 %>% 
  dplyr::filter(decimalLatitude < 0 | decimalLongitude < 100)

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = dori_obis1, aes(x = decimalLongitude, y = decimalLatitude, color = basisOfRecord)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))

# unir GBIF e OBIS

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis))

setdiff(names(dori_obis), names(dori_obis))
library(tidyr)
library(dplyr)
all_data <- bind_rows(dori_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      dori_obis %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  tibble::column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude, basisOfRecord) %>% 
  distinct() %>% 
  tibble::rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Natator depressus") %>% 
  dplyr::select(-rn)


# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Natator depressus")))

write.csv(all_data, "C:/Users/dudap/OneDrive/Disciplina César/atividade 3.csv", row.names = FALSE)
