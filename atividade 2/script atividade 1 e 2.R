#Disciplina César
## packages ----
install.packages("vegan")
install.packages("robis")
install.packages("rgbif")
install.packages("bdc")
install.packages("CoordinateCleaner")
install.packages("validate")
install.packages("taxadb")
install.packages("taxize")

library(tidyverse)
library(vegan)
library(robis)
library(rgbif)
library(bdc)
library(CoordinateCleaner)
library(validate)
library(taxadb)
library(dplyr)
library(taxize)
#Aula 1
data.frame(
  participante = seq(1:10),
  fichas = c(read.csv("data/iris_mod.csv", header = T) %>% 
               distinct(amostra) %>% 
               pull()%>% 
               sample()),
  n = 'amostras'
) %>% 
  pivot_wider(
    names_from = "n", 
    values_from = "fichas"
  ) %>% 
  knitr::kable()

#Aula 2
#Checando os dados
setwd("C:/Users/dudap/OneDrive/Disciplina César")
iris <- read.csv("arquivo_unico_LUCIANA - arquivo_unico_LUCIANA.csv", header = T)
lapply(iris, unique)
str(iris)
iris %>% 
  select(especie, sepal_lenght_cm:petal_width_cm) %>% 
  pivot_longer(cols = -especie, names_to = "variavel", values_to = "valores") %>% 
  ggplot(aes(x = valores, fill = especie)) +
  geom_histogram() +
  facet_wrap(~ variavel, scales = 'free_x') +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "tamanho (mm)") +
  scale_fill_discrete(
    expression(bold("especie:")),
    labels = c(expression(italic("Iris setosa")), 
               expression(italic("Iris versicolor")), 
               expression(italic("Iris virginica"))))

rules <- validator(in_range(lat, min = -90, max = 90),
                   in_range(lat, min = -180, max = 180),
                   is.numeric(site),
                   is.numeric(data),
                   all_complete(iris))

out   <- confront(iris, rules)
summary(out)
summary(iris)
plot(out)

#Taxóns
# check taxa
especie <- iris %>% 
  distinct(especie) %>% 
  pull() %>% 
  c("Iris setosa",. ) %>% # inserimos uma espécie fictícia para teste
  taxadb::filter_name(., provider = "itis") %>% 
  data.frame() %>% 
  bind_cols(especie = iris %>% 
              distinct(especie) %>% 
              pull())
exists("filter_name")

#Manipulando os dados
#Planilha base
iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, data, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, data, amostra, sep = "_")) %>% 
  left_join(especie %>% 
              select(especie, acceptedNameUsageID, scientificName)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = longitude, # rename fields according to DwC 
                decimalLatitude = latitude,
                eventDate = data) %>% 
  mutate(geodeticDatum = "WGS84", # and add complimentary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

#Planilhas do eMOF
#Planilha de eventos
## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 

#Planilha de ocorrências
## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, acceptedNameUsageID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 

#Planilha de atributos (eMOF)
## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, sepal_lenght_cm:petal_width_cm) %>%  
  pivot_longer(cols = sepal_lenght_cm:petal_width_cm,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("sepal_lenght_cm", "sepal_width_cm", "petal_width_cm", "petal_length_cm"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))
#Controle de qualidade
# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)
# check NA values
eMOF %>%
  filter(is.na(eventID))
occurrences %>%
  filter(is.na(eventID))
# Escrevendo as matrizes como arquivos de texto
rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}
