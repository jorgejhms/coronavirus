###===Casos Amazónicos===###
#Obtención de datos de contagios y fallecidos por el COVID en las zonas amazónicas.

###===Paquetes===###
library(ggplot2) #graficos
library(dplyr) #manipulación de data 
library(zoo) #series de tiempo
library(readr)
library(tidyr)
library(RcppRoll)

source("funciones.R")

###===Importación y limpieza de data===###
positivos <- read.csv("data/positivos_covid.csv", fileEncoding = "latin1")
fallecidos <- read.csv("data/fallecidos_covid.csv", fileEncoding = "latin1")
fallecidos_sinadef <- read.csv("data/fallecidos_sinadef.csv", sep =";", fileEncoding = "latin1") #cambio a read.csv para aplicar separador

positivos <- mutate(positivos, 
                    y = substr(FECHA_RESULTADO, 1, 4),
                    m = substr(FECHA_RESULTADO,5,6),
                    d = substr(FECHA_RESULTADO,7,8),
                    fecha=as.Date(paste0(y,"-",m,"-",d)),
                    EDAD_n = as.numeric(EDAD),
)

fallecidos <- mutate(fallecidos, 
                     y = substr(FECHA_FALLECIMIENTO, 1, 4),
                     m = substr(FECHA_FALLECIMIENTO,5,6),
                     d = substr(FECHA_FALLECIMIENTO,7,8),
                     fecha=as.Date(paste0(y,"-",m,"-",d)),
                     edad_declarada = as.numeric(EDAD_DECLARADA),
                     yn = substr(FECHA_NAC, 1, 4),
                     mn = substr(FECHA_NAC, 5, 6),
                     dn = substr(FECHA_NAC, 7, 8)
)

#Filtros de data amazónica

p.LORETO <- filter(positivos, DEPARTAMENTO == "LORETO")
p.UCAYALI <- filter(positivos, DEPARTAMENTO == "UCAYALI")
p.MADRE_DE_DIOS <- filter(positivos, DEPARTAMENTO == "MADRE DE DIOS")
p.AMAZONAS <- filter (positivos, DEPARTAMENTO == "AMAZONAS" & PROVINCIA == c("BAGUA", "CONDORCANQUI"))
p.SAN_MARTIN <- filter (positivos, DEPARTAMENTO == "SAN MARTIN")
p.HUANUCO <- filter (positivos, DEPARTAMENTO == "HUANUCO" & PROVINCIA == c("LEONCIO PRADO", "PACHITEA", "PUERTO INCA"))
p.PASCO <- filter(positivos, DEPARTAMENTO == "PASCO" & PROVINCIA == "OXAPAMPA")
p.JUNIN <- filter(positivos, DEPARTAMENTO == "JUNIN" & PROVINCIA == c("CHANCHAMAYO", "SATIPO"))
p.CUSCO <- filter(positivos, DEPARTAMENTO == "CUSCO" & PROVINCIA == "LA CONVENCION")
p.PUNO <- filter(positivos, DEPARTAMENTO == "PUNO" & PROVINCIA == c("SANDIA", "CARABAYA"))

p.amazonico <- filter(positivos, (DEPARTAMENTO == "LORETO") | (DEPARTAMENTO == "UCAYALI") | (DEPARTAMENTO == "AMAZONAS" & PROVINCIA == c("BAGUA", "CONDORCANQUI")) | (DEPARTAMENTO == "SAN MARTIN") | (DEPARTAMENTO == "HUANUCO" & PROVINCIA == c("LEONCIO PRADO", "PACHITEA", "PUERTO INCA")) | (DEPARTAMENTO == "PASCO" & PROVINCIA == "OXAPAMPA") | (DEPARTAMENTO == "JUNIN" & PROVINCIA == c("CHANCHAMAYO", "SATIPO")) | (DEPARTAMENTO == "CUSCO" & PROVINCIA == "LA CONVENCION") | (DEPARTAMENTO == "PUNO" & PROVINCIA == c("SANDIA", "CARABAYA")))