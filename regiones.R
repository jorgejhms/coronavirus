## Regiones

Sys.setlocale(category = "LC_ALL", locale ="es_ES.UTF-8") # setea el sistema en español

library(flexdashboard)
library(ggplot2) # graficos
library(dplyr) # manipulación de data
library(zoo) # series de tiempo
library(readr)
library(tidyr)
library(RcppRoll)
library("viridis") # paleta de colores
library(lubridate) # manejo de fechas
library(DT)
library(broom)
library(sf)
library(grid)

source("funciones.R") #carga funciones de R

minsa_p <- read.csv("data/positivos_covid.csv", sep = ";")
minsa_f <- read.csv("data/fallecidos_covid.csv", sep = ";")

## Limpieza de Data

minsa_p <- minsa_p %>%
  mutate(
    FECHA_RESULTADO = as.Date(as.character(FECHA_RESULTADO), format = "%Y%m%d"),
    EDAD_n = as.numeric(EDAD),
    DEPARTAMENTO = recode(DEPARTAMENTO, "LIMA REGION" = "LIMA") #Juntando Lima y Lima pronvincias
  )

minsa_f <- minsa_f %>%
  mutate(
    FECHA_CORTE = as.Date(as.character(FECHA_CORTE), format = "%Y%m%d"),
    FECHA_FALLECIMIENTO = as.Date(as.character(FECHA_FALLECIMIENTO), format = "%Y%m%d"),
    FECHA_NAC = as.Date(as.character(FECHA_NAC), format = "%Y%m%d"),
  )

minsa_db <- minsa_p %>%
  select(FECHA_RESULTADO, DEPARTAMENTO) %>%
  group_by(FECHA_RESULTADO, DEPARTAMENTO) %>%
  summarise(positivos = n()) %>%
  mutate(positivos_cum = cumsum(replace_na(positivos, 0))) %>%
  rename(fecha = FECHA_RESULTADO)

fallecidos_cum <-
  minsa_f %>%
  select(FECHA_FALLECIMIENTO, DEPARTAMENTO) %>%
  group_by(FECHA_FALLECIMIENTO, DEPARTAMENTO) %>%
  summarise(fallecidos = n()) %>%
  mutate(fallecidos_cum = cumsum(replace_na(fallecidos, 0))) %>%
  rename(fecha = FECHA_FALLECIMIENTO)

minsa_db <- full_join(minsa_db, fallecidos_cum, by = c("fecha", "DEPARTAMENTO") ) %>%
  arrange(fecha)
rm(fallecidos_cum)

## Graficos por región

ggplot (minsa_db, aes(x = fecha, y = positivos)) +
  geom_line(aes(y = rollmean(positivos, 7, fill = NA)))+
  scale_x_date(
    date_labels = "%b",
    breaks = "1 month",
    minor_breaks = NULL,
    limits = c(as.Date("2020-02-28", "%Y-%m-%d"), NA)
  ) +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = " ",
             scientific = FALSE)
  ) +
  theme (
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45)
  ) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Contagios diarios de Coronavirus en Perú") +
  facet_wrap( ~ DEPARTAMENTO)

minsa_f %>%
  select(FECHA_FALLECIMIENTO, DEPARTAMENTO) %>%
  group_by(FECHA_FALLECIMIENTO, DEPARTAMENTO) %>%
  summarise(fallecidos = n()) %>%
ggplot (aes(x = FECHA_FALLECIMIENTO, y = fallecidos)) +
  geom_line(aes(y = rollmean(fallecidos, 7, fill = NA)))+
  scale_x_date(
    date_labels = "%b",
    breaks = "1 month",
    minor_breaks = NULL,
    limits = c(as.Date("2020-02-28", "%Y-%m-%d"), NA)
  ) +
  scale_y_continuous(
    labels = function(x)
      format(x, big.mark = " ",
             scientific = FALSE)
  ) +
  theme (
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45)
  ) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Fallecidos diarios de Coronavirus en Perú") +
  facet_wrap( ~ DEPARTAMENTO)