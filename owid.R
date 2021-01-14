## OWID

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

source("funciones.R") #carga funciones de R

owiddata <- read.csv("data/owid-covid-data.csv")

owid_per <- owiddata %>% filter(iso_code == "PER")
owid_sa <- owiddata %>% filter(continent == "South America")

## Gráficos

ggplot (owid_sa, aes(x = as.Date(date, "%Y-%m-%d"), y = new_cases_smoothed)) +
  #geom_line(aes(y = rollmean(new_cases, 7, fill = NA)))+
  geom_line() +
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
    axis.text.x = element_blank()
  ) +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Contagios diarios de Coronavirus en Sudamérica") +
  facet_wrap( ~ location, scales = "free")