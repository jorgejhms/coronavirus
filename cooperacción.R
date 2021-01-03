## Paquetes

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
library(ggrepel) # permite agregar valores ultimos en graficos

source("funciones.R") #carga funciones de R

owiddata <- read.csv("data/owid-covid-data.csv")

w_db <- owiddata %>% filter(iso_code == "OWID_WRL")
w_db <- slice(w_db, 1:(n()-1)) #eliminando ultima linea no actualizada

w_casos <- tail(w_db$total_cases, 1)

w_muertos <- tail(w_db$total_deaths, 1)
  
w_p_casos_diarios <- w_db %>%
  ggplot(aes(x = as.Date(date), y = new_cases)) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = rollmean(new_cases, 7, fill = NA), color = "red"), size = 1.5) +
  scale_color_manual(values = "red", labels = "Media móvil 7 días") +
  scale_x_date(date_labels = "%b",
               breaks = "1 month",
               limits = as.Date(c("2020-01-01", "2020-12-31"))) +
  labs (x = "Fecha", y = "Casos") +
  theme (legend.position = "bottom"
         , legend.title = element_blank()) +
  labs(x = element_blank(),
       y = element_blank(),
       title = element_blank())

w_p_casos_cum <- w_db %>%
  ggplot(aes(x = as.Date(date), y = total_cases)) +
  geom_line() +
  scale_x_date(date_labels = "%b",
               breaks = "1 month",
               limits = as.Date(c("2020-01-01", "2020-12-31"))) +
  scale_y_log10() +
  geom_text_repel(
    aes(label = format(total_cases, big.mark = ",", scientific = FALSE)),
    data = tail(w_db, 1),
    fontface ="plain", 
    color = "black", size = 3
  ) +
  labs (x = "Fecha", y = "Casos") +
  theme (legend.position = "bottom"
         , legend.title = element_blank()) +
  labs(x = element_blank(),
       y = element_blank(),
       title = element_blank())