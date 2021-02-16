# Emilio

# Setea los textos del sistema en español
Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")


# Carga de librerías
library(flexdashboard)
library(tidyverse)
library(plotly)
library(zoo) # series de tiempo
library(readr)
library(RcppRoll)
library(viridis) # paleta de colores
library(lubridate) # manejo de fechas
library(DT)
library(broom)
library(sf)
library(ggrepel)

# Carga de data de datos abiertos

minsa_p <-
  data.table::fread("../data/positivos_covid.csv", sep = ";") %>%
  mutate(
    FECHA_RESULTADO = as.Date(as.character(FECHA_RESULTADO), format = "%Y%m%d"),
    # Reconocimiento texto como fecha
    EDAD_n = as.numeric(EDAD),
    DEPARTAMENTO = recode(DEPARTAMENTO, "LIMA REGION" = "LIMA")
    #Junta Lima y Lima pronvincias
  )

# Calculo de positivos por día más calculo de acumulados en cada día.
minsa_p_cum <- minsa_p %>%
  select(FECHA_RESULTADO) %>%
  group_by(FECHA_RESULTADO) %>%
  summarise(positivos = n()) %>%
  mutate(positivos_cum = cumsum(replace_na(positivos, 0)))

# Gráfica
ggplot(minsa_p_cum, aes(x = FECHA_RESULTADO, y = positivos)) +
  geom_line(aes(y = rollmean(positivos, 7, fill = NA)), size = 1.2) + #linea de contagios + calculo de media movil
  # Añadido de fechas verticales
  geom_vline(
    xintercept = as.Date("2020-03-16"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-10"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-19"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-21"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-05"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-08-09"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-08-20"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-01"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-14"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-04-28"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-05-01"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  geom_vline(
    xintercept = as.Date("2020-11-18"),
    linetype = "dashed",
    color = "deepskyblue3",
    size = 0.7
  ) +
  scale_x_date(date_labels = "%b", #Formateo escala fecha
               breaks = "1 month",
               minor_breaks = NULL) +
  theme(legend.position = "bottom"
        , legend.title = element_blank()) +
  theme_classic() + # tema sin lineas de fondo
  labs(x = element_blank(),
       y = element_blank(),
       title = "Contagios positivos de COVID-19 (promedio de 7 días)")